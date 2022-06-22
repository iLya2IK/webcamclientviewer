unit webcammain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ValEdit,
  ComCtrls, StdCtrls, Grids, JSONPropStorage, Buttons, ExtDlgs, libpascurl,
  extmemorystream, fpjson, jsonparser, ECommonObjs;

type

  THTTP2BackgroundTask = class;
  THTTP2BackgroundTasks = class;

  TOnHTTP2Finish = procedure (aTask : THTTP2BackgroundTask) of object;

  { THTTP2SettingsIntf }

  THTTP2SettingsIntf = class(TThreadSafeObject)
  private
    FSID, FHost, FDevice : String;
    function GetDevice : String;
    function GetHost : String;
    function GetSID : String;
    procedure SetDevice(AValue : String);
    procedure SetHost(AValue : String);
    procedure SetSID(AValue : String);
  public
    property SID : String read GetSID write SetSID;
    property Host : String read GetHost write SetHost;
    property Device : String read GetDevice write SetDevice;
  end;

  { THTTP2BackgroundTask }

  THTTP2BackgroundTask = class(TThreadSafeObject)
  private
    FErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
    FPath : String;
    FOnSuccess : TOnHTTP2Finish;
    FResponseCode : Longint;
    FErrorCode, FErrorSubCode : Longint;
    FPool : THTTP2BackgroundTasks;
    FCURL : CURL;
    FResponse : TMemoryStream;
    FRequest : TCustomMemoryStream;
    FOnFinish : TOnHTTP2Finish;
    FIsSilent : Boolean;
    headers : pcurl_slist;
    FState : Byte;
    FSettings : THTTP2SettingsIntf;
    FData : TObject;
    function GetErrorStr : String;
    function GetState : Byte;
    procedure SetState(aState : Byte);
    procedure Finalize;
    procedure AttachToPool;
    procedure DoError(err : Integer); overload;
    procedure DoError(err, subcode : Integer); overload;
  public
    constructor Create(aPool : THTTP2BackgroundTasks; Settings : THTTP2SettingsIntf;
      aIsSilent : Boolean);
    destructor Destroy; override;
    function doPost(const aPath : String; aContent : Pointer;
      aContentSize : Int64; stack : Boolean) : Boolean;
    function Seek(offset: curl_off_t; origin: LongInt) : LongInt; virtual;
    function Write(ptr: Pointer; size: LongWord; nmemb: LongWord) : LongWord; virtual;
    function Read(ptr: Pointer; size: LongWord; nmemb: LongWord) : LongWord; virtual;

    procedure DoIdle; virtual;

    procedure Terminate;
    procedure Close; virtual;
    function Finished : Boolean;

    property OnFinish : TOnHTTP2Finish read FOnFinish write FOnFinish;
    property OnSuccess : TOnHTTP2Finish read FOnSuccess write FOnSuccess;

    property Path : String read FPath;
    property State : Byte read GetState write SetState;
    property IsSilent : Boolean read FIsSilent;
    property ResponseCode : Longint read FResponseCode;
    property ErrorCode : Longint read FErrorCode;
    property ErrorSubCode : Longint read FErrorSubCode;
    property ErrorString : String read GetErrorStr;

    property Request : TCustomMemoryStream read FRequest;
    property Response : TMemoryStream read FResponse;

    property Data : TObject read FData write FData;
  end;

  THTTP2BackgroundOutStreamTask = class;
  THTTP2BackgroundInStreamTask = class;

  TOnGetNextFrame = function (aTsk : THTTP2BackgroundOutStreamTask) : Integer of object;
  TOnHasNextFrame = procedure (aTsk : THTTP2BackgroundInStreamTask) of object;

  TMemSeq = specialize TThreadSafeFastBaseSeq<TCustomMemoryStream>;

  { THTTP2BackgroundOutStreamTask }

  THTTP2BackgroundOutStreamTask = class(THTTP2BackgroundTask)
  private
    FInc : integer;
    FFrames : TMemSeq;
    FOnGetNextFrame : TOnGetNextFrame;
  protected
    procedure PopNextFrame;
  public
    constructor Create(aPool : THTTP2BackgroundTasks; Settings : THTTP2SettingsIntf;
      aIsSilent : Boolean);
    destructor Destroy; override;

    function  Read(ptr: Pointer; size: LongWord; nmemb: LongWord) : LongWord; override;
    procedure LaunchStream(aOnGetNextFrame: TOnGetNextFrame);
    procedure PushFrame(aFrame : TCustomMemoryStream);

    procedure DoIdle; override;
  end;

  TWCRESTWebCamFrameState = (fstWaitingStartOfFrame, fstWaitingData);

  { THTTP2BackgroundInStreamTask }

  THTTP2BackgroundInStreamTask = class(THTTP2BackgroundTask)
  private
    FFrameBuffer : TExtMemoryStream;
    FFrameState  : TWCRESTWebCamFrameState;
    FFrameSize   : Cardinal;
    FFrameBufferSize : Cardinal;
    FFrameID : Integer;

    FFrames : TMemSeq;
    FOnHasNextFrame : TOnHasNextFrame;
  protected
    procedure PushFrame(aStartAt : Int64);
    function TryConsumeFrame(Chunk : Pointer; ChunkSz : Integer) : integer;
  public
    constructor Create(aPool : THTTP2BackgroundTasks; Settings : THTTP2SettingsIntf;
      aIsSilent : Boolean);
    destructor Destroy; override;

    function Write(ptr: Pointer; size: LongWord; nmemb: LongWord) : LongWord; override;
    procedure LaunchStream(const aDevice : String; aOnHasNextFrame : TOnHasNextFrame
      );

    function PopFrame : TCustomMemoryStream;

    procedure Close; override;

    procedure DoIdle; override;
  end;

  THTTP2StreamsTasks = specialize TThreadSafeFastBaseSeq<THTTP2BackgroundTask>;

  { TThreadsafeCURLM }

  TThreadsafeCURLM = class(TThreadSafeObject)
  private
    FValue : CURLM;
    function getValue : CURLM;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitCURLM;
    procedure DoneCURLM;
    property Value : CURLM read getValue;
  end;

  THTTP2BackgroundTasksProto = class (specialize TThreadSafeFastBaseSeq<THTTP2BackgroundTask>);

  { THTTP2BackgroundTasks }

  THTTP2BackgroundTasks = class (THTTP2BackgroundTasksProto)
  private
    FCURLM : TThreadsafeCURLM;
    FOnMultiError : TNotifyEvent;
    procedure IdleTask(aStrm : TObject);
    procedure TerminateTask(aStrm : TObject);
    function IsTaskFinished(aStrm : TObject; {%H-}data : pointer) : Boolean;
    procedure SetTaskFinished(aStrm : TObject; data : pointer);
    procedure SetMultiPollError(aStrm : TObject; data : pointer);
    procedure AfterTaskExtract(aStrm: TObject);
    procedure DoMultiError(code : integer);
    function DoInitMultiPipeling : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property  CURLMv : TThreadsafeCURLM read FCURLM;
    procedure DoIdle;
    procedure Terminate;

    function Ready : Boolean;

    property OnMultiError : TNotifyEvent read FOnMultiError write FOnMultiError;
  end;

  { THTTP2AsyncBackground }

  THTTP2AsyncBackground = class(TThread)
  private
    FTasks : THTTP2BackgroundTasks;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Execute; override;
    procedure AddTask(aTask : THTTP2BackgroundTask);

    function Ready : Boolean;

    property Tasks : THTTP2BackgroundTasks read FTasks;
  end;

  { THTTP2StreamFrame }

  THTTP2StreamFrame = class(TThreadSafeObject)
  private
    FLstFrame : TCustomMemoryStream;
    FFrame : TCustomMemoryStream;
    FFrameID : integer;
    function GetFrame : TCustomMemoryStream;
    function GetFrameID : Integer;
    function GetLstFrame : TCustomMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;

    function ExtractFrame : TCustomMemoryStream;
    procedure NextFrame(aBmp : TCustomMemoryStream);

    property Frame : TCustomMemoryStream read GetFrame;
    property LstFrame : TCustomMemoryStream read GetLstFrame;
    property FrameID : integer read GetFrameID;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    AuthOpts : TValueListEditor;
    AuthToServerBtn : TToolButton;
    DeviceList : TListBox;
    DevicesAU : TCheckBox;
    Image1 : TImage;
    ImageList1 : TImageList;
    AppConfig : TJSONPropStorage;
    Label1 : TLabel;
    Label2 : TLabel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    RecordLabel : TLabel;
    LogMemo : TMemo;
    MsgContent : TValueListEditor;
    MsgHeader : TValueListEditor;
    MsgsAU : TCheckBox;
    MsgsGrid : TStringGrid;
    Panel1 : TPanel;
    Panel10 : TPanel;
    Panel11 : TPanel;
    Panel2 : TPanel;
    Panel3 : TPanel;
    Panel4 : TPanel;
    Panel5 : TPanel;
    Panel6 : TPanel;
    Panel7 : TPanel;
    Panel8 : TPanel;
    Panel9 : TPanel;
    RecordsGrid : TStringGrid;
    RecsAU : TCheckBox;
    SendMsgBtn : TSpeedButton;
    Splitter1 : TSplitter;
    Splitter2 : TSplitter;
    Splitter3 : TSplitter;
    Splitter4 : TSplitter;
    Splitter5 : TSplitter;
    Timer1 : TTimer;
    LongTimer : TTimer;
    TaskTimer : TTimer;
    ToolBar1 : TToolBar;
    DevicesToolbar : TToolBar;
    RecordsToolbar : TToolBar;
    MsgsToolbar : TToolBar;
    ToolButton1 : TToolButton;
    ConfigClientBtn : TToolButton;
    RefreshDevicesBtn : TToolButton;
    DisconnectBtn : TToolButton;
    CopyDeviceBtn : TToolButton;
    RefreshRecordsBtn : TToolButton;
    RefreshMsgsBtn : TToolButton;
    DeleteSelectedBtn : TToolButton;
    DelSelAndOlderBtn : TToolButton;
    ToolButton2: TToolButton;
    LaunchStreamBtn : TToolButton;
    LaunchInStrm : TToolButton;
    procedure AuthOptsEditingDone(Sender : TObject);
    procedure DeleteSelectedBtnClick(Sender : TObject);
    procedure DelSelAndOlderBtnClick(Sender : TObject);
    procedure DisconnectBtnClick(Sender : TObject);
    procedure LaunchInStrmClick(Sender : TObject);
    procedure LaunchStreamBtnClick(Sender : TObject);
    procedure SendMsgBtnClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure LongTimerTimer(Sender : TObject);
    procedure AuthToServerBtnClick(Sender : TObject);
    procedure RecordsGridDblClick(Sender : TObject);
    procedure TaskTimerTimer(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
    procedure ToolButton1Click(Sender : TObject);
    procedure ConfigClientBtnClick(Sender : TObject);
    procedure RefreshDevicesBtnClick(Sender : TObject);
    procedure CopyDeviceBtnClick(Sender : TObject);
    procedure RefreshRecordsBtnClick(Sender : TObject);
    procedure RefreshMsgsBtnClick(Sender : TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    FSynchroFinishedTasks : THTTP2BackgroundTasksProto;
    FTaskPool : THTTP2AsyncBackground;
    FSetts : THTTP2SettingsIntf;

    FFrame : THTTP2StreamFrame;
    FUpdates : THTTP2StreamsTasks;

    function DoInitMultiPipeling : Boolean;

    procedure SuccessAuth(ATask : THTTP2BackgroundTask);
    procedure SuccessDeleteRecords(ATask : THTTP2BackgroundTask);
    procedure SuccessGetConfig(ATask : THTTP2BackgroundTask);

    procedure SetConnected(AValue : Boolean);
    procedure SuccessRequestRecord(ATask : THTTP2BackgroundTask);
    procedure SuccessSaveAsSnapshot(ATask : THTTP2BackgroundTask);
    procedure SuccessSendMsg(ATask : THTTP2BackgroundTask);
    procedure SuccessUpdateDevices(ATask : THTTP2BackgroundTask);
    procedure SuccessUpdateMsgs(ATask : THTTP2BackgroundTask);
    procedure SuccessUpdateRecords(ATask : THTTP2BackgroundTask);
    procedure TaskFinished(ATask : THTTP2BackgroundTask);
    procedure SynchroFinishTasks;
    procedure SynchroUpdateTasks;
    procedure GenNewFrame;
    procedure OnStrmWinClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure OnHasNextFrame(ATask : THTTP2BackgroundInStreamTask);
    procedure SuccessIOStream(ATask : THTTP2BackgroundTask);
    function OnGetNextFrame(ATask : THTTP2BackgroundOutStreamTask) : integer;
    function ConsumeResponseToObj(ATask : THTTP2BackgroundTask) : TJSONObject;
  private
    FConnected,
      NeedToUpdateDevices, NeedToUpdateRecords, NeedToUpdateMsgs,
      FStreaming : Boolean;
    LastMsgsStamp, LastRecsStamp : String;

    function LaunchOutStream : Boolean;
    function LaunchInStream(const aDeviceName : String) : Boolean;
    procedure doPost(const aPath, aContent : String;
      OnSuccess : TOnHTTP2Finish; silent : Boolean = true);
    procedure doPost(const aPath : String; aContent : Pointer;
      aContentSize : Int64; OnSuccess : TOnHTTP2Finish; stack : boolean;
      silent : Boolean = true);
    function GetDevice : String;
    function GetHost : String;
    function GetSID : String;
    procedure SetSID(AValue : String);

    function GetSelectedDeviceName : String;

    procedure Disconnect;

    procedure UpdateDevices;
    procedure UpdateRecords;
    procedure UpdateMsgs;
    procedure DeleteRecords(aIndices : TJSONArray);
    procedure SendMsg(aMsg : TJSONObject);
    procedure RequestRecord(rid : integer);
    procedure DeleteSelected(andOlder : Boolean);
    procedure SaveAsSnapshot(const aFileName : String);

    procedure AddLog(const STR : String);

    property Connected : Boolean read FConnected write SetConnected;
  public
    property SID : String read GetSID write SetSID;
    property Host : String read GetHost;
    property Device : String read GetDevice;
  end;

var
  MainForm : TMainForm;

implementation

uses LazUTF8, Clipbrd, configdlg, streamwin;

const STATE_INIT     = 0;
      STATE_FINISHED = 1;
      STATE_TERMINATED = 2;

      TASK_NO_ERROR = 0;
      TASK_ERROR_ATTACH_REQ = $10;
      TASK_ERROR_CANT_EASY_CURL = $11;
      TASK_ERROR_GET_INFO = $12;
      TASK_ERROR_CURL = $13;

      ERR_WEBCAM_STREAM_BUFFER_OVERFLOW = $21;
      ERR_WEBCAM_STREAM_FRAME_TO_BIG = $22;
      ERR_WEBCAM_STREAM_WRONG_HEADER = $23;


const WEBCAM_FRAME_START_SEQ : Word = $aaaa;
      WEBCAM_FRAME_HEADER_SIZE  = Sizeof(Word) + Sizeof(Cardinal);
      WEBCAM_FRAME_BUFFER_SIZE  = $200000;

const cBAD = 'BAD';

      cMSG       = 'msg';
      cMSGS      = 'msgs';
      cRECORDS   = 'records';
      cRESULT    = 'result';
      cNAME      = 'name';
      cPASS      = 'pass';
      cSHASH     = 'shash';
      cMETA      = 'meta';
      cREC       = 'record';
      cSTAMP     = 'stamp';
      cRID       = 'rid';
      cMID       = 'mid';
      cSYNC      = 'sync';
      cDEVICE    = 'device';
      cDEVICES   = 'devices';
      cTARGET    = 'target';
      cPARAMS    = 'params';
      cCODE      = 'code';
      cCONFIG    = 'config';

const RESPONSE_ERRORS : Array [0..15] of String = (
                          'NO_ERROR',
                          'UNSPECIFIED',
                          'INTERNAL_UNKNOWN_ERROR',
                          'DATABASE_FAIL',
                          'JSON_PARSER_FAIL',
                          'JSON_FAIL',
                          'NO_SUCH_SESSION',
                          'NO_SUCH_USER',
                          'NO_DEVICES_ONLINE',
                          'NO_SUCH_RECORD',
                          'NO_DATA_RETURNED',
                          'EMPTY_REQUEST',
                          'MALFORMED_REQUEST',
                          'NO_CHANNEL',
                          'ERRORED_STREAM',
                          'NO_SUCH_DEVICE');

{$R *.lfm}

function WriteFunctionCallback(ptr: Pointer; size: LongWord;
  nmemb: LongWord; data: Pointer): LongWord; cdecl;
begin
  Result := THTTP2BackgroundTask(data).Write(ptr, size, nmemb);
end;

function SeekFunctionCallback(ptr: Pointer; offset: curl_off_t;
  origin: LongInt): LongInt; cdecl;
begin
  Result := THTTP2BackgroundTask(ptr).Seek(offset, origin);
end;

function ReadFunctionCallback(ptr: Pointer; size: LongWord;
  nmemb: LongWord; data: Pointer): LongWord; cdecl;
begin
  Result := THTTP2BackgroundTask(data).Read(ptr, size, nmemb);
end;

function HTTPEncode(const AStr: String): String;

const
  HTTPAllowed = ['A'..'Z','a'..'z',
                 '*','@','.','_','-',
                 '0'..'9',
                 '$','!','''','(',')'];

var
  SS,S,R: PChar;
  H : String[2];
  L : Integer;

begin
  L:=Length(AStr);
  Result:='';
  if (L=0) then exit;

  SetLength(Result,L*3); // Worst case scenario
  R:=PChar(Result);
  S:=PChar(AStr);
  SS:=S; // Avoid #0 limit !!
  while ((S-SS)<L) do
    begin
    if S^ in HTTPAllowed then
      R^:=S^
    else if (S^=' ') then
      R^:='+'
    else
      begin
      R^:='%';
      H:=HexStr(Ord(S^),2);
      Inc(R);
      R^:=H[1];
      Inc(R);
      R^:=H[2];
      end;
    Inc(R);
    Inc(S);
    end;
  SetLength(Result,R-PChar(Result));
end;

{ TThreadsafeCURLM }

function TThreadsafeCURLM.getValue : CURLM;
begin
  Lock;
  try
    Result := FValue;
  finally
    UnLock;
  end;
end;

constructor TThreadsafeCURLM.Create;
begin
  inherited Create;
  FValue := nil;
end;

destructor TThreadsafeCURLM.Destroy;
begin
  DoneCURLM;
  inherited Destroy;
end;

procedure TThreadsafeCURLM.InitCURLM;
begin
  Lock;
  try
    FValue := curl_multi_init();
    if Assigned(FValue) then
      curl_multi_setopt(FValue, CURLMOPT_PIPELINING, CURLPIPE_MULTIPLEX);
  finally
    UnLock;
  end;
end;

procedure TThreadsafeCURLM.DoneCURLM;
begin
  Lock;
  try
    if assigned(FValue) then
      curl_multi_cleanup(FValue);
    FValue := nil;
  finally
    UnLock;
  end;
end;

{ THTTP2BackgroundInStreamTask }

procedure THTTP2BackgroundInStreamTask.PushFrame(aStartAt : Int64);
var aFrame : TMemoryStream;
begin
  Lock;
  try
    Inc(FFrameID);
    aFrame := TMemoryStream.Create;
    FFrameBuffer.Position := aStartAt;
    aFrame.CopyFrom(FFrameBuffer, FFrameSize + WEBCAM_FRAME_HEADER_SIZE);
    aFrame.Position := 0;
    FFrames.Push_back(aFrame);
  finally
    UnLock;
  end;
end;

function THTTP2BackgroundInStreamTask.TryConsumeFrame(Chunk : Pointer;
  ChunkSz : Integer) : integer;
var BP : Int64;

procedure TruncateFrameBuffer;
begin
  if (BP > 0) then
  begin
    if ((FFrameBufferSize - BP) > 0) then
    begin
      FFrameBufferSize := FFrameBufferSize - BP;
      Move(Pointer(FFrameBuffer.Memory + BP)^,
           FFrameBuffer.Memory^, FFrameBufferSize);
    end else
      FFrameBufferSize := 0;
    BP := 0;
  end;
end;

function BufferFreeSize : Integer;
begin
  Result := WEBCAM_FRAME_BUFFER_SIZE - FFrameBufferSize;
end;

var W : Word;
    C : Cardinal;
    P : Int64;

    ChunkPos : Integer;
begin
  BP := 0;
  Lock;
  try
    ChunkPos := 0;
    while true do
    begin
      if BufferFreeSize = 0 then
      begin
        DoError(ERR_WEBCAM_STREAM_BUFFER_OVERFLOW);
        Exit;
      end;

      if ChunkPos < ChunkSz then
      begin
        FFrameBuffer.Position := FFrameBufferSize;
        P := ChunkSz - ChunkPos;
        if P > BufferFreeSize then P := BufferFreeSize;
        FFrameBuffer.Write(Pointer(Chunk+ChunkPos)^, P);
        Inc(ChunkPos, P);
        FFrameBufferSize := FFrameBuffer.Position;
      end;

      FFrameBuffer.Position := BP;
      case FFrameState of
        fstWaitingStartOfFrame:
        begin
          FFrameSize := 0;
          if (FFrameBufferSize - BP) >= WEBCAM_FRAME_HEADER_SIZE then
          begin
            FFrameBuffer.Read(W, SizeOf(Word));
            if W = WEBCAM_FRAME_START_SEQ then
            begin
              FFrameBuffer.Read(C, SizeOf(Cardinal));
              if C > (WEBCAM_FRAME_BUFFER_SIZE - WEBCAM_FRAME_HEADER_SIZE) then
              begin
                DoError(ERR_WEBCAM_STREAM_FRAME_TO_BIG);
                Exit;
              end else
              begin
                FFrameSize := C;
                FFrameState := fstWaitingData;
              end;
            end else
            begin
              DoError(ERR_WEBCAM_STREAM_WRONG_HEADER);
              Exit;
            end;
          end else
          begin
            TruncateFrameBuffer;
            if ChunkPos = ChunkSz then
              Exit;
          end;
        end;
        fstWaitingData:
        begin
          if (FFrameBufferSize - BP) >= (FFrameSize + WEBCAM_FRAME_HEADER_SIZE) then
          begin
            PushFrame(BP);
            Inc(BP, FFrameSize + WEBCAM_FRAME_HEADER_SIZE);
            FFrameState := fstWaitingStartOfFrame;
          end else
          begin
            TruncateFrameBuffer;
            if ChunkPos = ChunkSz then
              Exit;
          end;
        end;
      end;
    end;
  finally
    UnLock;
    Result := ChunkPos;
  end;
end;

constructor THTTP2BackgroundInStreamTask.Create(aPool : THTTP2BackgroundTasks;
  Settings : THTTP2SettingsIntf; aIsSilent : Boolean);
begin
  inherited Create(aPool, Settings, aIsSilent);

  FreeAndNil(FResponse);

  FFrames := TMemSeq.Create;

  FFrameBuffer := TExtMemoryStream.Create(WEBCAM_FRAME_BUFFER_SIZE);
  FFrameState := fstWaitingStartOfFrame;
  FFrameBufferSize := 0;
  FFrameSize := 0;
  FFrameID := 0;
end;

destructor THTTP2BackgroundInStreamTask.Destroy;
begin
  FFrameBuffer.Free;
  FFrames.Clean;
  FFrames.Free;
  inherited Destroy;
end;

function THTTP2BackgroundInStreamTask.Write(ptr : Pointer; size : LongWord;
  nmemb : LongWord) : LongWord;
begin
  if Finished then Exit(0);

  Result := TryConsumeFrame(ptr, size * nmemb);


  if (FFrames.Count > 0) and Assigned(FOnHasNextFrame) then
    FOnHasNextFrame(Self);
end;

procedure THTTP2BackgroundInStreamTask.LaunchStream(const aDevice : String;
  aOnHasNextFrame : TOnHasNextFrame);
begin
  FOnHasNextFrame := aOnHasNextFrame;
  if FPool.Ready then
  begin
    FCURL := curl_easy_init;
    if Assigned(FCurl) then
    begin
      FPath := '/output.raw?' +cSHASH+'='+HTTPEncode(FSettings.SID) +
                                            '&'+cDEVICE+'='+HTTPEncode(aDevice);
      curl_easy_setopt(FCURL, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2_0);
      curl_easy_setopt(FCURL, CURLOPT_URL, PChar(FSettings.Host + FPath));
      curl_easy_setopt(FCURL, CURLOPT_SSL_VERIFYPEER, Longint(0));
      curl_easy_setopt(FCURL, CURLOPT_SSL_VERIFYHOST, Longint(0));
      curl_easy_setopt(FCURL, CURLOPT_WRITEDATA, Pointer(Self));
      curl_easy_setopt(FCURL, CURLOPT_WRITEFUNCTION, @WriteFunctionCallback);
      curl_easy_setopt(FCURL, CURLOPT_TIMEOUT, Longint(-1));
      curl_easy_setopt(FCURL, CURLOPT_NOSIGNAL, Longint(1));
      headers := nil;
      headers := curl_slist_append(headers, Pchar('content-length: ' + inttostr(0)));
      curl_easy_setopt(FCURL, CURLOPT_HTTPHEADER, headers);
      curl_easy_setopt(FCURL, CURLOPT_PIPEWAIT, Longint(1));

      curl_easy_setopt(FCURL, CURLOPT_ERRORBUFFER, PChar(FErrorBuffer));
      FillChar(FErrorBuffer, CURL_ERROR_SIZE, #0);

      AttachToPool;
    end else
      DoError(TASK_ERROR_CANT_EASY_CURL);
  end;
end;

function THTTP2BackgroundInStreamTask.PopFrame : TCustomMemoryStream;
begin
  Result := FFrames.PopValue;
end;

procedure THTTP2BackgroundInStreamTask.Close;
begin
  Lock;
  try
    Data := nil;
  finally
    UnLock;
  end;
  inherited Close;
end;

procedure THTTP2BackgroundInStreamTask.DoIdle;
begin
  //inherited DoIdle;
  if (FFrames.Count > 0) and Assigned(FOnHasNextFrame) then
    FOnHasNextFrame(Self);
end;

{ THTTP2StreamFrame }

function THTTP2StreamFrame.GetFrame : TCustomMemoryStream;
begin
  Lock;
  try
    Result := FFrame;
  finally
    UnLock;
  end;
end;

function THTTP2StreamFrame.GetFrameID : Integer;
begin
  Lock;
  try
    Result := FFrameID;
  finally
    UnLock;
  end;
end;

function THTTP2StreamFrame.GetLstFrame : TCustomMemoryStream;
begin
  Lock;
  try
    Result := FLstFrame;
  finally
    UnLock;
  end;
end;

constructor THTTP2StreamFrame.Create;
begin
  inherited Create;
  FFrame := nil;
  FLstFrame := nil;
  FFrameID := 0;
end;

destructor THTTP2StreamFrame.Destroy;
begin
  if assigned(FFrame) then FreeAndNil(FFrame);
  if assigned(FLstFrame) then FreeAndNil(FLstFrame);
  inherited Destroy;
end;

function THTTP2StreamFrame.ExtractFrame : TCustomMemoryStream;
begin
  Lock;
  try
    Result := FFrame;
    FFrame := nil;
  finally
    UnLock;
  end;
end;

procedure THTTP2StreamFrame.NextFrame(aBmp : TCustomMemoryStream);
begin
  Lock;
  try
    Inc(FFrameID);
    if assigned(FLstFrame) then FreeAndNil(FLstFrame);
    FLstFrame := TMemoryStream.Create;
    FLstFrame.CopyFrom(aBmp, aBmp.Size);
    aBmp.Position := 0;
    FFrame := aBmp;
  finally
    UnLock;
  end;
end;

{ THTTP2BackgroundOutStreamTask }

procedure THTTP2BackgroundOutStreamTask.PushFrame(aFrame : TCustomMemoryStream);
begin
  if assigned(aFrame) then
   FFrames.Push_back(aFrame);
end;

procedure THTTP2BackgroundOutStreamTask.PopNextFrame;
var Fr : TCustomMemoryStream;
begin
  Fr := FFrames.PopValue;
  if Assigned(Fr) then
  begin
    if Assigned(FRequest) then FRequest.Free;
    FRequest := Fr;
  end;
end;

constructor THTTP2BackgroundOutStreamTask.Create(aPool : THTTP2BackgroundTasks;
  Settings : THTTP2SettingsIntf; aIsSilent : Boolean);
begin
  inherited Create(aPool, Settings, aIsSilent);
  FFrames := TMemSeq.Create;
  FInc := 0;
end;

destructor THTTP2BackgroundOutStreamTask.Destroy;
begin
  FFrames.Free;
  inherited Destroy;
end;

function THTTP2BackgroundOutStreamTask.Read(ptr : Pointer; size : LongWord;
  nmemb : LongWord) : LongWord;

function DoRead : LongWord;
begin
  if assigned(Request) and (Request.Size > 0) then
  begin
    Result := Request.Read(ptr^, nmemb * size);
  end else
    Result := 0;
end;

begin
  if Finished then Exit(CURL_READFUNC_ABORT);

  Lock;
  try
    Result := DoRead;
    if Result = 0 then
    begin
      PopNextFrame;
      Result := DoRead;
    end;
  finally
    UnLock;
  end;

  if Result = 0 then
    Result := CURL_READFUNC_PAUSE;
end;

procedure THTTP2BackgroundOutStreamTask.LaunchStream(aOnGetNextFrame : TOnGetNextFrame);
begin
  FOnGetNextFrame := aOnGetNextFrame;
  if FPool.Ready then
  begin
    FCURL := curl_easy_init;
    if Assigned(FCurl) then
    begin
      FResponse.Position := 0;
      FResponse.Size := 0;
      FPath := '/input.raw?' +cSHASH+'='+HTTPEncode(FSettings.SID);
      curl_easy_setopt(FCURL, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2_0);
      curl_easy_setopt(FCURL, CURLOPT_URL, PChar(FSettings.Host + FPath));
      curl_easy_setopt(FCURL, CURLOPT_SSL_VERIFYPEER, Longint(0));
      curl_easy_setopt(FCURL, CURLOPT_SSL_VERIFYHOST, Longint(0));
      curl_easy_setopt(FCURL, CURLOPT_UPLOAD, Longint(1));
      curl_easy_setopt(FCURL, CURLOPT_WRITEDATA, Pointer(Self));
      curl_easy_setopt(FCURL, CURLOPT_WRITEFUNCTION, @WriteFunctionCallback);
      curl_easy_setopt(FCURL, CURLOPT_SEEKFUNCTION, @SeekFunctionCallback);
      curl_easy_setopt(FCURL, CURLOPT_SEEKDATA, Pointer(Self));
      headers := nil;
      headers := curl_slist_append(headers, Pchar('content-length: ' + inttostr($500000000)));
      curl_easy_setopt(FCURL, CURLOPT_HTTPHEADER, headers);
      curl_easy_setopt(FCURL, CURLOPT_PIPEWAIT, Longint(1));

      curl_easy_setopt(FCURL, CURLOPT_READDATA, Pointer(Self));
      curl_easy_setopt(FCURL, CURLOPT_READFUNCTION,
         @ReadFunctionCallback);
      curl_easy_setopt(FCURL, CURLOPT_INFILESIZE_LARGE, -1);
      curl_easy_setopt(FCURL, CURLOPT_INFILESIZE, -1);
      curl_easy_setopt(FCURL, CURLOPT_NOSIGNAL, Longint(1));

      FRequest := TMemoryStream.Create;
      FRequest.Position := 0;

      curl_easy_setopt(FCURL, CURLOPT_ERRORBUFFER, PChar(FErrorBuffer));
      FillChar(FErrorBuffer, CURL_ERROR_SIZE, #0);

      AttachToPool;
    end else
      DoError(TASK_ERROR_CANT_EASY_CURL);
  end;
end;

procedure THTTP2BackgroundOutStreamTask.DoIdle;
var fr : integer;
begin
  Lock;
  try
    fr := FOnGetNextFrame(Self);
  finally
    UnLock;
  end;
  if fr > 0 then
  begin
    fr := integer(curl_easy_pause(FCURL, CURLPAUSE_CONT));
    if fr <> integer(CURLE_OK) then
      DoError(TASK_ERROR_CURL, fr);
  end;
end;

{ THTTP2AsyncBackground }

constructor THTTP2AsyncBackground.Create;
begin
  inherited Create(true);
  FreeOnTerminate := true;
  FTasks := THTTP2BackgroundTasks.Create;
end;

destructor THTTP2AsyncBackground.Destroy;
begin
  FTasks.Free;
  inherited Destroy;
end;

procedure THTTP2AsyncBackground.Execute;
begin
  while not Terminated do
  begin
    Tasks.DoIdle;
    Sleep(100);
  end;
  Tasks.Terminate;
end;

procedure THTTP2AsyncBackground.AddTask(aTask : THTTP2BackgroundTask);
begin
  FTasks.Push_back(aTask);
end;

function THTTP2AsyncBackground.Ready : Boolean;
begin
  Result := Tasks.Ready;
end;

{ THTTP2SettingsIntf }

function THTTP2SettingsIntf.GetDevice : String;
begin
  Lock;
  try
    Result := FDevice;
  finally
    UnLock;
  end;
end;

function THTTP2SettingsIntf.GetHost : String;
begin
  Lock;
  try
    Result := FHost;
  finally
    UnLock;
  end;
end;

function THTTP2SettingsIntf.GetSID : String;
begin
  Lock;
  try
    Result := FSID;
  finally
    UnLock;
  end;
end;

procedure THTTP2SettingsIntf.SetDevice(AValue : String);
begin
  Lock;
  try
    FDevice := AValue;
  finally
    UnLock;
  end;
end;

procedure THTTP2SettingsIntf.SetHost(AValue : String);
begin
  Lock;
  try
    FHost := AValue;
  finally
    UnLock;
  end;
end;

procedure THTTP2SettingsIntf.SetSID(AValue : String);
begin
  Lock;
  try
    FSID := AValue;
  finally
    UnLock;
  end;
end;

{ THTTP2BackgroundTask }

function THTTP2BackgroundTask.GetState : Byte;
begin
  Lock;
  try
    Result := FState;
  finally
    UnLock;
  end;
end;

function THTTP2BackgroundTask.GetErrorStr : String;
begin
  Result := StrPas(FErrorBuffer);
end;

procedure THTTP2BackgroundTask.SetState(aState : Byte);
begin
  Lock;
  try
    FState := aState;
  finally
    UnLock;
  end;
end;

procedure THTTP2BackgroundTask.Finalize;
begin
  if Assigned(FCURL) then
  begin
    FPool.CURLMv.Lock;
    try
      try
        curl_multi_remove_handle(FPool.CURLMv.FValue, FCURL);
      except
        //do nothing
      end;
    finally
      FPool.CURLMv.UnLock;
    end;
    if Assigned(headers) then
      curl_slist_free_all(headers);
    curl_easy_cleanup(FCURL);
    FCURL := nil;
  end;
end;

procedure THTTP2BackgroundTask.AttachToPool;
begin
  FPool.CURLMv.Lock;
  try
    FErrorSubCode := Integer(curl_multi_add_handle(FPool.CURLMv.FValue, FCURL));
    if FErrorSubCode <> Integer( CURLE_OK ) then
      DoError(TASK_ERROR_ATTACH_REQ, FErrorSubCode);
  finally
    FPool.CURLMv.UnLock;
  end;
end;

procedure THTTP2BackgroundTask.DoError(err : Integer);
begin
  DoError(err, 0);
end;

procedure THTTP2BackgroundTask.DoError(err, subcode : Integer);
begin
  Lock;
  try
    FErrorCode := err;
    FErrorSubCode := subcode;
  finally
    UnLock;
  end;
  State := STATE_FINISHED;
end;

constructor THTTP2BackgroundTask.Create(aPool : THTTP2BackgroundTasks;
  Settings : THTTP2SettingsIntf; aIsSilent : Boolean);
begin
  inherited Create;
  FCURL := nil;
  FPool := aPool;
  FSettings := Settings;
  FErrorCode := TASK_NO_ERROR;
  FErrorSubCode := 0;
  FState := STATE_INIT;
  FIsSilent := aIsSilent;

  FResponse := TMemoryStream.Create;
  FRequest := nil;
end;

destructor THTTP2BackgroundTask.Destroy;
begin
  Finalize;
  if Assigned(FResponse) then  FResponse.Free;
  if Assigned(FRequest) then  FRequest.Free;
  inherited Destroy;
end;

function THTTP2BackgroundTask.doPost(const aPath : String;
  aContent : Pointer; aContentSize : Int64; stack : Boolean) : Boolean;
begin
  Result := false;

  if FPool.Ready then
  begin
    FCURL := curl_easy_init;
    if Assigned(FCurl) then
    begin
      FResponse.Position := 0;
      FResponse.Size := 0;
      FPath := aPath;
      curl_easy_setopt(FCURL, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2_0);
      curl_easy_setopt(FCURL, CURLOPT_URL, PChar(FSettings.Host + FPath));
      curl_easy_setopt(FCURL, CURLOPT_POST, Longint(1));
      curl_easy_setopt(FCURL, CURLOPT_SSL_VERIFYPEER, Longint(0));
      curl_easy_setopt(FCURL, CURLOPT_SSL_VERIFYHOST, Longint(0));
      curl_easy_setopt(FCURL, CURLOPT_WRITEDATA, Pointer(Self));
      curl_easy_setopt(FCURL, CURLOPT_WRITEFUNCTION,
         @WriteFunctionCallback);
      headers := nil;
      headers := curl_slist_append(headers, Pchar('content-length: ' + inttostr(aContentSize)));
      curl_easy_setopt(FCURL, CURLOPT_HTTPHEADER, headers);
      curl_easy_setopt(FCURL, CURLOPT_PIPEWAIT, Longint(1));
      curl_easy_setopt(FCURL, CURLOPT_NOSIGNAL, Longint(1));

      if (aContentSize > 0) then begin
        if stack then
        begin
          FRequest := TMemoryStream.Create;
          FRequest.Write(aContent^,  aContentSize);
          FRequest.Position := 0;
        end else
        begin
          FRequest := TExtMemoryStream.Create;
          TExtMemoryStream(FRequest).SetPtr(aContent,  aContentSize);
        end;
        curl_easy_setopt(FCURL, CURLOPT_READDATA, Pointer(Self));
        curl_easy_setopt(FCURL, CURLOPT_READFUNCTION,
           @ReadFunctionCallback);
        curl_easy_setopt(FCURL, CURLOPT_INFILESIZE, Longint(aContentSize));
      end;
      curl_easy_setopt(FCURL, CURLOPT_ERRORBUFFER, PChar(FErrorBuffer));
      FillChar(FErrorBuffer, CURL_ERROR_SIZE, #0);

      AttachToPool;
    end else
      DoError(TASK_ERROR_CANT_EASY_CURL);
  end;
end;

function THTTP2BackgroundTask.Seek(offset : curl_off_t; origin : LongInt
  ) : LongInt;
var origin_v : TSeekOrigin;
begin
  Lock;
  try
    case origin of
      0 : origin_v := soBeginning;
      1 : origin_v := soCurrent;
      2 : origin_v := soEnd;
    end;
    FRequest.Seek(offset, origin_v);

    Result := CURL_SEEKFUNC_OK;
  finally
    UnLock;
  end;
end;

function THTTP2BackgroundTask.Write(ptr : Pointer; size : LongWord; nmemb : LongWord
  ) : LongWord;
begin
  if Finished then Exit(0);

  Result := FResponse.Write(ptr^, size * nmemb);
end;

function THTTP2BackgroundTask.Read(ptr : Pointer; size : LongWord; nmemb : LongWord
  ) : LongWord;
begin
  if Finished then Exit(CURL_READFUNC_ABORT);

  if assigned(FRequest) then
    Result := FRequest.Read(ptr^, nmemb * size) else
    Result := 0;
end;

procedure THTTP2BackgroundTask.DoIdle;
begin
  //
end;

procedure THTTP2BackgroundTask.Terminate;
begin
  State := STATE_TERMINATED;
end;

procedure THTTP2BackgroundTask.Close;
begin
  Terminate;
end;

function THTTP2BackgroundTask.Finished : Boolean;
begin
  Lock;
  try
    Result := FState >= STATE_FINISHED;
  finally
    UnLock;
  end;
end;

{ THTTP2BackgroundTasks }

function THTTP2BackgroundTasks.IsTaskFinished(aStrm : TObject; {%H-}data : pointer
  ) : Boolean;
begin
  Result := THTTP2BackgroundTask(aStrm).Finished;
end;

procedure THTTP2BackgroundTasks.SetTaskFinished(aStrm : TObject; data : pointer
  );
var rc, sb : integer;
begin
  if THTTP2BackgroundTask(aStrm).FCURL = pCURLMsg_rec(data)^.easy_handle then
  begin
    THTTP2BackgroundTask(aStrm).State := STATE_FINISHED;
    if pCURLMsg_rec(data)^.result <> CURLE_OK then
    begin
      THTTP2BackgroundTask(aStrm).DoError(TASK_ERROR_CURL,
                                          integer(pCURLMsg_rec(data)^.result));
    end else
    begin
      sb := Longint(curl_easy_getinfo(pCURLMsg_rec(data)^.easy_handle,
                                                 CURLINFO_RESPONSE_CODE,
                                                 @rc));
      if sb = Longint(CURLE_OK) then
        THTTP2BackgroundTask(aStrm).FResponseCode := rc
      else
        THTTP2BackgroundTask(aStrm).DoError(TASK_ERROR_GET_INFO, sb);
    end;
  end;
end;

procedure THTTP2BackgroundTasks.SetMultiPollError(aStrm : TObject;
  data : pointer);
begin
  THTTP2BackgroundTask(aStrm).DoError(TASK_ERROR_CURL, pInteger(data)^);
end;

procedure THTTP2BackgroundTasks.IdleTask(aStrm : TObject);
begin
  THTTP2BackgroundTask(aStrm).DoIdle;
end;

procedure THTTP2BackgroundTasks.TerminateTask(aStrm : TObject);
begin
  THTTP2BackgroundTask(aStrm).Terminate;
end;

procedure THTTP2BackgroundTasks.AfterTaskExtract(aStrm : TObject);
begin
  if assigned(THTTP2BackgroundTask(aStrm).OnFinish) then
    THTTP2BackgroundTask(aStrm).OnFinish(THTTP2BackgroundTask(aStrm)) else
    aStrm.Free;
end;

procedure THTTP2BackgroundTasks.DoMultiError(code : integer);
begin
  DoForAllEx(@SetMultiPollError, @code);
  Lock;
  try
    if Assigned(FCURLM) then curl_multi_cleanup(FCURLM);
    FCURLM := nil;
  finally
    UnLock;
  end;
  if assigned(OnMultiError) then
    OnMultiError(Self);
end;

function THTTP2BackgroundTasks.DoInitMultiPipeling : Boolean;
begin
  CURLMv.Lock;
  try
    if Assigned(CURLMv.FValue) then Exit(true);
    CURLMv.InitCURLM;

    Result := Assigned(CURLMv.FValue);
  finally
    CURLMv.UnLock;
  end;
end;

constructor THTTP2BackgroundTasks.Create;
begin
  inherited Create;
  FCURLM := TThreadsafeCURLM.Create;
  FOnMultiError := nil;
end;

destructor THTTP2BackgroundTasks.Destroy;
begin
  FCURLM.Free;
  inherited Destroy;
end;

procedure THTTP2BackgroundTasks.DoIdle;
var response_code, still_running, msgq : integer;
    m : pCURLMsg_rec;
begin
  try
    CURLMv.Lock;
    try
      if Ready then
      begin
        response_code := Integer(curl_multi_perform(CURLMv.FValue, @still_running));

        if (response_code = Integer( CURLE_OK )) then
        begin
         repeat
           m := curl_multi_info_read(CURLMv.FValue, @msgq);
           if (assigned(m) and (m^.msg = CURLMSG_DONE)) then
             DoForAllEx(@SetTaskFinished, m);
         until not Assigned(m);

         if (still_running > 0) then
           response_code := Integer(curl_multi_poll(CURLMv.FValue, [], 0, 200, nil));
        end;
      end else
        response_code := 0;
    finally
      CURLMv.UnLock;
    end;
  except
    response_code := -1;
  end;
  if (response_code <> Integer( CURLE_OK )) then
    DoMultiError(response_code);

  DoForAll(@IdleTask);
  ExtractObjectsByCriteria(@IsTaskFinished, @AfterTaskExtract, nil);
end;

procedure THTTP2BackgroundTasks.Terminate;
begin
  DoForAll(@TerminateTask);
end;

function THTTP2BackgroundTasks.Ready : Boolean;
begin
  Result := Assigned(CURLMv.FValue);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender : TObject);
var
  defs : TStringList;
  i : integer;
begin
  FSynchroFinishedTasks := THTTP2BackgroundTasksProto.Create();
  FTaskPool := THTTP2AsyncBackground.Create();
  FUpdates := THTTP2StreamsTasks.Create;

  FFrame := THTTP2StreamFrame.Create;
  FStreaming := false;

  FSetts := THTTP2SettingsIntf.Create;

  TJSONData.CompressedJSON := true;
  defs := TStringList.Create;
  defs.Add('https://localhost:443');
  defs.Add('');
  defs.Add('');
  defs.Add('user-device');
  defs.Add('');
  curl_global_init(CURL_GLOBAL_ALL);
  for i := 0 to defs.Count-1 do
  begin
    AuthOpts.Cells[1, i]:=AppConfig.ReadString(AuthOpts.Keys[i], defs[i]);
  end;
  FConnected := true;
  Disconnect;
  defs.free;

  AuthOptsEditingDone(nil);

  FTaskPool.Start;
end;

procedure TMainForm.SendMsgBtnClick(Sender : TObject);
var
  aMsg, aParams : TJSONObject;
  i : integer;
  S : String;

procedure StrToVariantPar(const aName, Str : String);
var B : Boolean;
    L : Integer;
    I64 : Int64;
    F : Double;
begin
  if TryStrToInt(Str, L) then
     aParams.Add(aName, L) else
  if TryStrToInt64(Str, I64) then
     aParams.Add(aName, I64) else
  if TryStrToFloat(Str, F) then
     aParams.Add(aName, F) else
  if TryStrToBool(Str, B) then
     aParams.Add(aName, B) else
     aParams.Add(aName, Str);
end;

begin
  aMsg := TJSONObject.Create;
  try
    aMsg.Add(cMSG, MsgHeader.Cells[1, 1]);
    aMsg.Add(cTARGET, MsgHeader.Cells[1, 2]);
    aParams := TJSONObject.Create;
    for i := 0 to MsgContent.RowCount-1 do
    begin
      S := UTF8Trim(MsgContent.Cells[0, i]);
      if Length(S) > 0 then
      begin
        StrToVariantPar(S, MsgContent.Cells[1, i]);
      end;
    end;
    if (aParams.Count > 0) then
      aMsg.Add(cPARAMS, aParams) else
      aParams.Free;
    SendMsg(aMsg);
  finally
    aMsg.Free;
  end;
end;

procedure TMainForm.DisconnectBtnClick(Sender : TObject);
begin
  Disconnect;
end;

procedure TMainForm.LaunchInStrmClick(Sender : TObject);
begin
  if LaunchInStream(GetSelectedDeviceName) then
  begin
    AddLog('In stream launched');
  end else
    Disconnect;
end;

procedure TMainForm.LaunchStreamBtnClick(Sender : TObject);
begin
  if not FStreaming then
  begin
    if LaunchOutStream then
    begin
      AddLog('Out stream launched');
    end else
      Disconnect;
  end else
    AddLog('Out stream already launched');
end;

procedure TMainForm.DeleteSelectedBtnClick(Sender : TObject);
begin
  DeleteSelected(false);
end;

procedure TMainForm.AuthOptsEditingDone(Sender : TObject);
begin
  FSetts.Device := Device;
  FSetts.Host   := Host;
  FSetts.SID    := SID;
end;

procedure TMainForm.DelSelAndOlderBtnClick(Sender : TObject);
begin
  DeleteSelected(true);
end;

procedure TMainForm.FormDestroy(Sender : TObject);
var
  i : integer;
begin
  for i := 0 to AuthOpts.RowCount-1 do
  begin
    AppConfig.WriteString(AuthOpts.Keys[i], AuthOpts.Cells[1, i]);
  end;

  FTaskPool.Terminate;
  FTaskPool.WaitFor;

  curl_global_cleanup();

  Timer1.Enabled := false;
  LongTimer.Enabled := false;
  TaskTimer.Enabled := false;

  FSetts.Free;
  FSynchroFinishedTasks.Free;
  FFrame.Free;
  FUpdates.ExtractAll;
  FUpdates.Free;
end;

procedure TMainForm.LongTimerTimer(Sender : TObject);
begin
  if Connected then
  begin
    if RecsAU.Checked then NeedToUpdateRecords := true;
    if DevicesAU.Checked then NeedToUpdateDevices := true;
    if MsgsAU.Checked then NeedToUpdateMsgs := true;
  end;
end;

procedure TMainForm.SuccessAuth(ATask : THTTP2BackgroundTask);
var
  jObj : TJSONObject;
  jData : TJSONData;
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    jObj := ConsumeResponseToObj(ATask);
    if Assigned(jObj) then
    begin
      if jObj.Find(cSHASH, jData) then
      begin
        SID := jData.AsString;
        Connected := true;
        NeedToUpdateDevices := true;
        NeedToUpdateRecords := true;
        NeedToUpdateMsgs    := true;
        Timer1.Enabled := true;
        LongTimer.Enabled := true;
      end;
      FreeAndNil(jObj);
    end;
  end else
      Disconnect;
end;

procedure TMainForm.AuthToServerBtnClick(Sender : TObject);
var
  jObj : TJSONObject;
  aName, aPass, aStr : String;
begin
  aName := AuthOpts.Cells[1, 1];
  aPass := AuthOpts.Cells[1, 2];

  jObj := TJSONObject.Create([cNAME, aName,
                              cPASS, aPass,
                              cDEVICE, Device]);
  try
    aStr := jObj.AsJSON;
  finally
    jObj.Free;
  end;
  Disconnect;
  doPost('/authorize.json',  aStr, @SuccessAuth, false);
end;

procedure TMainForm.RecordsGridDblClick(Sender : TObject);
var rid, dev, stamp : String;
    rid_col, dev_col, stamp_col, i : integer;
begin
  if (RecordsGrid.Row > 0) then
  begin
    rid_col := -1;
    for i := 0 to RecordsGrid.ColCount-1 do
    begin
      if (SameText(RecordsGrid.Cells[i, 0], cRID)) then
        rid_col := i;
      if (SameText(RecordsGrid.Cells[i, 0], cDEVICE)) then
        dev_col := i;
      if (SameText(RecordsGrid.Cells[i, 0], cSTAMP)) then
        stamp_col := i;
    end;
    if rid_col >= 0 then
    begin
      rid := RecordsGrid.Cells[rid_col, RecordsGrid.Row];
      if dev_col >= 0 then
        dev := RecordsGrid.Cells[dev_col, RecordsGrid.Row]
      else
        dev := '';
      if stamp_col >= 0 then
        stamp := RecordsGrid.Cells[stamp_col, RecordsGrid.Row]
      else
        stamp := '';

      RecordLabel.Caption := Format('record id: %s'#10+
                          'device src: %s'#10+
                          'record stamp: %s', [rid, dev, stamp]);
      RequestRecord(StrToInt(rid));
    end;
  end;
end;

procedure TMainForm.TaskTimerTimer(Sender : TObject);
begin
  SynchroUpdateTasks;
  SynchroFinishTasks;
end;

procedure TMainForm.Timer1Timer(Sender : TObject);
begin
  if Connected then
  begin
    if NeedToUpdateDevices then
      UpdateDevices else
    if NeedToUpdateRecords then
      UpdateRecords else
    if NeedToUpdateMsgs then
      UpdateMsgs;

    if FStreaming then
      GenNewFrame;
  end;
end;

procedure TMainForm.ToolButton1Click(Sender : TObject);
var msg : TJSONObject;
begin
  msg := TJSONObject.Create([cMSG, cSYNC]);
  try
    SendMsg(msg);
  finally
    msg.Free;
  end;
end;

procedure TMainForm.SuccessGetConfig(ATask : THTTP2BackgroundTask);
var
  jObj, jRes : TJSONObject;
  jArr : TJSONArray;
  aStr : String;
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    jObj := ConsumeResponseToObj(ATask);
    if Assigned(jObj) then
    begin
      try
        if jObj.Find(cCONFIG, jArr) and ConfDlg.Execute(jArr) then
        begin
          jRes := TJSONObject.Create([cSHASH, SID,
                                      cCONFIG, ConfDlg.ResultConf]);
          try
            aStr := jRes.AsJSON;
          finally
            jRes.Free;
          end;
          doPost('/setConfig.json',aStr, nil);
        end;
      finally
        jObj.Free;
      end;
    end;
  end else
    Disconnect;
end;

procedure TMainForm.ConfigClientBtnClick(Sender : TObject);
var
  jObj : TJSONObject;
  aStr : String;
begin
  jObj := TJSONObject.Create([cSHASH, SID]);
  try
    aStr := jObj.AsJSON;
  finally
    jObj.Free;
  end;
  doPost('/getConfig.json',aStr, @SuccessGetConfig);
end;

procedure TMainForm.RefreshDevicesBtnClick(Sender : TObject);
begin
  UpdateDevices;
end;

procedure TMainForm.CopyDeviceBtnClick(Sender : TObject);
begin
  Clipboard.AsText := GetSelectedDeviceName;
end;

procedure TMainForm.RefreshRecordsBtnClick(Sender : TObject);
begin
  UpdateRecords;
end;

procedure TMainForm.RefreshMsgsBtnClick(Sender : TObject);
begin
  UpdateMsgs;
end;

procedure TMainForm.ToolButton2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    SaveAsSnapshot(OpenPictureDialog1.FileName);
  end;
end;

procedure TMainForm.SetConnected(AValue : Boolean);

procedure SetEnabled(TB : TToolBar; en : Boolean);
var i :integer;
begin
  TB.Enabled := en;
  for i := 0 to TB.ControlCount-1 do
  begin
    TB.Controls[i].Enabled := en;
  end;
end;

begin
  if FConnected = AValue then Exit;
  FConnected := AValue;

  SetEnabled(MsgsToolbar, AValue);
  SetEnabled(RecordsToolbar, AValue);
  SetEnabled(DevicesToolbar, AValue);
  SendMsgBtn.Enabled := AValue;
  DisconnectBtn.Enabled := AValue;
  ConfigClientBtn.Enabled := AValue;
end;

procedure TMainForm.TaskFinished(ATask : THTTP2BackgroundTask);
begin
  FSynchroFinishedTasks.Push_back(ATask);
end;

procedure TMainForm.SynchroFinishTasks;
var
  Tsk : THTTP2BackgroundTask;
begin
  while true do
  begin
    Tsk := FSynchroFinishedTasks.PopValue;
    if assigned(Tsk) then
    begin
      try
        if Length(Tsk.ErrorString) > 0 then
          AddLog(Tsk.ErrorString);

        case Tsk.ErrorCode of
        Integer( CURLE_OK) :
          if (not Tsk.IsSilent) or (Tsk.ResponseCode <> 200) then
            AddLog(Format('HTTP2 "%s". Code - %d', [Tsk.Path, Tsk.ResponseCode]));
        TASK_ERROR_ATTACH_REQ :
          AddLog(Format('Cant attach easy req to multi. Code - %d',
                              [Tsk.ErrorSubCode]));
        TASK_ERROR_CANT_EASY_CURL :
          AddLog(Format('Cant create easy req. Code - %d', [Tsk.ErrorSubCode]));
        else
          AddLog(Format('HTTP2 "%s" FAIL. Code - %d. Subcode - %d', [Tsk.Path,
                                              Tsk.ErrorCode, Tsk.ErrorSubCode]));
        end;

        if Length(Tsk.ErrorString) > 0 then
          AddLog(Tsk.ErrorString);

        if Assigned(Tsk.OnSuccess) then
          Tsk.OnSuccess(Tsk);

      finally
        Tsk.Free;
      end;
    end else
      Break;
  end;
end;

procedure TMainForm.SynchroUpdateTasks;
var
  Tsk : THTTP2BackgroundTask;
begin
  while true do
  begin
    Tsk := FUpdates.PopValue;
    if assigned(Tsk) then
    begin
      Tsk.Lock;
      try
        if Assigned( Tsk.Data ) then
        begin
          if Tsk is THTTP2BackgroundInStreamTask then
            TStreamFrm(Tsk.Data).UpdateFrame(THTTP2BackgroundInStreamTask(Tsk).PopFrame)
          else
          if Tsk is THTTP2BackgroundOutStreamTask then
          begin
            FFrame.Lock;
            try
              TStreamFrm(Tsk.Data).UpdateFrame(FFrame.LstFrame, false);
            finally
              FFrame.UnLock;
            end;
          end;
        end;
      finally
        Tsk.UnLock;
      end;
    end else
      Break;
  end;
end;

function TMainForm.DoInitMultiPipeling : Boolean;
begin
  Result := FTaskPool.Tasks.DoInitMultiPipeling;
  if Result then
    TaskTimer.Enabled := true;
end;

procedure TMainForm.GenNewFrame;
var pic : TJPEGImage;
    fmem : TCustomMemoryStream;
    sz : Cardinal;
    p : integer;
    S : String;
begin
  pic := TJPEGImage.Create;
  try
    pic.Width := 320;
    pic.Height := 320;
    with pic.Canvas do
    begin
      p := (FFrame.FrameID mod 96);
      case p of
        0..15   : Brush.Color := ($FF * p div 16);
        16..31  : Brush.Color := (($FF * (p-16) div 16) shl 8) or $FF;
        32..47  : Brush.Color := (($FF * (47-p) div 16)) or $FF00;
        48..63  : Brush.Color := (($FF * (p-48) div 16) shl 16) or $FF00;
        64..79  : Brush.Color := (($FF * (79-p) div 16) shl 8) or $FF0000;
        80..95  : Brush.Color := (($FF * (95-p) div 16) shl 16);
      end;
      Rectangle(0,0,pic.Width,pic.Height);
      Font.Color := clWhite;
      Font.Size := pic.Width div 8;
      S := Inttostr(FFrame.FrameID);
      TextOut((pic.Width - TextWidth(S)) div 2, (pic.Height - TextHeight(S)) div 2, S);
    end;
    fmem := TMemoryStream.Create;
    fmem.Write(WEBCAM_FRAME_START_SEQ, sizeof(word));
    Sz := 0;
    fmem.Write(Sz, sizeof(Sz));
    pic.SaveToStream(fmem);
    Sz := fmem.Size - WEBCAM_FRAME_HEADER_SIZE;
    fmem.Position := sizeof(word);
    fmem.Write(Sz, sizeof(Sz));
    fmem.Position := 0;

    FFrame.NextFrame(fmem);
  finally
    pic.Free;
  end;
end;

procedure TMainForm.OnStrmWinClose(Sender : TObject;
  var CloseAction : TCloseAction);
begin
  if assigned(TStreamFrm(Sender).Data) then
    THTTP2BackgroundTask(TStreamFrm(Sender).Data).Close;
end;

procedure TMainForm.OnHasNextFrame(ATask : THTTP2BackgroundInStreamTask);
begin
  FUpdates.Push_back(ATask);
end;

procedure TMainForm.SuccessIOStream(ATask : THTTP2BackgroundTask);
var frm : TStreamFrm;
begin
  ATask.Lock;
  try
    if ATask is THTTP2BackgroundOutStreamTask then
      FStreaming := false;
    if Assigned(ATask.Data) then
    begin
      frm :=TStreamFrm(ATask.Data);
      ATask.Data := nil;
    end else
      frm := nil;
  finally
    ATask.UnLock;
  end;
  if assigned(frm) then
  begin
    frm.Close;
    frm.Free;
  end;
end;

function TMainForm.OnGetNextFrame(ATask : THTTP2BackgroundOutStreamTask) : integer;
begin
  if Assigned(FFrame.Frame) then
  begin
    FFrame.Lock;
    try
      ATask.PushFrame(FFrame.ExtractFrame);
      Result := FFrame.FrameID;
    finally
      FFrame.UnLock;
    end;
    FUpdates.Push_back(ATask);
  end else
    Result := 0;
end;

function TMainForm.ConsumeResponseToObj(ATask : THTTP2BackgroundTask
  ) : TJSONObject;
var jData : TJSONData;
    aResult : String;
    aCode, aRCode : Integer;
begin
  Result := nil;
  aCode := -1;
  aResult := cBAD;
  ATask.Response.Position := 0;
  try
    if ATask.Response.Size > 0 then
    begin
      try
        jData := GetJSON(ATask.Response);
        if (jData is TJSONObject) then
        begin
          Result := TJSONObject(jData);

          if Result.Find(cRESULT, jData) then
          begin
            aResult := jData.AsString;
            if SameText(aResult, cBAD) then
            begin
              if Result.Find(cCODE, jData) then
                aCode := jData.AsInteger;
              FreeAndNil(Result);
            end else
              aCode := 0;
          end else begin
            aResult := cBAD;
            FreeAndNil(Result);
          end;
        end else
          if assigned(jData) then FreeAndNil(jData);
      except
        Result := nil;
        aResult := cBAD;
      end;
    end;
  finally
    if (aCode > 0) or (not ATask.IsSilent) then
    begin
      if aCode < 0 then aRCode := 1 else
      if aCode > High(RESPONSE_ERRORS) then aRCode := 1 else
         aRCode := aCode;
      AddLog(Format('HTTP2 JSON Req result code [%d] - %s', [aCode, RESPONSE_ERRORS[aRCode]]));
    end;
  end;
end;

function TMainForm.LaunchOutStream : Boolean;
var
  Tsk : THTTP2BackgroundOutStreamTask;
  StrmWin : TStreamFrm;
begin
  Result := false;

  if DoInitMultiPipeling then
  begin
    Result := true;

    Application.CreateForm(TStreamFrm, StrmWin);

    Tsk := THTTP2BackgroundOutStreamTask.Create(FTaskPool.Tasks, FSetts, True);
    Tsk.OnFinish := @TaskFinished;
    Tsk.OnSuccess := @SuccessIOStream;
    Tsk.Data := StrmWin;

    StrmWin.Show;
    StrmWin.Data := Tsk;
    StrmWin.OnClose := @OnStrmWinClose;

    Tsk.LaunchStream(@OnGetNextFrame);
    FTaskPool.AddTask(Tsk);
    FStreaming := true;
  end;
end;

function TMainForm.LaunchInStream(const aDeviceName : String) : Boolean;
var
  Tsk : THTTP2BackgroundInStreamTask;
  StrmWin : TStreamFrm;
begin
  Result := false;

  if DoInitMultiPipeling then
  begin
    Result := true;

    Application.CreateForm(TStreamFrm, StrmWin);

    Tsk := THTTP2BackgroundInStreamTask.Create(FTaskPool.Tasks, FSetts, True);
    Tsk.OnFinish := @TaskFinished;
    Tsk.OnSuccess := @SuccessIOStream;
    Tsk.Data := StrmWin;

    StrmWin.Show;
    StrmWin.Data := Tsk;
    StrmWin.OnClose := @OnStrmWinClose;

    Tsk.LaunchStream(aDeviceName, @OnHasNextFrame);
    FTaskPool.AddTask(Tsk);
  end;
end;

procedure TMainForm.doPost(const aPath, aContent : String;
  OnSuccess : TOnHTTP2Finish; silent : Boolean);
var ptr : pointer;
begin
  if Length(aContent) > 0 then ptr := @(aContent[1]) else ptr := nil;
  doPost(aPath, ptr, Length(aContent), OnSuccess,
                          true, silent);
end;

procedure TMainForm.doPost(const aPath : String; aContent : Pointer;
  aContentSize : Int64; OnSuccess : TOnHTTP2Finish; stack : boolean;
  silent : Boolean);
var
  Tsk : THTTP2BackgroundTask;
begin
  if DoInitMultiPipeling then
  begin
    Tsk := THTTP2BackgroundTask.Create(FTaskPool.Tasks, FSetts, silent);
    Tsk.OnFinish := @TaskFinished;
    Tsk.OnSuccess := OnSuccess;
    Tsk.doPost(aPath, aContent, aContentSize, stack);
    FTaskPool.AddTask(Tsk);
  end;
end;

function TMainForm.GetDevice : String;
begin
  Result := AuthOpts.Cells[1, 3];
end;

function TMainForm.GetHost : String;
begin
  Result := AuthOpts.Cells[1, 0];
end;

function TMainForm.GetSID : String;
begin
  Result := AuthOpts.Cells[1, 4];
end;

procedure TMainForm.SetSID(AValue : String);
begin
  AuthOpts.Cells[1, 4] := AValue;
  FSetts.SID           := AValue;
end;

function TMainForm.GetSelectedDeviceName : String;
var
  jObj : TJSONData;
begin
  if DeviceList.ItemIndex >= 0 then
  begin
    Result := DeviceList.Items[DeviceList.ItemIndex];
    jObj := GetJSON(Result);
    if Assigned(jObj) then
    begin
      if jObj is TJSONObject then
        Result := TJSONObject(jObj).Get(cDEVICE, Result) else
        Result := jObj.AsString;
      jObj.Free;
    end;
  end else
    Result := Device;
end;

procedure TMainForm.Disconnect;
begin
  FTaskPool.Tasks.Terminate;
  Connected := false;
  NeedToUpdateDevices := false;
  NeedToUpdateRecords := false;
  NeedToUpdateMsgs    := false;
  RecordsGrid.RowCount := 1;
  MsgsGrid.RowCount := 1;
  DeviceList.Items.Clear;
  Image1.Picture := nil;
  Timer1.Enabled := false;
  LongTimer.Enabled := false;
  LastMsgsStamp := '{"msg":"sync"}';
  LastRecsStamp := '';
  SID := '';
end;

procedure TMainForm.SuccessUpdateDevices(ATask : THTTP2BackgroundTask);
var i : integer;
  jObj : TJSONObject;
  jArr : TJSONArray;
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    jObj := ConsumeResponseToObj(ATask);
    if Assigned(jObj) then
    begin
      jArr := TJSONArray(jObj.Find(cDEVICES));
      if Assigned(jArr) then
      begin
        for i := 0 to jArr.Count-1 do
        begin
          DeviceList.Items.Add(jarr.Items[i].AsJSON);
        end;
      end;
      FreeAndNil(jObj);
    end else
      Disconnect;
  end else
    Disconnect;
end;

procedure TMainForm.UpdateDevices;
var
  jObj : TJSONObject;
  aStr : String;
begin
  DeviceList.Items.BeginUpdate;
  try
    NeedToUpdateDevices := false;
    DeviceList.Items.Clear;
    jObj := TJSONObject.Create([cSHASH, SID]);
    try
      aStr := jObj.AsJSON;
    finally
      jObj.Free;
    end;
    doPost('/getDevicesOnline.json',aStr, @SuccessUpdateDevices);
  finally
    DeviceList.Items.EndUpdate;
  end;
end;

procedure TMainForm.SuccessUpdateRecords(ATask : THTTP2BackgroundTask);
var i, j, k, n, off : integer;
  jObj, jEl : TJSONObject;
  jArr : TJSONArray;
  aStr : String;
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    jObj := ConsumeResponseToObj(ATask);
    if Assigned(jObj) then
    try
      jArr := TJSONArray(jObj.Find(cRECORDS));
      off := RecordsGrid.RowCount;
      RecordsGrid.RowCount := jArr.Count + off;
      if jArr.Count > 0 then
      with RecordsGrid do
      begin
        jEl := TJSONObject(jArr[0]);
        ColCount := jEl.Count;
        for i := 0 to jEl.Count- 1 do
        begin
          aStr := jEl.Names[i];
          Cells[i, 0] := aStr;
          if SameText(aStr, cDEVICE) then
            ColWidths[i] := 128 else
          if SameText(aStr, cSTAMP) then
            ColWidths[i] := 164 else
          if SameText(aStr, cRID) then
            ColWidths[i] := 64 else
            ColWidths[i] := 100;
        end;
        for i := 0 to jArr.Count-1 do
        begin
          jEl := TJSONObject(jArr[i]);
          for n := 0 to jEl.Count-1 do
          begin
            k := -1;
            for j := 0 to ColCount-1 do
            begin
              if SameText(Cells[j, 0], jEl.Names[n]) then
              begin
                k := j;
                break;
              end;
            end;
            if SameText(cSTAMP, jEl.Names[n]) then
              LastRecsStamp := jEl.Items[n].AsString;
            if k >= 0 then
              Cells[ k, i+off ] := jEl.Items[n].AsString;
          end;
        end;
      end;
    finally
      jObj.Free;
    end;
  end else
    Disconnect;
end;

procedure TMainForm.UpdateRecords;
var
  jObj : TJSONObject;
  aStr : String;
begin
  RecordsGrid.BeginUpdate;
  try
    NeedToUpdateRecords := false;
    jObj := TJSONObject.Create([cSHASH, SID,
                                cSTAMP, LastRecsStamp]);
    try
      aStr := jObj.AsJSON;
    finally
      jObj.Free;
    end;
    doPost('/getRecordCount.json',aStr,@SuccessUpdateRecords);
  finally
    RecordsGrid.EndUpdate;
  end;
end;

procedure TMainForm.SuccessUpdateMsgs(ATask : THTTP2BackgroundTask);
var i, j, k, n, off : integer;
  jObj, jEl : TJSONObject;
  jArr : TJSONArray;
  aStr : String;
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    jObj := ConsumeResponseToObj(ATask);
    if Assigned(jObj) then
    try
      jArr := TJSONArray(jObj.Find(cMSGS));
      if assigned(jArr) then
      begin
        off := MsgsGrid.RowCount;
        MsgsGrid.RowCount := off + jArr.Count;
        if jArr.Count > 0 then
        with MsgsGrid do
        begin
          jEl := TJSONObject(jArr[0]);
          ColCount := jEl.Count;
          for i := 0 to jEl.Count- 1 do
          begin
            aStr := jEl.Names[i];
            Cells[i, 0] := aStr;
            if SameText(aStr, cMSG) then
              ColWidths[i] := 64 else
            if SameText(aStr, cDEVICE) then
              ColWidths[i] := 128 else
            if SameText(aStr, cPARAMS) then
              ColWidths[i] := 164 else
            if SameText(aStr, cSTAMP) then
              ColWidths[i] := 164 else
              ColWidths[i] := 100;
          end;
          for i := 0 to jArr.Count-1 do
          begin
            jEl := TJSONObject(jArr[i]);
            for n := 0 to jEl.Count-1 do
            begin
              k := -1;
              for j := 0 to ColCount-1 do
              begin
                if SameText(Cells[j, 0], jEl.Names[n]) then
                begin
                  k := j;
                  break;
                end;
              end;
              if SameText(cSTAMP, jEl.Names[n]) then
                LastMsgsStamp := jEl.Items[n].AsString;
              if k >= 0 then begin
                if (jEl.Items[n] is TJSONObject) or (jEl.Items[n] is TJSONArray) then
                begin
                  Cells[ k, i+off ] := jEl.Items[n].AsJSON;
                end else
                  Cells[ k, i+off ] := jEl.Items[n].AsString;
              end;
            end;
          end;
        end;
      end;
    finally
      jObj.Free;
    end;
  end else
    Disconnect;
end;

procedure TMainForm.UpdateMsgs;
var
  jObj : TJSONObject;
  aStr : String;
begin
  MsgsGrid.BeginUpdate;
  try
    NeedToUpdateMsgs := false;
    jObj := TJSONObject.Create([cSHASH, SID,
                                cSTAMP, LastMsgsStamp]);
    try
      aStr := jObj.AsJSON;
    finally
      jObj.Free;
    end;
    doPost('/getMsgs.json',aStr,@SuccessUpdateMsgs)
  finally
    MsgsGrid.EndUpdate;
  end;
end;

procedure TMainForm.SuccessDeleteRecords(ATask : THTTP2BackgroundTask);
var
  jObj : TJSONObject;
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    jObj := ConsumeResponseToObj(ATask);
    if Assigned(jObj) then
    begin
      FreeAndNil(jObj);
    end;
  end else
    Disconnect;
end;

procedure TMainForm.DeleteRecords(aIndices : TJSONArray);
var aMsg : TJSONObject;
begin
  if assigned(aIndices) then
  begin
    aMsg := TJSONObject.Create([cSHASH,   SID,
                                cRECORDS, aIndices]);
    try
      doPost('/deleteRecords.json', aMsg.AsJSON, @SuccessDeleteRecords)
    finally
      aMsg.Free;
    end;
  end;
end;

procedure TMainForm.SuccessSendMsg(ATask : THTTP2BackgroundTask);
var
  jObj : TJSONObject;
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    jObj := ConsumeResponseToObj(ATask);
    if Assigned(jObj) then
    begin
      FreeAndNil(jObj);
    end;
  end else
    Disconnect;
end;

procedure TMainForm.SendMsg(aMsg : TJSONObject);
begin
  if assigned(aMsg) then begin
    aMsg.Add(cSHASH, SID);
    doPost('/addMsgs.json', aMsg.AsJSON, @SuccessSendMsg);
  end;
end;

procedure TMainForm.SuccessRequestRecord(ATask : THTTP2BackgroundTask);
var
  jpg : TJPEGImage;
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    ATask.FResponse.Position := 0;
    Jpg := TJPEGImage.Create;
    try
      Jpg.LoadFromStream(ATask.FResponse);
      Image1.Picture.Jpeg := Jpg;
    finally
      Jpg.Free;
    end;
  end else
    Disconnect;
end;

procedure TMainForm.RequestRecord(rid : integer);
var
  jObj : TJSONObject;
begin
  jobj := TJSONObject.Create([cSHASH, SID,
                              cRID, rid]);
  try
    doPost('/getRecordData.json', jObj.AsJSON, @SuccessRequestRecord);
  finally
    jObj.Free;
  end;
end;

procedure TMainForm.SuccessSaveAsSnapshot(ATask : THTTP2BackgroundTask);
begin
  if ATask.ErrorCode = TASK_NO_ERROR then
  begin
    AddLog('file sended successful');
  end else
    Disconnect;
end;

procedure TMainForm.SaveAsSnapshot(const aFileName: String);
var
  FS : TFileStream;
  Buf : Pointer;
  Sz : Int64;
begin
  Buf := nil;
  FS := TFileStream.Create(aFileName, fmOpenRead);
  try
    Sz := FS.Size;
    FS.Position := 0;
    Buf := GetMem(Sz);
    if Assigned(Buf) then
      FS.Read(Buf^, Sz) else
    begin
      AddLog('Cant allocate memory');
      Exit;
    end;
  finally
    FS.Free;
  end;
  doPost('/addRecord.json?'+cSHASH+'='+HTTPEncode(SID), Buf, Sz, @SuccessSaveAsSnapshot, false);
end;

procedure TMainForm.DeleteSelected(andOlder : Boolean);
var
  arr : TJSONArray;
  rid_col, i, fstsel : Integer;
begin
  arr := TJSONArray.Create;
  rid_col := -1;
  for i := 0 to RecordsGrid.ColCount-1 do
  begin
    if (SameText(RecordsGrid.Cells[i, 0], cRID)) then
      rid_col := i;
  end;
  if rid_col >= 0 then
  begin
    fstsel := -1;
    for i := 1 to RecordsGrid.RowCount-1 do
    if RecordsGrid.IsCellSelected[rid_col, i] then
    begin
      if fstsel < 0 then fstsel := i;
      arr.Add(StrToint(RecordsGrid.Cells[rid_col, i]));
    end;
    if andOlder then arr.Insert(0, -1);
    DeleteRecords(arr);

    RecordsGrid.BeginUpdate;
    try
      for i := RecordsGrid.RowCount-1 downto 1 do
      begin
        if ((i < fstsel) and andOlder) or
           RecordsGrid.IsCellSelected[rid_col, i] then
          RecordsGrid.DeleteRow(i);
      end;
    finally
      RecordsGrid.EndUpdate;
    end;
  end;
end;

procedure TMainForm.AddLog(const STR : String);
begin
  LogMemo.Lines.Add('['+DateTimeToStr(Now)+'] '+Str);
end;

end.

