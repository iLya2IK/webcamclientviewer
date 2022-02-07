unit webcammain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ValEdit,
  ComCtrls, StdCtrls, Grids, JSONPropStorage, Buttons, libpascurl,
  BufferedStream, fpjson, jsonparser;

type

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
    procedure DeleteSelectedBtnClick(Sender : TObject);
    procedure DelSelAndOlderBtnClick(Sender : TObject);
    procedure DisconnectBtnClick(Sender : TObject);
    procedure SendMsgBtnClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure Image1Click(Sender : TObject);
    procedure LongTimerTimer(Sender : TObject);
    procedure Panel4Click(Sender : TObject);
    procedure AuthToServerBtnClick(Sender : TObject);
    procedure RecordsGridDblClick(Sender : TObject);
    procedure Timer1Timer(Sender : TObject);
    procedure ToolButton1Click(Sender : TObject);
    procedure ConfigClientBtnClick(Sender : TObject);
    procedure RefreshDevicesBtnClick(Sender : TObject);
    procedure CopyDeviceBtnClick(Sender : TObject);
    procedure RefreshRecordsBtnClick(Sender : TObject);
    procedure RefreshMsgsBtnClick(Sender : TObject);
  private
    procedure SetConnected(AValue : Boolean);
  private
    FCURL : CURL;
    FCURLM : CURLM;
    FResponse : TMemoryStream;
    FRequest : TBufferedStream;
    FConnected,
      NeedToUpdateDevices, NeedToUpdateRecords, NeedToUpdateMsgs: Boolean;
    LastMsgsStamp, LastRecsStamp : String;
    function doPost(const aPath, aReqQuery, aContent : String;
                          silent : Boolean = true) : Boolean;
    function GetDevice : String;
    function GetHost : String;
    function GetSID : String;
    procedure SetSID(AValue : String);
    function Write(ptr: Pointer; size: LongWord; nmemb: LongWord) : LongWord;
    function Read(ptr: Pointer; size: LongWord; nmemb: LongWord) : LongWord;

    function ConsumeResponseToObj(silent : Boolean = true) : TJSONObject;
    procedure Disconnect;

    procedure UpdateDevices;
    procedure UpdateRecords;
    procedure UpdateMsgs;
    procedure DeleteRecords(aIndices : TJSONArray);
    procedure SendMsg(aMsg : TJSONObject);
    procedure RequestRecord(rid : integer);
    procedure DeleteSelected(andOlder : Boolean);

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

uses LazUTF8, Clipbrd, configdlg;

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

const RESPONSE_ERRORS : Array [0..12] of String = (
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
                          'MALFORMED_REQUEST');

{$R *.lfm}

function WriteFunctionCallback(ptr: Pointer; size: LongWord;
  nmemb: LongWord; data: Pointer): LongWord; cdecl;
begin
  Result := TMainForm(data).Write(ptr, size, nmemb);
end;

function ReadFunctionCallback(ptr: Pointer; size: LongWord;
  nmemb: LongWord; data: Pointer): LongWord; cdecl;
begin
  Result := TMainForm(data).Read(ptr, size, nmemb);
end;

{ TMainForm }

procedure TMainForm.Panel4Click(Sender : TObject);
begin

end;

procedure TMainForm.FormCreate(Sender : TObject);
var
  defs : TStringList;
  i : integer;
begin
  TJSONData.CompressedJSON := true;
  defs := TStringList.Create;
  defs.Add('https://localhost:443');
  defs.Add('');
  defs.Add('');
  defs.Add('user-device');
  defs.Add('');
  curl_global_init(CURL_GLOBAL_ALL);
  FCURL := nil;
  FCURLM := nil;
  FResponse := TMemoryStream.Create;
  FRequest := TBufferedStream.Create;
  for i := 0 to defs.Count-1 do
  begin
    AuthOpts.Cells[1, i]:=AppConfig.ReadString(AuthOpts.Keys[i], defs[i]);
  end;
  FConnected := true;
  Disconnect;
  defs.free;
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

procedure TMainForm.DeleteSelectedBtnClick(Sender : TObject);
begin
  DeleteSelected(false);
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
  if Assigned(FCURL) then curl_easy_cleanup(FCURL);
  if Assigned(FCURLM) then curl_multi_cleanup(FCURLM);
  if Assigned(FResponse) then  FResponse.Free;
  if Assigned(FRequest) then  FRequest.Free;
  curl_global_cleanup();
  Timer1.Enabled := false;
  LongTimer.Enabled := false;
end;

procedure TMainForm.Image1Click(Sender : TObject);
begin

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

procedure TMainForm.AuthToServerBtnClick(Sender : TObject);
var
  jObj : TJSONObject;
  jData : TJSONData;
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
  if doPost('/authorize.json', '', aStr, false) and (FResponse.Size > 0) then
  begin
    jObj := ConsumeResponseToObj(false);
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

procedure TMainForm.ConfigClientBtnClick(Sender : TObject);
var
  jObj, jRes : TJSONObject;
  jArr : TJSONArray;
  aStr : String;
begin
  jObj := TJSONObject.Create([cSHASH, SID]);
  try
    aStr := jObj.AsJSON;
  finally
    jObj.Free;
  end;
  if doPost('/getConfig.json','',aStr) then
  begin
    jObj := ConsumeResponseToObj;
    if Assigned(jObj) then
    begin
      if jObj.Find(cCONFIG, jArr) then
      try
        if ConfDlg.Execute(jArr) then
        begin
          jRes := TJSONObject.Create([cSHASH, SID,
                                      cCONFIG, ConfDlg.ResultConf]);
          try
            aStr := jRes.AsJSON;
          finally
            jRes.Free;
          end;
          if doPost('/setConfig.json','',aStr) then
          begin
            //do nothing
          end else
            Disconnect;
        end;
      finally
        jObj.Free;
      end;
    end;
  end else
    Disconnect;
end;

procedure TMainForm.RefreshDevicesBtnClick(Sender : TObject);
begin
  UpdateDevices;
end;

procedure TMainForm.CopyDeviceBtnClick(Sender : TObject);
begin
  if DeviceList.ItemIndex >= 0 then
  begin
    Clipboard.AsText := DeviceList.Items[DeviceList.ItemIndex];
  end;
end;

procedure TMainForm.RefreshRecordsBtnClick(Sender : TObject);
begin
  UpdateRecords;
end;

procedure TMainForm.RefreshMsgsBtnClick(Sender : TObject);
begin
  UpdateMsgs;
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

function TMainForm.doPost(const aPath, aReqQuery, aContent : String;
  silent : Boolean) : Boolean;
var
  ErrorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  response_code : Longint;
  contentsize : integer;
  headers : pcurl_slist;
  still_running : Longint;
begin
  Result := false;

  if not Assigned(FCURLM) then begin
    FCURLM := curl_multi_init();
    curl_multi_setopt(FCURLM, CURLMOPT_PIPELINING, CURLPIPE_MULTIPLEX);
  end;

  if Assigned(FCURLM) then
  begin
    FCURL := curl_easy_init;
    if Assigned(FCurl) then
    begin
      FResponse.Position := 0;
      FResponse.Size := 0;
      curl_easy_setopt(FCURL, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2_0);
      curl_easy_setopt(FCURL, CURLOPT_URL, PChar(Host + aPath));
      curl_easy_setopt(FCURL, CURLOPT_POST, Longint(1));
      if Length(aReqQuery) > 0 then
      begin
        curl_easy_setopt(FCURL, CURLOPT_POSTFIELDSIZE, Length(aReqQuery));
        curl_easy_setopt(FCURL, CURLOPT_POSTFIELDS, PChar(aReqQuery));
      end;
      curl_easy_setopt(FCURL, CURLOPT_SSL_VERIFYPEER, Longint(0));
      curl_easy_setopt(FCURL, CURLOPT_SSL_VERIFYHOST, Longint(0));
      curl_easy_setopt(FCURL, CURLOPT_WRITEDATA, Pointer(Self));
      curl_easy_setopt(FCURL, CURLOPT_WRITEFUNCTION,
         @WriteFunctionCallback);
      contentsize := Length(aContent);
      headers := nil;
      headers := curl_slist_append(headers, Pchar('content-length: ' + inttostr(contentsize)));
      curl_easy_setopt(FCURL, CURLOPT_HTTPHEADER, headers);
      curl_easy_setopt(FCURL, CURLOPT_PIPEWAIT, Longint(1));

      if (contentsize > 0) then begin
        FRequest.SetPtr(@(aContent[1]), contentsize);
        curl_easy_setopt(FCURL, CURLOPT_READDATA, Pointer(Self));
        curl_easy_setopt(FCURL, CURLOPT_READFUNCTION,
           @ReadFunctionCallback);
        curl_easy_setopt(FCURL, CURLOPT_INFILESIZE, Longint(contentsize));
      end;
      curl_easy_setopt(FCURL, CURLOPT_ERRORBUFFER, PChar(ErrorBuffer));


      response_code := Integer(curl_multi_add_handle(FCURLM, FCURL));
      if response_code = Integer( CURLE_OK ) then
      begin
        still_running := 0;

        repeat
            response_code := Integer(curl_multi_perform(FCURLM, @still_running));

            if ((response_code = Integer( CURLE_OK )) and
                (still_running > 0)) then
              response_code := Integer(curl_multi_poll(FCURLM, [], 0, 3000, nil));

            if (response_code <> Integer( CURLE_OK )) then
              break;

            Sleep(10);
        until (still_running = 0);

        if response_code = Integer( CURLE_OK ) then
        begin
          curl_easy_getinfo(FCURL, CURLINFO_RESPONSE_CODE, @response_code);
          if not silent then
            AddLog(Format('HTTP2 "%s" OK. Code - %d', [aPath, response_code]));
          Result := true;
        end else
        begin
          AddLog(Format('HTTP2 "%s" FAIL. Code - %d', [aPath, response_code]));
          AddLog(ErrorBuffer);
        end;
        curl_multi_remove_handle(FCURLM, FCURL);
      end else
        AddLog(Format('Cant attach easy req to multi. Code - %d', [response_code]));

      curl_slist_free_all(headers);
      curl_easy_cleanup(FCURL);
      FCURL := nil;
    end;
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
end;

function TMainForm.Write(ptr : Pointer; size : LongWord; nmemb : LongWord
  ) : LongWord;
begin
  Result := FResponse.Write(ptr^, size * nmemb);
end;

function TMainForm.Read(ptr : Pointer; size : LongWord; nmemb : LongWord
  ) : LongWord;
begin
  Result := FRequest.Read(ptr^, nmemb * size);
end;

function TMainForm.ConsumeResponseToObj(silent : Boolean) : TJSONObject;
var jData : TJSONData;
    aResult : String;
    aCode, aRCode : Integer;
begin
  aCode := -1;
  aResult := cBAD;
  FResponse.Position := 0;
  try
    if FResponse.Size > 0 then
    begin
      try
        jData := GetJSON(FResponse);
        if (jData is TJSONObject) then
        begin
          Result := TJSONObject(jData);
          if Result.Find(cRESULT, jData) then
          begin
            aResult := jData.AsString;
            if SameText(aResult, cBAD) then
            begin
              if Result.Find(cCODE, jData) then
              begin
                aCode := jData.AsInteger;
              end;
              FreeAndNil(Result);
            end else
              aCode := 0;
          end else begin
            aResult := cBAD;
            FreeAndNil(Result);
          end;
        end else
          if assigned(jData) then jData.Free;
      except
        Result := nil;
        aResult := cBAD;
      end;
    end;
  finally
    if (aCode > 0) or (not silent) then
    begin
      if aCode > High(RESPONSE_ERRORS) then aRCode := 1 else aRCode := aCode;
      AddLog('JSON response ' + aResult + ' ' + RESPONSE_ERRORS[aRCode] + '('+
                                         inttostr(aCode)+')');
    end;
  end;
end;

procedure TMainForm.Disconnect;
begin
  if not Assigned(FCURLM) then begin
    FCURLM := curl_multi_init();
    FCURLM := nil;
  end;
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

procedure TMainForm.UpdateDevices;
var i : integer;
  jObj : TJSONObject;
  jArr : TJSONArray;
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
    if doPost('/getDevicesOnline.json','',aStr) then
    begin
      jObj := ConsumeResponseToObj();
      if Assigned(jObj) then
      begin
        jArr := TJSONArray(jObj.Find(cDEVICES));
        if Assigned(jArr) then
        begin
          for i := 0 to jArr.Count-1 do
          begin
            DeviceList.Items.Add(jarr.Items[i].AsString);
          end;
        end;
        FreeAndNil(jObj);
      end else
        Disconnect;
    end else
      Disconnect;
  finally
    DeviceList.Items.EndUpdate;
  end;
end;

procedure TMainForm.UpdateRecords;
var i, j, k, n, off : integer;
  jObj, jEl : TJSONObject;
  jArr : TJSONArray;
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
    if doPost('/getRecordCount.json','',aStr) then
    begin
      jObj := ConsumeResponseToObj();
      if Assigned(jObj) then
      begin
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
      end;
    end else
      Disconnect;
  finally
    RecordsGrid.EndUpdate;
  end;
end;

procedure TMainForm.UpdateMsgs;
var i, j, k, n, off : integer;
  jObj, jEl : TJSONObject;
  jArr : TJSONArray;
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
    if doPost('/getMsgs.json','',aStr) then
    begin
      jObj := ConsumeResponseToObj();
      if Assigned(jObj) then
      begin
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
      end;
    end else
      Disconnect;
  finally
    MsgsGrid.EndUpdate;
  end;
end;

procedure TMainForm.DeleteRecords(aIndices : TJSONArray);
var aMsg, jObj : TJSONObject;
begin
  if assigned(aIndices) then
  begin
    aMsg := TJSONObject.Create([cSHASH,   SID,
                                cRECORDS, aIndices]);
    try
      if doPost('/deleteRecords.json', '', aMsg.AsJSON) then
      begin
        jObj := ConsumeResponseToObj();
        if Assigned(jObj) then
        begin
          FreeAndNil(jObj);
        end;
      end else
        Disconnect;
    finally
      aMsg.Free;
    end;
  end;
end;

procedure TMainForm.SendMsg(aMsg : TJSONObject);
var
  jObj : TJSONObject;
begin
  if assigned(aMsg) then begin
    aMsg.Add(cSHASH, SID);
    if doPost('/addMsgs.json', '', aMsg.AsJSON) then
    begin
      jObj := ConsumeResponseToObj();
      if Assigned(jObj) then
      begin
        FreeAndNil(jObj);
      end;
    end else
      Disconnect;
  end;
end;

procedure TMainForm.RequestRecord(rid : integer);
var
  jObj : TJSONObject;
  jpg : TJPEGImage;
begin
  jobj := TJSONObject.Create([cSHASH, SID,
                              cRID, rid]);
  try
    if doPost('/getRecordData.json', '', jObj.AsJSON) then
    begin
      FResponse.Position := 0;
      Jpg := TJPEGImage.Create;
      try
        Jpg.LoadFromStream(FResponse);
        Image1.Picture.Jpeg := Jpg;
      finally
        Jpg.Free;
      end;
    end else
      Disconnect;
  finally
    jObj.Free;
  end;
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

