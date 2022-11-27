unit webcammain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ValEdit,
  ComCtrls, StdCtrls, Grids, JSONPropStorage, Buttons, ExtDlgs,
  fpjson, jsonparser,
  wccurlclient,
  ECommonObjs, Types, LCLType;

type

  { TMainForm }

  TMainForm = class(TForm)
    AuthOpts : TValueListEditor;
    AuthToServerBtn : TToolButton;
    VerifyTSLCB : TCheckBox;
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
    Panel12 : TPanel;
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
    procedure AuthOptsClick(Sender : TObject);
    procedure AuthOptsEditingDone(Sender : TObject);
    procedure DeleteSelectedBtnClick(Sender : TObject);
    procedure DelSelAndOlderBtnClick(Sender : TObject);
    procedure DeviceListDrawItem(Control : TWinControl; Index : Integer;
      ARect : TRect; State : TOwnerDrawState);
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
    procedure VerifyTSLCBChange(Sender : TObject);
  private
    CURLClient : TWCCURLClient;

    procedure OnInitCURL(Sender : TObject);
    procedure OnConnectedChanged(aValue : Boolean);
    procedure OnDisconnect(Sender : TObject);
    procedure AddLog(const aStr : String);
    procedure OnSetSID(const AValue : String);
    procedure OnSuccessAuth({%H-}aTask : THTTP2BackgroundTask; {%H-}aObj : TJSONObject);
    procedure OnSuccessUpdateDevices({%H-}aTask : THTTP2BackgroundTask; aArr : TJSONArray);
    procedure OnSuccessUpdateStreams({%H-}aTask : THTTP2BackgroundTask; aArr : TJSONArray);
    procedure OnSuccessUpdateMsgs({%H-}aTask : THTTP2BackgroundTask; aArr : TJSONArray);
    procedure OnSuccessUpdateRecords({%H-}aTask : THTTP2BackgroundTask; aArr : TJSONArray);
    procedure OnSuccessSendMsg({%H-}aTask : THTTP2BackgroundTask; {%H-}aObj : TJSONObject);
    procedure OnSuccessDeleteRecords(aTask : THTTP2BackgroundTask; {%H-}aObj : TJSONObject);
    procedure OnSuccessRequestRecord(aData : TCustomMemoryStream);
    procedure OnSuccessSaveAsSnapshot(Sender : TObject);
    procedure OnSuccessGetConfig({%H-}aTask : THTTP2BackgroundTask; aArr : TJSONArray);
    procedure OnAfterLaunchInOutStream(Sender : THTTP2BackgroundTask);
    procedure OnSynchroUpdateTask(aTask : THTTP2BackgroundTask);
    procedure OnSuccessIOStream(aTask : THTTP2BackgroundTask);

    procedure GenNewFrame;
    procedure OnStrmWinClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
  private
    FStreamingDevices : TStringList;
    procedure StreamingDevicesChange(Sender : TObject);

    function GetSelectedDeviceName : String;
    function GetRidCol : Integer;
    procedure DeleteSelected(andolder : Boolean);

    function GetProxy : String;
    function GetMeta : String;
    function GetDevice : String;
    function GetHost : String;
    function GetSID : String;
  end;

var
  MainForm : TMainForm;

implementation

uses LazUTF8, Clipbrd, configdlg, streamwin, wcwebcamconsts;

{$R *.lfm}

const HOST_POS = 0;
      PROXY_POS = 1;
      USER_POS = 2;
      PWRD_POS = 3;
      DEVICE_POS = 4;
      META_POS = 5;
      SID_POS = 6;

{ TMainForm }

procedure TMainForm.FormCreate(Sender : TObject);
var
  defs : TStringList;
  i : integer;
begin
  CURLClient := TWCCURLClient.Create;

  CURLClient.OnInitCURL := @OnInitCURL;
  CURLClient.OnConnectedChanged := @OnConnectedChanged;
  CURLClient.OnDisconnect := @OnDisconnect;
  CURLClient.OnAddLog := @AddLog;
  CURLClient.OnSuccessAuth := @OnSuccessAuth;
  CURLClient.OnSuccessUpdateDevices := @OnSuccessUpdateDevices;
  CURLClient.OnSuccessUpdateStreams := @OnSuccessUpdateStreams;
  CURLClient.OnSuccessUpdateMsgs := @OnSuccessUpdateMsgs;
  CURLClient.OnSuccessUpdateRecords := @OnSuccessUpdateRecords;
  CURLClient.OnSuccessSendMsg := @OnSuccessSendMsg;
  CURLClient.OnSuccessDeleteRecords := @OnSuccessDeleteRecords;
  CURLClient.OnSuccessRequestRecord := @OnSuccessRequestRecord;
  CURLClient.OnSuccessSaveAsSnapshot := @OnSuccessSaveAsSnapshot;
  CURLClient.OnSuccessGetConfig := @OnSuccessGetConfig;
  CURLClient.OnAfterLaunchInStream := @OnAfterLaunchInOutStream;
  CURLClient.OnAfterLaunchOutStream := @OnAfterLaunchInOutStream;
  CURLClient.OnSIDSetted := @OnSetSID;
  CURLClient.OnSynchroUpdateTask := @OnSynchroUpdateTask;
  CURLClient.OnSuccessIOStream := @OnSuccessIOStream;

  FStreamingDevices := TStringList.Create;
  FStreamingDevices.OnChange := @StreamingDevicesChange;

  defs := TStringList.Create;
  defs.Add('https://localhost:443');
  defs.Add('');
  defs.Add('');
  defs.Add('');
  defs.Add('user-device');
  defs.Add('');

  for i := 0 to defs.Count-1 do
  begin
    AuthOpts.Cells[1, i]:=AppConfig.ReadString(AuthOpts.Keys[i], defs[i]);
  end;

  defs.free;

  VerifyTSLCB.Checked := AppConfig.ReadBoolean(VerifyTSLCB.Name, true);

  AuthOptsEditingDone(nil);

  CURLClient.Start;
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
    CURLClient.SendMsg(aMsg);
  finally
    aMsg.Free;
  end;
end;

procedure TMainForm.DisconnectBtnClick(Sender : TObject);
begin
  CURLClient.Disconnect;
end;

procedure TMainForm.LaunchInStrmClick(Sender : TObject);
begin
  if CURLClient.LaunchInStream(GetSelectedDeviceName) then
  begin
    AddLog('In stream launched');
  end else
    CURLClient.Disconnect;
end;

procedure TMainForm.LaunchStreamBtnClick(Sender : TObject);
begin
  if not CURLClient.IsStreaming then
  begin
    if CURLClient.LaunchOutStream then
    begin
      AddLog('Out stream launched');
    end else
      CURLClient.Disconnect;
  end else
    AddLog('Out stream already launched');
end;

procedure TMainForm.DeleteSelectedBtnClick(Sender : TObject);
begin
  DeleteSelected(false);
end;

procedure TMainForm.AuthOptsEditingDone(Sender : TObject);
begin
  CURLClient.Setts.Device := GetDevice;
  CURLClient.Setts.SetProxy(GetProxy);
  CURLClient.Setts.MetaData := GetMeta;
  CURLClient.Setts.Host   := GetHost;
  CURLClient.Setts.SID    := GetSID;
end;

procedure TMainForm.AuthOptsClick(Sender : TObject);
begin

end;

procedure TMainForm.DelSelAndOlderBtnClick(Sender : TObject);
begin
  DeleteSelected(true);
end;

procedure TMainForm.DeviceListDrawItem(Control : TWinControl; Index : Integer;
  ARect : TRect; State : TOwnerDrawState);

function FindDevice(const DN : String): integer;
var i : integer;
begin
  Result := -1;
  for i := 0 to FStreamingDevices.Count-1 do
  begin
    if SameText(FStreamingDevices[i], DN) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure DrawPic(imi : integer);
var
  B : TBitmap;
begin
  if imi >= 0 then begin
    B := TBitmap.Create;
    try
      B.Width := 16;
      B.Height := 16;
      ImageList1.Getbitmap(imi, B);
      DeviceList.Canvas.Draw(aRect.Left, aRect.Top, B);
    finally
      B.Free;
    end;
    aRect.Left := aRect.Left + 16;
  end else
    aRect.Left := aRect.Left + 16;
end;

var
  k, imi : integer;
  ts : TTextStyle;
  S  : String;
begin
  S := DeviceList.Items[Index];
  k := FindDevice(CURLClient.ExtractDeviceName(S));
  if k >= 0 then
    imi := 12 else
    imi := -1;

  with DeviceList.Canvas do
  begin
    ts := TextStyle;
    ts.Alignment  := taLeftJustify;
    ts.Layout     := tlCenter;
    ts.Wordbreak  := false;
    ts.SingleLine := True;
    ts.Opaque     := false;

    if odSelected in State then
    begin
      brush.Color := clBlack;
      font.Color := clWhite;
      pen.Color := clYellow;
      pen.Style := psDot;
    end else
    begin
      brush.Color := clWhite;
      font.Color  := clBlack;
      pen.Color   := brush.Color;
      pen.Style   := psSolid;
    end;

    Rectangle(aRect);

    DrawPic(imi);

    DeviceList.Canvas.TextRect(aRect, aRect.Left+2, aRect.Top + 2, S, ts);
  end;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
var i : integer;
begin
  for i := 0 to AuthOpts.RowCount-1 do
  begin
    AppConfig.WriteString(AuthOpts.Keys[i], AuthOpts.Cells[1, i]);
  end;
  AppConfig.WriteBoolean(VerifyTSLCB.Name, VerifyTSLCB.Checked);

  Timer1.Enabled := false;
  LongTimer.Enabled := false;
  TaskTimer.Enabled := false;

  CURLClient.Free;
  FStreamingDevices.Free;
end;

procedure TMainForm.LongTimerTimer(Sender : TObject);
begin
  if CURLClient.Connected then
  begin
    if RecsAU.Checked then CURLClient.NeedToUpdateRecords := true;
    if DevicesAU.Checked then CURLClient.NeedToUpdateDevices := true;
    if MsgsAU.Checked then CURLClient.NeedToUpdateMsgs := true;
  end;
end;

procedure TMainForm.AuthToServerBtnClick(Sender : TObject);
var
  aName, aPass : String;
begin
  aName := AuthOpts.Cells[1, USER_POS];
  aPass := AuthOpts.Cells[1, PWRD_POS];
  CURLClient.VerifyTSL := VerifyTSLCB.Checked;
  CURLClient.Authorize(aName, aPass);
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
      CURLClient.RequestRecord(StrToInt(rid));
    end;
  end;
end;

procedure TMainForm.TaskTimerTimer(Sender : TObject);
begin
  CURLClient.TasksProceed;
end;

procedure TMainForm.Timer1Timer(Sender : TObject);
begin
  if CURLClient.Connected then
  begin
    CURLClient.Proceed;

    if CURLClient.IsStreaming then
      GenNewFrame;
  end;
end;

procedure TMainForm.ToolButton1Click(Sender : TObject);
var msg : TJSONObject;
begin
  msg := TJSONObject.Create([cMSG, cSYNC]);
  try
    CURLClient.SendMsg(msg);
  finally
    msg.Free;
  end;
end;

procedure TMainForm.ConfigClientBtnClick(Sender : TObject);
begin
  CURLClient.GetConfig;
end;

procedure TMainForm.RefreshDevicesBtnClick(Sender : TObject);
begin
  DeviceList.Clear;
  CURLClient.UpdateDevices;
  FStreamingDevices.Clear;
  CURLClient.UpdateStreams;
end;

procedure TMainForm.CopyDeviceBtnClick(Sender : TObject);
begin
  Clipboard.AsText := GetSelectedDeviceName;
end;

procedure TMainForm.RefreshRecordsBtnClick(Sender : TObject);
begin
  CURLClient.UpdateRecords;
end;

procedure TMainForm.RefreshMsgsBtnClick(Sender : TObject);
begin
  CURLClient.UpdateMsgs;
end;

procedure TMainForm.ToolButton2Click(Sender: TObject);
var
  FS : TFileStream;
  Buf : Pointer;
  Sz : Int64;
begin
  if OpenPictureDialog1.Execute then
  begin
    Buf := nil;
    FS := TFileStream.Create(OpenPictureDialog1.FileName, fmOpenRead);
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
      CURLClient.SaveAsSnapshot(Buf, Sz);
    finally
      FS.Free;
    end;
  end;
end;

procedure TMainForm.VerifyTSLCBChange(Sender : TObject);
begin
   CURLClient.VerifyTSL := VerifyTSLCB.Checked;
end;

procedure TMainForm.OnInitCURL(Sender : TObject);
begin
  TaskTimer.Enabled := true;
end;

procedure TMainForm.OnConnectedChanged(aValue : Boolean);
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
  SetEnabled(MsgsToolbar, AValue);
  SetEnabled(RecordsToolbar, AValue);
  SetEnabled(DevicesToolbar, AValue);
  SendMsgBtn.Enabled := AValue;
  DisconnectBtn.Enabled := AValue;
  ConfigClientBtn.Enabled := AValue;
end;

procedure TMainForm.OnDisconnect(Sender : TObject);
begin
  RecordsGrid.RowCount := 1;
  MsgsGrid.RowCount := 1;
  DeviceList.Items.Clear;
  Image1.Picture := nil;
  Timer1.Enabled := false;
  LongTimer.Enabled := false;
end;

procedure TMainForm.AddLog(const aStr : String);
begin
  LogMemo.Lines.Add('['+DateTimeToStr(Now)+'] '+aStr);
  CURLClient.Log.Clear;
end;

procedure TMainForm.OnSuccessAuth(aTask : THTTP2BackgroundTask;
  aObj : TJSONObject);
begin
  Timer1.Enabled := true;
  LongTimer.Enabled := true;
end;

procedure TMainForm.OnSuccessUpdateDevices(aTask : THTTP2BackgroundTask;
  aArr : TJSONArray);
var i : integer;
begin
  DeviceList.Items.BeginUpdate;
  for i := 0 to aArr.Count-1 do
  begin
    DeviceList.Items.Add(aArr.Items[i].AsJSON);
  end;
  DeviceList.Items.EndUpdate;
end;

procedure TMainForm.OnSuccessUpdateStreams(aTask : THTTP2BackgroundTask;
  aArr : TJSONArray);
var i : integer;
begin
  FStreamingDevices.BeginUpdate;
  for i := 0 to aArr.Count-1 do
  begin
    FStreamingDevices.Add(CURLClient.ExtractDeviceName(aArr.Items[i].AsJSON));
  end;
  FStreamingDevices.EndUpdate;
end;

procedure TMainForm.OnSuccessUpdateMsgs(aTask : THTTP2BackgroundTask;
  aArr : TJSONArray);
var i, j, k, n, off : integer;
  jEl : TJSONObject;
  aStr : String;
begin
  MsgsGrid.BeginUpdate;
  try
    off := MsgsGrid.RowCount;
    MsgsGrid.RowCount := off + aArr.Count;
    if aArr.Count > 0 then
    with MsgsGrid do
    begin
      jEl := TJSONObject(aArr[0]);
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
      for i := 0 to aArr.Count-1 do
      begin
        jEl := TJSONObject(aArr[i]);
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
  finally
    MsgsGrid.EndUpdate;
  end;
end;

procedure TMainForm.OnSuccessUpdateRecords(aTask : THTTP2BackgroundTask;
  aArr : TJSONArray);
var i, j, k, n, off : integer;
  jEl : TJSONObject;
  aStr : String;
begin
  with RecordsGrid do
  if aArr.Count > 0 then
  begin
    BeginUpdate;
    try
      off := RecordsGrid.RowCount;
      RecordsGrid.RowCount := aArr.Count + off;
      jEl := TJSONObject(aArr[0]);
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
      for i := 0 to aArr.Count-1 do
      begin
        jEl := TJSONObject(aArr[i]);
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
          if k >= 0 then
            Cells[ k, i+off ] := jEl.Items[n].AsString;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TMainForm.OnSuccessSendMsg(aTask : THTTP2BackgroundTask;
  aObj : TJSONObject);
begin
  //do nothing
end;

procedure TMainForm.OnSuccessDeleteRecords(aTask : THTTP2BackgroundTask;
  aObj : TJSONObject);
var i, j, rid_col, min_id, cid : integer;
    A : TJSONArray;
    O : TJSONData;
    delolder : boolean;
begin
  RecordsGrid.BeginUpdate;
  try
    rid_col := GetRidCol;
    if rid_col >= 0 then
    begin
      aTask.Request.Position := 0;
      O := GetJSON(aTask.Request);
      if (not Assigned(O)) and not (O is TJSONObject) then Exit;
      try
        A := TJSONArray(TJSONObject(O).Find(cRECORDS));
        if Assigned(A) then begin
          delolder := false;
          min_id := MaxInt;
          for j := 0 to A.Count-1 do
          begin
            if A.Items[j].AsInteger < 0 then
              delolder := true
            else
            if min_id > A.Items[j].AsInteger then
              min_id := A.Items[j].AsInteger;
          end;
          for i := RecordsGrid.RowCount-1 downto 1 do
          begin
            cid := StrToInt(RecordsGrid.Cells[rid_col, i]);

            if (cid <= min_id) and (delolder) then
              RecordsGrid.DeleteRow(i) else
            for j := 0 to A.Count-1 do
              if cid = A.Items[j].AsInteger then
              begin
                RecordsGrid.DeleteRow(i);
                break;
              end;
          end;
        end;
      finally
        FreeAndNil(O);
      end;
    end;
  finally
    RecordsGrid.EndUpdate;
  end;
end;

procedure TMainForm.OnSuccessRequestRecord(aData : TCustomMemoryStream);
var
  Jpg : TJPEGImage;
begin
  Jpg := TJPEGImage.Create;
  try
    Jpg.LoadFromStream(aData);
    Image1.Picture.Jpeg := Jpg;
  finally
    Jpg.Free;
  end;
end;

procedure TMainForm.OnSuccessSaveAsSnapshot(Sender : TObject);
begin
  AddLog('file sended successful');
end;

procedure TMainForm.OnSuccessGetConfig(aTask : THTTP2BackgroundTask;
  aArr : TJSONArray);
var aStr : String;
    jRes : TJSONObject;
begin
  if ConfDlg.Execute(aArr) then
  begin
    jRes := TJSONObject.Create([cSHASH, GetSID,
                                cCONFIG, ConfDlg.ResultConf]);
    try
      aStr := jRes.AsJSON;
    finally
      jRes.Free;
    end;
    CURLClient.SetConfig(aStr);
  end;
end;

procedure TMainForm.OnAfterLaunchInOutStream(Sender : THTTP2BackgroundTask);
var
  Tsk : THTTP2BackgroundTask;
  StrmWin : TStreamFrm;
begin
  Tsk := THTTP2BackgroundTask(Sender);
  Application.CreateForm(TStreamFrm, StrmWin);
  Tsk.Data := StrmWin;

  StrmWin.Show;
  StrmWin.Data := Tsk;
  StrmWin.OnClose := @OnStrmWinClose;
end;

procedure TMainForm.OnSynchroUpdateTask(aTask : THTTP2BackgroundTask);
begin
  if Assigned( aTask.Data ) then
  begin
    if aTask is THTTP2BackgroundInStreamTask then
      TStreamFrm(aTask.Data).UpdateFrame(THTTP2BackgroundInStreamTask(aTask).PopFrame)
    else
    if aTask is THTTP2BackgroundOutStreamTask then
    begin
      CURLClient.Frame.Lock;
      try
        TStreamFrm(aTask.Data).UpdateFrame(CURLClient.Frame.LstFrame, false);
      finally
        CURLClient.Frame.UnLock;
      end;
    end;
  end;
end;

procedure TMainForm.OnSuccessIOStream(aTask : THTTP2BackgroundTask);
var frm : TStreamFrm;
begin
  ATask.Lock;
  try
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
      p := (CURLClient.Frame.FrameID mod 96);
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
      S := Inttostr(CURLClient.Frame.FrameID);
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

    CURLClient.Frame.NextFrame(fmem);
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

procedure TMainForm.StreamingDevicesChange(Sender : TObject);
begin
  DeviceList.Invalidate;
end;

function TMainForm.GetProxy : String;
begin
  Result := AuthOpts.Cells[1, PROXY_POS];
end;

function TMainForm.GetMeta : String;
begin
  Result := AuthOpts.Cells[1, META_POS];
end;

function TMainForm.GetDevice : String;
begin
  Result := AuthOpts.Cells[1, DEVICE_POS];
end;

function TMainForm.GetHost : String;
begin
  Result := AuthOpts.Cells[1, HOST_POS];
end;

function TMainForm.GetSID : String;
begin
  Result := AuthOpts.Cells[1, SID_POS];
end;

procedure TMainForm.OnSetSID(const AValue : String);
begin
  AuthOpts.Cells[1, SID_POS] := AValue;
end;

function TMainForm.GetSelectedDeviceName : String;
begin
  if DeviceList.ItemIndex >= 0 then
  begin
    Result := CURLClient.ExtractDeviceName(DeviceList.Items[DeviceList.ItemIndex]);
  end else
    Result := GetDevice;
end;

function TMainForm.GetRidCol : Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to RecordsGrid.ColCount-1 do
  begin
    if (SameText(RecordsGrid.Cells[i, 0], cRID)) then
      Result := i;
  end;
end;

procedure TMainForm.DeleteSelected(andolder : Boolean);
var
  arr : TJSONArray;
  rid_col, i, fstsel : Integer;
begin
  arr := TJSONArray.Create;
  rid_col := GetRidCol;
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
    CURLClient.DeleteRecords(arr);
  end;
end;

end.

