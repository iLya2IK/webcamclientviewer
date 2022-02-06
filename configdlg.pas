unit configdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Grids, fpjson, jsonparser, OGLFastNumList;

type

  TConfRec = record
    miv, mav, dv, fv : double;
  end;
  PConfRec = ^TConfRec;

  { TConfDlg }

  TConfDlg = class(TForm)
    ConfigGrid : TStringGrid;
    ToolBar1 : TToolBar;
    ToolButton1 : TToolButton;
    ToolButton2 : TToolButton;
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure ToolButton1Click(Sender : TObject);
    procedure ToolButton2Click(Sender : TObject);
  private
    FCurConf, FResConf : TJSONArray;
    FDefs : TFastMapPtr;
  public
    function Execute(curConf : TJSONArray) : Boolean;
    property ResultConf : TJSONArray read FResConf;
  end;

var
  ConfDlg : TConfDlg;

implementation

const
cKIND      = 'kind';
cDESCR     = 'descr';
cMIVALUE   = 'miv';
cMAVALUE   = 'mav';
cDEFVALUE  = 'dv';
cFVALUE    = 'fv';

{$R *.lfm}

{ TConfDlg }

procedure TConfDlg.FormCreate(Sender : TObject);
begin
  FDefs := TFastMapPtr.Create(true);
end;

procedure TConfDlg.FormDestroy(Sender : TObject);
begin
  FDefs.Free;
end;

procedure TConfDlg.ToolButton1Click(Sender : TObject);
var
  k : integer;
  o : TJSONObject;
  rec : PConfRec;
  s : String;
  fv : Double;
  fs : TFormatSettings;
begin
  //move all data to FCurConf
  FResConf := TJSONArray.Create();
  for k := 1 to ConfigGrid.RowCount-1 do
  begin
    rec := PConfRec(FDefs.Value[k]);
    if assigned(rec) then
    begin
      s := ConfigGrid.Cells[4, k];
      if length(s) = 0 then begin
        fv := rec^.dv;
      end else
      begin
        fs := DefaultFormatSettings;
        fs.DecimalSeparator := '.';
        s := StringReplace(s, ',', '.', []);

        if not TryStrToFloat(s, fv, fs) then
          fv := rec^.fv;
        if fv > rec^.mav then fv := rec^.mav;
        if fv < rec^.miv then fv := rec^.miv;
      end;
      o := TJSONObject.Create([cKIND, k,
                               cFVALUE, fv]);
      FResConf.Add(o);
    end;
  end;

  ModalResult := mrOK;
end;

procedure TConfDlg.ToolButton2Click(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

function TConfDlg.Execute(curConf : TJSONArray) : Boolean;
var i, k : integer;
    o : TJSONObject;
    d : TJSONData;
    desc : String;
    miv, mav, fv, dv : double;
    rec : PConfRec;
begin
  FCurConf := curConf;
  ConfigGrid.ColWidths[0] := 32;
  ConfigGrid.ColWidths[1] := 180;
  ConfigGrid.Cells[0, 0] := 'kind';
  ConfigGrid.Cells[1, 0] := 'descr';
  ConfigGrid.Cells[2, 0] := 'min allow';
  ConfigGrid.Cells[3, 0] := 'max allow';
  ConfigGrid.Cells[4, 0] := 'value';
  FDefs.Clear;
  ConfigGrid.RowCount := 1;
  for i := 0 to curConf.Count-1 do
  begin
    if curConf[i] is TJSONObject then
    begin
      o := TJSONObject(curConf[i]);
      d := o.Find(cKIND);
      if assigned(d) and (d is TJSONIntegerNumber) then
        k := d.AsInteger
      else continue;
      d := o.Find(cDESCR);
      if assigned(d) then
        desc := d.AsString
      else desc := '';
      d := o.Find(cMIVALUE);
      if assigned(d) and (d is TJSONNumber) then
        miv := d.AsFloat
      else continue;
      d := o.Find(cMAVALUE);
      if assigned(d) and (d is TJSONNumber) then
        mav := d.AsFloat
      else continue;
      d := o.Find(cDEFVALUE);
      if assigned(d) and (d is TJSONNumber) then
      begin
        dv := d.AsFloat;
      end
      else continue;
      d := o.Find(cFVALUE);
      if assigned(d) and (d is TJSONNumber) then
      begin
        fv := d.AsFloat;
      end
      else continue;

      rec := GetMem(Sizeof(TConfRec));
      rec^.dv := dv;
      rec^.fv := fv;
      rec^.miv := miv;
      rec^.mav := mav;
      FDefs.AddPtr(k, rec);

      if ConfigGrid.RowCount <= k then
      begin
        ConfigGrid.RowCount := k + 1;
      end;
      ConfigGrid.Cells[0, k] := Inttostr(k);
      ConfigGrid.Cells[1, k] := desc;
      ConfigGrid.Cells[2, k] := FloatToStr(miv);
      ConfigGrid.Cells[3, k] := FloatToStr(mav);
      ConfigGrid.Cells[4, k] := FloatToStr(fv);
    end;
  end;
  ShowModal;
  Result := ModalResult = mrOK;
end;

end.

