unit streamwin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TStreamFrm }

  TStreamFrm = class(TForm)
    Image1 : TImage;
    Label1 : TLabel;
    procedure FormCreate(Sender : TObject);
  private
    FData : TObject;
    FUpdates : Integer;
  public
    procedure UpdateFrame(aFrame : TCustomMemoryStream; rmStrm : Boolean = true);
    property Data : TObject read FData write FData;
  end;

var
  StreamFrm : TStreamFrm;

implementation

{$R *.lfm}

{ TStreamFrm }

procedure TStreamFrm.FormCreate(Sender : TObject);
begin
  FUpdates := 0;
end;

procedure TStreamFrm.UpdateFrame(aFrame : TCustomMemoryStream; rmStrm : Boolean
  );
var
  jpg : TJPEGImage;
begin
  if Assigned(aFrame) and
     (aFrame.Size > 6) then
  begin
    Inc(FUpdates);
    Label1.Caption := Inttostr(FUpdates);
    aFrame.Position := 6;
    Jpg := TJPEGImage.Create;
    try
      Jpg.LoadFromStream(aFrame);
      Image1.Picture.Jpeg := Jpg;
    finally
      Jpg.Free;
      if rmStrm then  aFrame.Free;
    end;
  end;
end;

end.

