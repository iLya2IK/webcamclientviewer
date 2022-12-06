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
    FFormat : String;
  public
    procedure UpdateFrame(aFrame : TCustomMemoryStream; rmStrm : Boolean = true);
    property Data : TObject read FData write FData;
    property SubFormat : string read FFormat write FFormat;
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
  png : TPortableNetworkGraphic;
begin
  if Assigned(aFrame) and
     (aFrame.Size > 6) then
  begin
    Inc(FUpdates);
    Label1.Caption := Inttostr(FUpdates);
    aFrame.Position := 6;

    try
      if (Length(SubFormat) = 0) or (pos('_JPEG', SubFormat) > 0) then
      begin
        Jpg := TJPEGImage.Create;
        try
          Jpg.LoadFromStream(aFrame);
          Image1.Picture.Jpeg := Jpg;
        finally
          Jpg.Free;
        end;
      end else
      if pos('_PNG', SubFormat) > 0 then
      begin
        png := TPortableNetworkGraphic.Create;
        try
          png.LoadFromStream(aFrame);
          Image1.Picture.PNG := png;
        finally
          png.Free;
        end;
      end;
    finally
      if rmStrm then  aFrame.Free;
    end;
  end;
end;

end.

