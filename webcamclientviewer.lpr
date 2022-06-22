program webcamclientviewer;

{$mode objfpc}{$H+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  cmem,
  {$ENDIF}{$ENDIF}
  sysutils, FileUtil,
  Interfaces, // this includes the LCL widgetset
  Forms, webcammain, configdlg, streamwin
  { you can add units after this };

{$R *.res}


{$IFDEF DEBUG}
const cHeapTraceFile = 'heap.trc';
{$ENDIF}

begin
  {$IFDEF DEBUG}
  if FileExists(cHeapTraceFile) then
    DeleteFile(cHeapTraceFile);
  SetHeapTraceOutput(cHeapTraceFile);
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TConfDlg, ConfDlg);
  Application.Run;
end.

