program ImageHash;

{$SetPeFlags $0020} // LargeAddressAware

{$mode objfpc}{$H+}

uses
  //FastMM4,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils,
  Forms,
  {$IFDEF USE_JPEGTURBO}
  {$ENDIF}
  uFrmMain, uImageHashing, uThreadScanner, uThreadHashing, uThreadClassifier,
  uUtils, uFrmAutoMark, FPReadJPEGTurbo;

{$R *.res}

begin
  {$if Declared(UseHeapTrace)}
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ifend}

  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmAutoMark, frmAutoMark);
  Application.Run;
end.

