program ImageHash;

{$SetPeFlags $0020} // LargeAddressAware

{$mode objfpc}{$H+}

uses
  {$if not Declared(UseHeapTrace)}
  ConcMM,
  {$ifend}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils,
  Forms,
  {$IFDEF USE_JPEGTURBO}
  FPReadJPEGTurbo,
  {$ENDIF}
  uFrmMain, uImageHashing, uThreadScanner, uThreadHashing, uThreadClassifier,
  uUtils, uFrmAutoMark, uFrmPathEditor, uNotifier, uFileSearcher;

{$R *.res}
     
{$if Declared(UseHeapTrace)}
var
  heapTrcFile: string;
{$ifend}
begin
  {$if Declared(UseHeapTrace)}
  heapTrcFile:= ExtractFilePath(ParamStr(0)) + 'heap.trc';
  if FileExists(heapTrcFile) then
    DeleteFile(heapTrcFile);
  SetHeapTraceOutput(heapTrcFile);
  {$ifend}

  RequireDerivedFormResource:=True;
  Application.MainFormOnTaskBar:=True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfrmAutoMark, frmAutoMark);
  Application.Run;
end.

