unit uThreadScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type
  TFileScannerThread = class(TThread)
  private
    fPath: string;
    fFilter: string;
    fList: TStringList;
  protected
    procedure Execute; override;
    procedure FileFoundEvent(FileIterator: TFileIterator);
  public
    constructor Create;
    destructor Destroy; override;
    property Path: string read fPath write fPath;
    property Filter: string read fFilter write fFilter;
    property List: TStringList read fList;
  end;

implementation

{ TFileScannerThread }

constructor TFileScannerThread.Create;
begin
  inherited Create(true);
  FreeOnTerminate:= false;
  fPath:= '';
  fFilter:= '*.*';
  fList:= TStringList.Create;
end;

destructor TFileScannerThread.Destroy;
begin
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure TFileScannerThread.Execute;
var
  Searcher: TFileSearcher;
begin
  fPath:= IncludeTrailingPathDelimiter(fPath);
  Searcher := TFileSearcher.Create;
  try
    Searcher.OnFileFound:= @FileFoundEvent;
    Searcher.DirectoryAttribute := faDirectory;
    Searcher.Search(fPath, fFilter, true);
  finally
    Searcher.Free;
  end;
  fList.Sort;
end;

procedure TFileScannerThread.FileFoundEvent(FileIterator: TFileIterator);
var
  s: string;
begin
  s:= Copy(FileIterator.FileName, Length(fPath) + 1, MaxInt);
  fList.Add(s);
end;

end.

