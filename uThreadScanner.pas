unit uThreadScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFileSearcher;

type
  TFileScannerThread = class(TThread)
  private
    fPath: string;
    fFilter: string;
    fList: TStringList;
    fSubDirs: boolean;
  protected
    procedure Execute; override;
    procedure FileFoundEvent(FileIterator: TMyFileIterator);
  public
    constructor Create;
    destructor Destroy; override;
    property Path: string read fPath write fPath;
    property Filter: string read fFilter write fFilter;
    property SubDirs: boolean read fSubDirs write fSubDirs;
    property List: TStringList read fList;
  end;

implementation

{ TFileScannerThread }

constructor TFileScannerThread.Create;
begin
  inherited Create(true);
  FreeOnTerminate:= false;
  fPath:= '';
  fSubDirs:= true;
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
  Searcher: TMyFileSearcher;
begin
  fPath:= IncludeTrailingPathDelimiter(fPath);
  Searcher := TMyFileSearcher.Create;
  try
    Searcher.OnFileFound:= @FileFoundEvent;
    Searcher.DirectoryAttribute := faDirectory;
    Searcher.SearchMask:= fFilter;
    Searcher.SearchSubDirs:= fSubDirs;
    Searcher.Search(fPath);
  finally
    Searcher.Free;
  end;
  fList.Sort;
end;

procedure TFileScannerThread.FileFoundEvent(FileIterator: TMyFileIterator);
var
  s: string;
begin
  s:= Copy(FileIterator.FileName, Length(fPath) + 1, MaxInt);
  fList.Add(s);
end;

end.

