unit uFileSearcher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Masks, LazFileUtils, LazUTF8;

type
  TMyFileIterator = class
  private
    FPath: String;
    FLevel: Integer;
    FFileInfo: TSearchRec;
    FSearching: Boolean;
    function GetFileName: String;
  public
    procedure Stop;
    function IsDirectory: Boolean;
  public
    property FileName: String read GetFileName;
    property FileInfo: TSearchRec read FFileInfo;
    property Level: Integer read FLevel;
    property Path: String read FPath;
    property Searching: Boolean read FSearching;
  end;

  TFileFoundEvent = procedure (FileIterator: TMyFileIterator) of object;
  TDirectoryFoundEvent = procedure (FileIterator: TMyFileIterator) of object;
  TDirectoryEnterEvent = procedure (FileIterator: TMyFileIterator) of object;
  TQueryFileFoundEvent = procedure (FileIterator: TMyFileIterator; const Fn: String; var Accept: Boolean) of object;
  TQueryDirectoryFoundEvent = procedure (FileIterator: TMyFileIterator; const Dir: String; var Accept: Boolean) of object;

  TMyFileSearcher = class(TMyFileIterator)
  private
    FMaskSeparator: char;
    FPathSeparator: char;
    FFollowSymLink: Boolean;
    FOnFileFound: TFileFoundEvent;
    FOnDirectoryFound: TDirectoryFoundEvent;
    FOnDirectoryEnter: TDirectoryEnterEvent;
    FFileAttribute: Word;
    FDirectoryAttribute: Word;
    FOnQueryFileFound: TQueryFileFoundEvent;
    FOnQueryDirectoryEnter: TQueryDirectoryFoundEvent;
    FCircularLinkDetection: Boolean;
    FSearchMask: String;
    FSearchSubDirs: Boolean;
    FCaseSensitive: Boolean;
    procedure RaiseSearchingError;
  protected
    procedure DoDirectoryEnter; virtual;
    procedure DoDirectoryFound; virtual;
    procedure DoFileFound; virtual;
    procedure DoQueryFileFound(const Fn: String; var Accept: Boolean);
    procedure DoQueryDirectoryEnter(const Dir: String; var Accept: Boolean);
    procedure DoSearch(const APath: String; const ALevel: Integer; AMaskList: TMaskList);
    procedure SplitSearchPath(const ASearchPath: String; var SearchDirectories: TStringList);
  public
    constructor Create;
    procedure Search(const ASearchPath: String);
  public
    property MaskSeparator: char read FMaskSeparator write FMaskSeparator;
    property PathSeparator: char read FPathSeparator write FPathSeparator;
    property FollowSymLink: Boolean read FFollowSymLink write FFollowSymLink;
    property SearchMask: String read FSearchMask write FSearchMask;
    property SearchSubDirs: Boolean read FSearchSubDirs write FSearchSubDirs;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property FileAttribute: Word read FFileAttribute write FFileAttribute default faAnyfile;
    property DirectoryAttribute: Word read FDirectoryAttribute write FDirectoryAttribute default faDirectory;
    property OnDirectoryFound: TDirectoryFoundEvent read FOnDirectoryFound write FOnDirectoryFound;
    property OnFileFound: TFileFoundEvent read FOnFileFound write FOnFileFound;
    property OnDirectoryEnter: TDirectoryEnterEvent read FOnDirectoryEnter write FOnDirectoryEnter;
    property OnQueryFileFound: TQueryFileFoundEvent read FOnQueryFileFound write FOnQueryFileFound;
    property OnQueryDirectoryEnter: TQueryDirectoryFoundEvent read FOnQueryDirectoryEnter write FOnQueryDirectoryEnter;
  end;

implementation


{ TMyFileIterator }

function TMyFileIterator.GetFileName: String;
begin
  Result := FPath + FFileInfo.Name;
end;

procedure TMyFileIterator.Stop;
begin
  FSearching := False;
end;

function TMyFileIterator.IsDirectory: Boolean;
begin
  Result := (FFileInfo.Attr and faDirectory) <> 0;
end;

{ TMyFileSearcher }

procedure TMyFileSearcher.RaiseSearchingError;
begin
  raise Exception.Create('The file searcher is already searching!');
end;

procedure TMyFileSearcher.DoDirectoryEnter;
begin
  if Assigned(FonDirectoryEnter) then FOnDirectoryEnter(Self);
end;

procedure TMyFileSearcher.DoDirectoryFound;
begin
  if Assigned(FOnDirectoryFound) then OnDirectoryFound(Self);
end;

procedure TMyFileSearcher.DoFileFound;
begin
  if Assigned(FOnFileFound) then OnFileFound(Self);
end;

procedure TMyFileSearcher.DoQueryFileFound(const Fn: String; var Accept: Boolean);
begin
  if Assigned(FOnQueryFileFound) then
    FOnQueryFileFound(Self, Fn, Accept);
end;

procedure TMyFileSearcher.DoQueryDirectoryEnter(const Dir: String;
  var Accept: Boolean);
begin
  if Assigned(FOnQueryDirectoryEnter) then
    FOnQueryDirectoryEnter(Self, Dir, Accept);
end;

constructor TMyFileSearcher.Create;
begin
  inherited Create;
  FMaskSeparator := ';';
  FPathSeparator := ';';
  FFollowSymLink := True;
  FSearchMask:= '';
  FSearchSubDirs:= True;
  FCaseSensitive:= False;
  FCircularLinkDetection := False;
  FFileAttribute := faAnyFile;
  FDirectoryAttribute := faDirectory;
  FSearching := False;
end;

procedure TMyFileSearcher.DoSearch(const APath: String; const ALevel: Integer; AMaskList: TMaskList);

  function IsSpecialPathName(const Fn: String): Boolean;
  begin
    Result:= (Fn = '') or (Fn = '.') or (Fn = '..');
  end;

  function AcceptFile(const Fn: String): Boolean;
  begin
    Result := True;
    DoQueryFileFound(Fn, Result);
  end;
  function AcceptDir(const Dir: String): Boolean;
  begin
    Result := True;
    DoQueryDirectoryEnter(Dir, Result);
  end;

var
  P, FQDir, LinkTarget: String;
  PathInfo: TSearchRec;
begin        
  FPath := APath;
  FLevel := ALevel;
  P := APath + AllDirectoryEntriesMask;
  // First, process all immediate children
  if FindFirst(P, FileAttribute, PathInfo) = 0 then
    try
      repeat
        // skip special files
        if IsSpecialPathName(PathInfo.Name) then
           Continue;
        // Deal with both files and directories
        if (PathInfo.Attr and faDirectory) = 0 then begin
          // File
          if ((AMaskList = nil) or AMaskList.Matches(PathInfo.Name)) and
             AcceptFile(ExpandFileName(APath+PathInfo.Name)) then begin
            FFileInfo := PathInfo;
            DoFileFound;
          end;
        end else begin
          // Directory
          FFileInfo := PathInfo;
          DoDirectoryFound;
        end;
      until (FindNext(PathInfo) <> 0) or not FSearching;
    finally
      FindClose(PathInfo);
    end;

  // Then descend into subdirs, if allowed
  if FSearchSubDirs or (ALevel > 0) then
    if FindFirst(P, DirectoryAttribute, PathInfo) = 0 then
      try    
        FPath:= APath;
        FLevel:= ALevel;
        repeat
          if ((PathInfo.Attr and faDirectory) = 0) or
             IsSpecialPathName(PathInfo.Name) then
            Continue;

          FQDir:= ExpandFilename(APath + PathInfo.Name);

          if not AcceptDir(FQDir) then
            Continue;

          // deal with symlinks
          if FileIsSymlink(FQDir) then begin
            if not FFollowSymlink then
              Continue;
            LinkTarget := ReadAllLinks(FQDir, False);
            if LinkTarget = '' then //broken link
              Continue;
            LinkTarget := ExpandFileName(LinkTarget);
            if not FilenamesCaseSensitive then
              LinkTarget:= Utf8LowerCase(LinkTarget);

            // Loop detection: if we get back to *where we currently are*, thats 100% a loop.
            // We may be some levels deep in a loop involving multiple links, but this is the first we can
            // safely discover. LinkTarget is always absolute.
            if FileIsInPath(FQDir, LinkTarget) then
              Continue;

            // The link could also point "down" the tree. This broken state can only happen after moving
            // relative links and indicates we probably shouldn't follow either.
            if FileIsInPath(LinkTarget, FQDir) then
              Continue;
          end;

          FFileInfo:= PathInfo;
          if not FSearching then Break;
          DoDirectoryEnter;
          DoSearch(AppendPathDelim(FQDir), Succ(ALevel), AMaskList);
        until (FindNext(PathInfo) <> 0) or not FSearching;
      finally
        FindClose(PathInfo);
      end;
end;

procedure TMyFileSearcher.SplitSearchPath(const ASearchPath: String; var SearchDirectories: TStringList);
var
  Dir, OtherDir: String;
  p1, p2, i: Integer;
begin
  p1:=1;
  while p1<=Length(ASearchPath) do
  begin
    p2:= Pos(FPathSeparator, ASearchPath, p1);
    if p2 < 1 then
      p2:= length(ASearchPath)+1;
    Dir:= ResolveDots(Copy(ASearchPath, p1, p2-p1));
    p1:= p2+1;
    if Dir = '' then continue;
    Dir:= ChompPathDelim(Dir);
    for i:= SearchDirectories.Count-1 downto 0 do begin
      OtherDir:= SearchDirectories[i];
      if (CompareFilenames(Dir, OtherDir)=0) or
        (FSearchSubDirs and (FileIsInPath(Dir, OtherDir))) then begin
        // directory Dir is already searched
        Dir:= '';
        break;
      end;
      if FSearchSubDirs and FileIsInPath(OtherDir, Dir) then
        // directory Dir includes the old directory => delete
        SearchDirectories.Delete(i);
    end;
    if Dir<>'' then
      SearchDirectories.Add(Dir);
  end;
end;

procedure TMyFileSearcher.Search(const ASearchPath: String);
var
  MaskList: TMaskList;
  SearchDirectories: TStringList;
var
  i: Integer;
begin
  if FSearching then RaiseSearchingError;
  {$ifdef windows}
  MaskList:= TWindowsMaskList.Create(FSearchMask, FMaskSeparator, FCaseSensitive);
  {$else}
  MaskList:= TMaskList.Create(FSearchMask, FMaskSeparator, FCaseSensitive);
  {$endif}
  // empty mask = all files mask
  if MaskList.Count = 0 then
    FreeAndNil(MaskList);

  FSearching:= True;
  SearchDirectories:= TStringList.Create;
  try
    SplitSearchPath(ASearchPath, SearchDirectories);
    //Search currentdirectory if ASearchPath = ''
    if (SearchDirectories.Count=0) then
      DoSearch('', 0, MaskList)
    else
    begin
      for i:= 0 to SearchDirectories.Count-1 do
        DoSearch(AppendPathDelim(SearchDirectories[i]), 0, MaskList);
    end;
  finally
    SearchDirectories.Free;
    FSearching:= False;
    MaskList.Free;
  end;
end;

end.

