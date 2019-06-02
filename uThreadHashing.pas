unit uThreadHashing;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, syncobjs, uImageHashing, Graphics, IntfGraphics;

type
  TImageMark = (imUnmarked, imDelete, imIgnore);
  PImageInfoItem = ^TImageInfoItem;
  TImageInfoItem = record
    Status: Longint;
    Sourcedir: integer;
    Filename: string;
    Error: string;
    Hash00, Hash90, Hash180: QWord;
    Thumbnail: TLazIntfImage;
    ImgW, ImgH: integer;
    Mark: TImageMark;
    procedure Init;
    procedure Done;
    function FullName(Dirs: TStrings): String;
  end;

  TImageInfoList = array of TImageInfoItem;

  PImageInfoList = ^TImageInfoItem;

  TImageFinishEvent = procedure of object;

  TImageHashThread = class(TThread)
  private
    fCount: integer;
    fList: PImageInfoList;
    fPrefixes: TStrings;
    fImageFinishEvent: TImageFinishEvent;
    fThumbSize: integer;
    fSawMemoryError: boolean;
    fFinalMemoryError: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: integer read fCount write fCount;
    property List: PImageInfoList read fList write fList;
    property Prefixes: TStrings read fPrefixes write fPrefixes;
    property ThumbSize: integer read fThumbSize write fThumbSize;
    property FinalMemoryError: boolean read fFinalMemoryError write fFinalMemoryError;

    property SawMemoryError: boolean read fSawMemoryError;
    property OnImageFinish: TImageFinishEvent read fImageFinishEvent write fImageFinishEvent;
  end;

const
  STATUS_NONE = 0;
  STATUS_WORKING = 1;
  STATUS_DONE = 2;

implementation

{ TImageInfoItem }

procedure TImageInfoItem.Init;
begin
  Status:= STATUS_NONE;
  Sourcedir:= 0;
  Filename:= '';
  Error:= '';
  Hash00:= 0;
  Hash90:= 0;
  Hash180:= 0;
  Thumbnail:= nil;
  ImgW:= 0;
  ImgH:= 0;
  Mark:= imUnmarked;
end;

procedure TImageInfoItem.Done;
begin
  FreeAndNil(Thumbnail);
  Filename:= '';
end;

function TImageInfoItem.FullName(Dirs: TStrings): String;
begin
  Result:= ConcatPaths([Dirs[Self.Sourcedir], Self.Filename]);
end;

{ TImageHashThread }

constructor TImageHashThread.Create;
begin
  Priority:= tpLower;
  inherited Create(true);
  fList:= nil;
  fCount:= 0;
  fThumbSize:= 48;
  fSawMemoryError:= false;
  fFinalMemoryError:= false;
end;

destructor TImageHashThread.Destroy;
begin
  inherited Destroy;
end;

procedure TImageHashThread.Execute;
var
  cursor, hbits: integer;
  im: PImageInfoItem;
  fn: String;
begin
  cursor:= 0;
  while not Terminated and (cursor < fCount) do begin
    if InterlockedCompareExchange(fList[cursor].Status, STATUS_WORKING, STATUS_NONE) = STATUS_NONE then begin
      // we got this item, work on it
      im:= @fList[Cursor];
      fn:= IncludeTrailingPathDelimiter(fPrefixes[im^.Sourcedir]) + im^.Filename;
      try
        hashFromFileName(fn, sizeof(QWord), @im^.Hash00, @im^.Hash90, @im^.Hash180, hbits, fThumbSize, @im^.thumbnail, @im^.ImgW, @im^.ImgH);
      except
        // ignore for now, and keep "working" mark on it unless this is second pass
        on e: EOutOfMemory do begin
          fSawMemoryError:= true;
          if not fFinalMemoryError then
            continue;
        end;
        // eat exception, but keep note of it
        on e: Exception do
          im^.Error:= e.Message;
      end;
      InterLockedExchange(im^.Status, STATUS_DONE);
      if Assigned(fImageFinishEvent) then
        Queue(fImageFinishEvent);
    end else
      // another thread either is working on it or already finished
      inc(cursor);
  end;
end;

end.

