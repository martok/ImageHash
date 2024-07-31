unit uUtils;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, fgl;

type
  PThread = ^TThread;

procedure WaitForMultipleThreads(a: PThread; NThreads: Integer; CB: TProcedureOfObject; cancelflag: PBoolean = nil);


type
  generic TListTool<T> = record
  type
    TAList = specialize TFPGList<T>;   
    TAArray = array of T;
    TACompareFunc = function(const Item1, Item2: T): Integer;
    TACompareNest = function(const Item1, Item2: T): Integer is nested;

    class function FindSmallestValue(AList: TAList; ACompare: TACompareFunc; out Value: T): integer; static; overload;
    class function FindSmallestValue(AList: TAList; ACompare: TACompareNest; out Value: T): integer; static; overload; 
    class function FindSmallestValue(AArray: TAArray; ACompare: TACompareFunc; out Value: T): integer; static; overload;
    class function FindSmallestValue(AArray: TAArray; ACompare: TACompareNest; out Value: T): integer; static; overload;
  end;

function GetFileInfos(AFileName: string; out AInfos: TSearchRec): boolean;

function SendFilesToTrash(AFileNames: TStringArray): boolean;
       
function GetBitCount32(const Num: DWord): NativeUInt; inline;
function GetBitCount64(const Num: QWord): NativeUInt; inline;

implementation

uses
  ShellApi;

procedure WaitForMultipleThreads(a: PThread; NThreads: Integer; CB: TProcedureOfObject; cancelflag: PBoolean = nil);
var
  i: Integer;
begin
  for i:= 0 to NThreads-1 do begin
    repeat
      if Assigned(cancelflag) and cancelflag^ then
        a[i].Terminate;
      if Assigned(CB) then
        CB();
      Sleep(1);
    until a[i].Finished;
  end;
  // Make sure any queued events are processed before returning to where the threads may be freed
  // This should be done by the callback function
  if GetCurrentThreadId = MainThreadID then begin
    if Assigned(CB) then
      CB();
  end;
end;

{ TListTool }

class function TListTool.FindSmallestValue(AList: TAList; ACompare: TACompareFunc; out Value: T): integer;
  function Comp(const Item1, Item2: T): Integer;
  begin
    Result:= ACompare(Item1, Item2);
  end;

begin
  Result:= FindSmallestValue(AList, @Comp, Value);
end;

class function TListTool.FindSmallestValue(AList: TAList; ACompare: TACompareNest; out Value: T): integer;
var
  i: integer;
begin
  if AList.Count = 0 then
    Exit(-1);

  Value:= AList[0];
  Result:= 0;

  for i:= 1 to AList.Count - 1 do begin
    if ACompare(AList[i], Value) < 0 then begin
      Result:= i;
      Value:= AList[i];
    end;
  end;
end;

class function TListTool.FindSmallestValue(AArray: TAArray; ACompare: TACompareFunc; out Value: T): integer;
  function Comp(const Item1, Item2: T): Integer;
  begin
    Result:= ACompare(Item1, Item2);
  end;

begin
  Result:= FindSmallestValue(AArray, @Comp, Value);
end;

class function TListTool.FindSmallestValue(AArray: TAArray; ACompare: TACompareNest; out Value: T): integer;
var
  i: integer;
begin
  if Length(AArray) = 0 then
    Exit(-1);

  Value:= AArray[0];
  Result:= 0;

  for i:= 1 to high(AArray) do begin
    if ACompare(AArray[i], Value) < 0 then begin
      Result:= i;
      Value:= AArray[i];
    end;
  end;
end;

function GetFileInfos(AFileName: string; out AInfos: TSearchRec): boolean;
begin
  Result:= FindFirst(AFileName, faAnyFile, AInfos) = 0;
  if Result then
    FindClose(AInfos);
end;

function SendFilesToTrash(AFileNames: TStringArray): boolean;
var
  fop: TSHFILEOPSTRUCTW;
  ws: WideString;
begin
  fop.wnd:= 0;
  fop.wFunc:= FO_DELETE;
  fop.fFlags:= FOF_NOCONFIRMATION or FOF_ALLOWUNDO;
  ws:= WideString(AnsiString.Join(#0, AFileNames) + #0);
  fop.pFrom:= PWideChar(ws);
  Result:= 0 = SHFileOperationW(@fop);
end;


{$AsmMode intel}
function GetBitCount32(const Num: DWord): NativeUInt; inline; assembler; nostackframe;
asm
  POPCNT @result, num
end;

{$IFDEF CPUX86}  
function GetBitCount64(const Num: QWord): NativeUInt; inline;
begin
  Result:= GetBitCount32(Lo(Num)) + GetBitCount32(Hi(Num));
end;

{$ELSE}

function GetBitCount64(const Num: QWord): NativeUInt; inline; assembler; nostackframe;
asm
  POPCNT @result, num
end;

{$ENDIF}

end.

