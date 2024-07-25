unit uCanvasInterpolation;

// This Unit is a 1:1 copy of the same classes in FPCanvas, but optimized slightly.

{$mode ObjFPC}{$H+}
{$Optimization LEVEL4}
{$Optimization FASTMATH}
{$Optimization REGVAR}
{$Q-}{$R-}

interface

uses
  Classes, SysUtils, FPImage, FPCanvas;

type
  { TFPBaseInterpolationOpt }

  TFPBaseInterpolationOpt = class (TFPCustomInterpolation)
  private
    procedure CreatePixelWeights (OldSize, NewSize: integer;
      out Entries: Pointer; out EntrySize: integer; out Support: integer);
  protected
    procedure Execute (x,y,w,h : integer); override;
    function Filter (x : double): double; virtual;
    function MaxSupport : double; virtual;
  end;

  { TMitchelInterpolationOpt }

  TMitchelInterpolationOpt = class (TFPBaseInterpolationOpt)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

implementation

uses
  Math;


{ TFPBaseInterpolationOpt }

procedure TFPBaseInterpolationOpt.CreatePixelWeights(OldSize, NewSize: integer;
  out Entries: Pointer; out EntrySize: integer; out Support: integer);
// create an array of #NewSize entries. Each entry starts with an integer
// for the StartIndex, followed by #Support singles for the pixel weights.
// The sum of weights for each entry is 1.
var
  Entry: Pointer;

  procedure SetSupport(NewSupport: integer);
  begin
    Support:=NewSupport;
    EntrySize:=SizeOf(integer)+SizeOf(Single)*Support;
    Getmem(Entries,EntrySize*NewSize);
    Entry:= Entries;
  end;

var
  i: Integer;
  Factor: double;
  StartPos: Double;
  StartIndex: Integer;
  j: Integer;
  FirstValue: Double;
  //Sum: double;
begin
  if NewSize=OldSize then
  begin
    SetSupport(1);
    for i:=0 to NewSize-1 do
    begin
      // 1:1
      PInteger(Entry)^:=i;
      inc(Entry,SizeOf(Integer));
      PSingle(Entry)^:=1.0;
      inc(Entry,SizeOf(Single));
    end;
  end else if NewSize<OldSize then
  begin
    // shrink
    SetSupport(Max(2,(OldSize+NewSize-1) div NewSize));
    Factor:=double(OldSize)/double(NewSize);
    for i:=0 to NewSize-1 do
    begin
      StartPos:=Factor*i;
      StartIndex:=Floor(StartPos);
      PInteger(Entry)^:=StartIndex;
      inc(Entry,SizeOf(Integer));
      // first pixel
      FirstValue:=(1.0-(StartPos-double(StartIndex)));
      PSingle(Entry)^:=FirstValue/Factor;
      inc(Entry,SizeOf(Single));
      // middle pixel
      for j:=1 to Support-2 do
      begin
        PSingle(Entry)^:=1.0/Factor;
        inc(Entry,SizeOf(Single));
      end;
      // last pixel
      PSingle(Entry)^:=(Factor-FirstValue-(Support-2))/Factor;
      inc(Entry,SizeOf(Single));
    end;
  end else
  begin
    // enlarge
    if OldSize=1 then
    begin
      SetSupport(1);
      for i:=0 to NewSize-1 do
      begin
        // nothing to interpolate
        PInteger(Entry)^:=0;
        inc(Entry,SizeOf(Integer));
        PSingle(Entry)^:=1.0;
        inc(Entry,SizeOf(Single));
      end;
    end else
    begin
      SetSupport(2);
      Factor:=double(OldSize-1)/double(NewSize);
      for i:=0 to NewSize-1 do
      begin
        StartPos:=Factor*i+Factor/2;
        StartIndex:=Floor(StartPos);
        PInteger(Entry)^:=StartIndex;
        inc(Entry,SizeOf(Integer));
        // first pixel
        FirstValue:=(1.0-(StartPos-double(StartIndex)));
        // convert linear distribution
        FirstValue:=Min(1.0,Max(0.0,Filter(FirstValue/MaxSupport)));
        PSingle(Entry)^:=FirstValue;
        inc(Entry,SizeOf(Single));
        // last pixel
        PSingle(Entry)^:=1.0-FirstValue;
        inc(Entry,SizeOf(Single));
      end;
    end;
  end;
  if Entry<>Entries+EntrySize*NewSize then
    raise Exception.Create('TFPBase2Interpolation.Execute inconsistency');
end;

procedure TFPBaseInterpolationOpt.Execute(x, y, w, h: integer);
// paint Image on Canvas at x,y,w*h
var
  dy: Integer;
  dx: Integer;
  HorzResized: PFPColor;
  xEntries: Pointer;
  xEntrySize: integer;
  xSupport: integer;// how many horizontal pixel are needed to create one pixel
  yEntries: Pointer;
  yEntrySize: integer;
  ySupport: integer;// how many vertical pixel are needed to create one pixel
  NewSupportLines: LongInt;
  yEntry: Pointer;
  SrcStartY: LongInt;
  LastSrcStartY: LongInt;
  sy: Integer;
  xEntry: Pointer;
  sx: LongInt;
  cx: Integer;
  f, r, g, b, a: Single;
  Col: TFPColor;
  CurEntry: Pointer;
begin
  if (w<=0) or (h<=0) or (image.Width=0) or (image.Height=0) then
    exit;

  xEntries:=nil;
  yEntries:=nil;
  HorzResized:=nil;
  try
    CreatePixelWeights(image.Width,w,xEntries,xEntrySize,xSupport);
    CreatePixelWeights(image.Height,h,yEntries,yEntrySize,ySupport);
    // create temporary buffer for the horizontally resized pixel for the
    // current y line
    GetMem(HorzResized,w*ySupport*SizeOf(TFPColor));

    SrcStartY:=0;
    for dy:=0 to h-1 do
    begin
      if dy=0 then
      begin
        yEntry:=yEntries;
        SrcStartY:=PInteger(yEntry)^;
        NewSupportLines:=ySupport;
      end else
      begin
        LastSrcStartY:=SrcStartY;
        inc(yEntry,yEntrySize);
        SrcStartY:=PInteger(yEntry)^;
        NewSupportLines:=SrcStartY-LastSrcStartY;
        // move lines up
        if (NewSupportLines>0) and (ySupport>NewSupportLines) then
          System.Move(HorzResized[NewSupportLines*w],
                      HorzResized[0],
                      (ySupport-NewSupportLines)*w*SizeOf(TFPColor));
      end;

      // compute new horizontally resized line(s)
      for sy:=ySupport-NewSupportLines to ySupport-1 do
      begin
        xEntry:=xEntries;
        for dx:=0 to w-1 do
        begin
          sx:=PInteger(xEntry)^;
          inc(xEntry,SizeOf(integer));
          r:= 0;
          g:= 0;
          b:= 0;
          a:= 0;
          for cx:=0 to xSupport-1 do
          begin
            f:=PSingle(xEntry)^;
            inc(xEntry,SizeOf(Single));
            Col:=image.Colors[sx+cx,SrcStartY+sy];
            r += Col.red*f;    
            g += Col.green*f;
            b += Col.blue*f;
            a += Col.alpha*f;
          end;  
          Col.red:=Min(round(r),$ffff);
          Col.green:=Min(round(g),$ffff);
          Col.blue:=Min(round(b),$ffff);
          Col.alpha:=Min(round(a),$ffff);
          HorzResized[dx+sy*w]:= Col;
        end;
      end;

      // compute new vertically resized line
      for dx:=0 to w-1 do
      begin
        CurEntry:=yEntry+SizeOf(integer);
        r:= 0;
        g:= 0;
        b:= 0;
        a:= 0;
        for sy:=0 to ySupport-1 do
        begin
          f:=PSingle(CurEntry)^;
          inc(CurEntry,SizeOf(Single));
          Col:=HorzResized[dx+sy*w];
          r += Col.red*f;
          g += Col.green*f;
          b += Col.blue*f;
          a += Col.alpha*f;
        end;                               
        Col.red:=Min(round(r),$ffff);
        Col.green:=Min(round(g),$ffff);
        Col.blue:=Min(round(b),$ffff);
        Col.alpha:=Min(round(a),$ffff);
        Canvas.DrawPixel(x+dx,y+dy, Col);
      end;
    end;
  finally
    if xEntries<>nil then FreeMem(xEntries);
    if yEntries<>nil then FreeMem(yEntries);
    if HorzResized<>nil then FreeMem(HorzResized);
  end;
end;

function TFPBaseInterpolationOpt.Filter(x: double): double;
begin
  Result:=x;
end;

function TFPBaseInterpolationOpt.MaxSupport: double;
begin
  Result:=1.0;
end;

{ TMitchelInterpolationOpt }

function TMitchelInterpolationOpt.Filter(x: double): double;
const
  B  = (1.0/3.0);
  C  = (1.0/3.0);
  P0 = ((  6.0- 2.0*B       )/6.0);
  P2 = ((-18.0+12.0*B+ 6.0*C)/6.0);
  P3 = (( 12.0- 9.0*B- 6.0*C)/6.0);
  Q0 = ((       8.0*B+24.0*C)/6.0);
  Q1 = ((     -12.0*B-48.0*C)/6.0);
  Q2 = ((       6.0*B+30.0*C)/6.0);
  Q3 = ((     - 1.0*B- 6.0*C)/6.0);
begin
  if (x < -2.0) then
    result := 0.0
  else if (x < -1.0) then
    result := Q0-x*(Q1-x*(Q2-x*Q3))
  else if (x < 0.0) then
    result := P0+x*x*(P2-x*P3)
  else if (x < 1.0) then
    result := P0+x*x*(P2+x*P3)
  else if (x < 2.0) then
    result := Q0+x*(Q1+x*(Q2+x*Q3))
  else
  result := 0.0;
end;

function TMitchelInterpolationOpt.MaxSupport: double;
begin
  result := 2.0;
end;

end.

