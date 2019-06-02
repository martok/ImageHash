unit FPReadJPEGTurbo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, ctypes, turbojpeg, GraphType;

type
  { TFPReaderJPEGTurbo }
  { This is a FPImage reader for jpeg images. }

  TJPEGScale = (jsFullSize, jsHalf, jsQuarter, jsEighth);
  TJPEGReadPerformance = (jpBestQuality, jpBestSpeed);

  TFPReaderJPEGTurbo = class(TFPCustomImageReader)
  private
    FSmoothing: boolean;
    FWidth: Integer;
    FHeight: Integer;
    FGrayscale: boolean;
    FScale: TJPEGScale;
    FPerformance: TJPEGReadPerformance;
    procedure SetPerformance(const AValue: TJPEGReadPerformance);
    procedure SetSmoothing(const AValue: boolean);
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): boolean; override;
    class function InternalSize(Str: TStream): TPoint; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property GrayScale: boolean read FGrayscale;
    property Scale: TJPEGScale read FScale write FScale;
  end;

implementation

uses
  IntfGraphics;

{ TFPReaderJPEGTurbo }

procedure TFPReaderJPEGTurbo.SetSmoothing(const AValue: boolean);
begin
  if FSmoothing=AValue then exit;
  FSmoothing:=AValue;
end;

procedure TFPReaderJPEGTurbo.SetPerformance(const AValue: TJPEGReadPerformance);
begin
  if FPerformance=AValue then exit;
  FPerformance:=AValue;
end;

procedure TFPReaderJPEGTurbo.InternalRead(Str: TStream; Img: TFPCustomImage);
type
  TRGBAQuad = record
    Blue:  Byte;
    Green: Byte;
    Red:   Byte;
    Alpha: Byte;
  end;
  PRGBAQuad = ^TRGBAQuad;
const
  ScaleFactors: array[TJPEGScale] of tjscalingfactor = (
    (num:1; denom:1),
    (num:1; denom:2),
    (num:1; denom:4),
    (num:1; denom:8)
  );
var
  MemStream: TMemoryStream;
  jpegDecompressor: tjhandle;
  Continue: Boolean;
  scFact: tjscalingfactor;

  procedure ReadHeader;
  var
    jpegWidth: cint;
    jpegHeight: cint;
    jpegSubsamp, jpegColorspace: cint;
  begin
    tjDecompressHeader3(jpegDecompressor, MemStream.Memory,MemStream.Size, @jpegWidth, @jpegHeight, @jpegSubsamp, @jpegColorspace);
    FWidth := jpegWidth;
    FHeight := jpegHeight;
    FGrayscale := jpegColorspace = cint(TJCS_GRAY);
  end;

  procedure ReadPixelsGeneric;
  var
    outData: TMemoryStream;
    y, x: Integer;
    lColor: TFPColor;
    mp: PRGBAQuad;
  begin
    outData:= TMemoryStream.Create;
    try
      outData.SetSize(FWidth*FHeight*sizeof(TRGBAQuad));
      tjDecompress2(jpegDecompressor, MemStream.Memory, MemStream.Size, outData.Memory, FWidth, 0, FHeight, cint(TJPF_BGRX), TJFLAG_FASTDCT);
      mp:= outData.Memory;
      for y := 0 to FHeight - 1 do begin
        for x := 0 to FWidth - 1 do begin
          lColor.alpha:= alphaOpaque;
          lColor.blue:= mp^.Blue or mp^.Blue shl 8;
          lColor.green:= mp^.Green or mp^.Green shl 8;
          lColor.red:= mp^.Red or mp^.Red shl 8;
          Img.Colors[x, y] := lColor;
          inc(mp);
        end;
      end;
    finally
      FreeAndNil(outData);
    end;
  end;

  procedure ReadPixelsLazIntfImage;
  var
    li: TLazIntfImage;
    fmt: TJPF;
    rd: TRawImageDescription;
    lo: cint;
  begin
    fmt:= TJPF(-1);
    li:= TLazIntfImage(Img);
    rd:= li.DataDescription;
    if (rd.Format = ricfRGBA) then begin
      if rd.Depth = 24 then begin
        if rd.RedShift=0 then
          fmt:= TJPF_RGB
        else
          fmt:= TJPF_BGR;
      end else
      if rd.Depth = 32 then begin
        if rd.RedShift=0 then
          fmt:= TJPF_RGBA
        else
          fmt:= TJPF_BGRA;
      end;
      if rd.LineOrder = riloTopToBottom then
        lo:= 0
      else
        lo:= TJFLAG_BOTTOMUP;
    end;
    if cint(fmt) >= 0 then begin
      //FillChar(li.PixelData^, rd.BytesPerLine*FHeight, 0);
      tjDecompress2(jpegDecompressor, MemStream.Memory, MemStream.Size, li.PixelData, FWidth, rd.BytesPerLine, FHeight, cint(fmt), TJFLAG_FASTDCT or lo)
    end
    else
      ReadPixelsGeneric;
  end;

begin
  FWidth:=0;
  FHeight:=0;
  MemStream:=nil;
  try
    if Str is TMemoryStream then
      MemStream:=TMemoryStream(Str)
    else begin
      MemStream:=TMemoryStream.Create;
      MemStream.CopyFrom(Str, 0);
      MemStream.Position:=0;
    end;
    if MemStream.Size > 0 then begin
      jpegDecompressor := tjInitDecompress();
      try
        ReadHeader;
        Continue:= true;
        Progress(psStarting, 0, False, Rect(0,0,0,0), '', Continue);
        if not Continue then
          exit;
        scFact:= ScaleFactors[FScale];
        FWidth:= TJSCALED(FWidth, scFact);
        FHeight:= TJSCALED(FHeight, scFact);
        Img.SetSize(FWidth, FHeight);
        if Img is TLazIntfImage then
          ReadPixelsLazIntfImage
        else
          ReadPixelsGeneric;
        Progress(psEnding, 100, false, Rect(0,0,0,0), '', Continue);
      finally
        tjDestroy(jpegDecompressor);
      end;
    end;
  finally
    if (MemStream<>nil) and (MemStream<>Str) then
      MemStream.Free;
  end;
end;

class function TFPReaderJPEGTurbo.InternalSize(Str: TStream): TPoint;
var
  jpegDecompressor: tjhandle;
  jpegWidth: cint;
  jpegHeight: cint;
  jpegSubsamp, jpegColorspace: cint;
  pHeader: Pointer;
  hSize: LongInt;
begin
  jpegDecompressor := tjInitDecompress();
  try
    GetMem(pHeader, 16*1024);
    try
      hSize:= Str.Read(pHeader^, 16*1024);
      tjDecompressHeader3(jpegDecompressor, pHeader, hSize, @jpegWidth, @jpegHeight, @jpegSubsamp, @jpegColorspace);
      Result.X:= jpegWidth;
      Result.Y:= jpegHeight
    finally
      Freemem(pHeader);
    end;
  finally
    tjDestroy(jpegDecompressor);
  end;
end;

function TFPReaderJPEGTurbo.InternalCheck(Str: TStream): boolean;
var
  Buf: array[0..1] of Byte = (0, 0);
  p: Int64;
begin
  if Str=nil then exit(false);
  p:=Str.Position;
  Result := (Str.Read(Buf, 2)=2) and (Buf[0]=$FF) and (Buf[1]=$D8); // byte sequence FFD8 = start of image
  Str.Position:=p;
end;

constructor TFPReaderJPEGTurbo.Create;
begin
  inherited Create;
  FScale:= jsFullSize;
end;

destructor TFPReaderJPEGTurbo.Destroy;
begin
  inherited Destroy;
end;

initialization
  ImageHandlers.UnregisterImageHandlers('JPEG Graphics', true, false);
  ImageHandlers.RegisterImageReader('JPEG Graphics (Turbo)', 'jpg;jpeg', TFPReaderJPEGTurbo);
end.
