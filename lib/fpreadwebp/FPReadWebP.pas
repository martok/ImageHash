unit FPReadWebP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, ctypes, WebPDec, GraphType;

type
  { TFPReaderWebP }
  { This is a FPImage reader for webp still images. }

  TFPReaderWebP = class(TFPCustomImageReader)
  private
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): boolean; override;
    class function InternalSize(Str: TStream): TPoint; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  IntfGraphics;

{ TFPReaderWebP }

procedure TFPReaderWebP.InternalRead(Str: TStream; Img: TFPCustomImage);
type
  TColorOrder = (coRGB, coBGR, coRGBA, coBGRA);
  TRGBAQuad = record  
    Red:   Byte;
    Green: Byte; 
    Blue:  Byte;
    Alpha: Byte;
  end;
  PRGBAQuad = ^TRGBAQuad;
var
  MemStream: TMemoryStream;
  Continue: boolean;
  outPixels: PByte;
  ow, oh: Integer;

  procedure ReadPixelsGeneric;
  var
    y, x: Integer;
    lColor: TFPColor;
    mp: PRGBAQuad;
  begin
    mp:= PRGBAQuad(outPixels);
    for y := 0 to oh - 1 do begin
      for x := 0 to ow - 1 do begin
        lColor.alpha:= mp^.Alpha or mp^.Alpha shl 8;
        lColor.blue:= mp^.Blue or mp^.Blue shl 8;
        lColor.green:= mp^.Green or mp^.Green shl 8;
        lColor.red:= mp^.Red or mp^.Red shl 8;
        Img.Colors[x, y] := lColor;
        inc(mp);
      end;
    end;
  end;

  function GetColorOrder(desc: TRawImageDescription): TColorOrder;
  begin
    if (desc.Format = ricfRGBA) then begin
      if desc.Depth = 24 then begin
        if desc.RedShift=0 then
          Exit(coRGB)
        else
          Exit(coBGR);
      end else
      if desc.Depth = 32 then begin
        if desc.RedShift=0 then
          Exit(coRGB)
        else
          Exit(coBGR);
      end;
    end;
    raise FPImageException.Create('Bad Img target format');
  end;

  procedure ReadPixelsLazIntfImage;
  var
    li: TLazIntfImage;
    rd: TRawImageDescription;
    srcLine, dstLine: PByte;
    order: TColorOrder;
    bpl: PtrUInt;
    y, x: Integer;
  begin
    li:= TLazIntfImage(Img);
    rd:= li.DataDescription;
    order:= GetColorOrder(rd);
    bpl:= rd.BytesPerLine;
    for y:= 0 to oh - 1 do begin
      srcLine:= outPixels + Y * ow * 4; // src is alawys RGBA
      dstLine:= li.GetDataLineStart(Y);
      case order of
        coRGB: 
          for x:= 0 to ow - 1 do begin
            dstLine[0]:= srcLine[0];
            dstLine[1]:= srcLine[1];
            dstLine[2]:= srcLine[2];
            inc(dstLine, 3);
            inc(srcLine, 4);
          end;
        coBGR:  
          for x:= 0 to ow - 1 do begin
            dstLine[0]:= srcLine[2];
            dstLine[1]:= srcLine[1];
            dstLine[2]:= srcLine[0];
            inc(dstLine, 3);
            inc(srcLine, 4);
          end;
        coRGBA:
          Move(srcLine, dstLine, bpl);
        coBGRA:
          for x:= 0 to ow - 1 do begin
            dstLine[0]:= srcLine[2];
            dstLine[1]:= srcLine[1];
            dstLine[2]:= srcLine[0];
            dstLine[3]:= srcLine[3];
            inc(dstLine, 4);
            inc(srcLine, 4);
          end;
      end;
    end;
  end;

begin
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
      Continue:= true;
      Progress(psStarting, 0, False, Rect(0,0,0,0), '', Continue);
      if not Continue then
        exit;
      outPixels:= WebPDecodeRGBA(MemStream.Memory, MemStream.Size, ow, oh);
      if not Assigned(outPixels) then
        raise FPImageException.Create('WebP Decode failed');
      try
        Img.SetSize(ow, oh);
        if Img is TLazIntfImage then
          ReadPixelsLazIntfImage
        else
          ReadPixelsGeneric;
      finally
        Freemem(outPixels);
      end;
      Progress(psEnding, 100, false, Rect(0,0,0,0), '', Continue);
    end;
  finally
    if (MemStream<>nil) and (MemStream<>Str) then
      MemStream.Free;
  end;
end;

class function TFPReaderWebP.InternalSize(Str: TStream): TPoint;
var
  webpWidth: cint;
  webpHeight: cint;
  pHeader: Pointer;
  hSize: LongInt;
begin
  Result:= inherited;
  GetMem(pHeader, 16*1024);
  try
    hSize:= Str.Read(pHeader^, 16*1024);
    if WebPGetInfo(pHeader, hSize, webpWidth, webpHeight) then begin
      Result.X:= webpWidth;
      Result.Y:= webpHeight
    end;
  finally
    Freemem(pHeader);
  end;
end;

function TFPReaderWebP.InternalCheck(Str: TStream): boolean;
var
  Buf: array[0..3] of AnsiChar = #0#0#0#0;
  p: Int64;
begin
  if Str=nil then exit(false);
  p:=Str.Position;
  Result:= (Str.Read(Buf, 4)=4) and (Buf='RIFF') and
           (Str.Read(Buf, 4)=4) and
           (Str.Read(Buf, 4)=4) and (Buf='WEBP');
  // We could check the type here, VP8, VP8L,VP8X
  Str.Position:=p;
end;

constructor TFPReaderWebP.Create;
begin
  inherited Create;
end;

destructor TFPReaderWebP.Destroy;
begin
  inherited Destroy;
end;

initialization
  ImageHandlers.UnregisterImageHandlers('WebP Graphics', true, false);
  ImageHandlers.RegisterImageReader('WebP Graphics', 'webp', TFPReaderWebP);
end.
