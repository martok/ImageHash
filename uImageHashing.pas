unit uImageHashing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, IntfGraphics, FPimage, lcltype, lazcanvas, GraphType,
  fpcanvas, uCanvasInterpolation;

function imgLoadFromFile(aFile: string): TLazIntfImage;
function imgConvertCompatibleGrey(source: TLazIntfImage): TLazIntfImage;
function imgScaleCompatible(source: TLazIntfImage; Width, Height: integer; Interpolation: TFPCustomInterpolation = nil): TLazIntfImage;
procedure imgGetData(img: TLazIntfImage; dest: PByte);

const
  HASH_BITS = bitsizeof(QWORD);
  HASH_SIZE = Trunc(sqrt(HASH_BITS));

type
  TImageData = packed array [0..(HASH_SIZE*HASH_SIZE)-1] of Byte;
  TImageHash = bitpacked array[0..HASH_BITS-1] of boolean;
  PImageHash = ^TImageHash;

  PLazIntfImage = ^TLazIntfImage;

procedure hashDHash(id: PByte; hash: PImageHash);
procedure hashDHashSq3(id: PByte; hash00, hash90, hash180: PImageHash);

procedure hashFromFileName(aFile: string; const hash00, hash90, hash180: PImageHash;
     thumbsize:integer=0; thumb: PLazIntfImage = nil;
     ImgW: PInteger = nil; ImgH: PInteger = nil);

function hashToString(hash: PImageHash): string;

implementation

uses
  SysConst;

function imgLoadFromFile(aFile: string): TLazIntfImage;
var
  source: TLazIntfImage;
  sourceformat: TRawImage;
begin
  try
    sourceformat.Init;
    sourceformat.Description.Init_BPP24_B8G8R8_BIO_TTB(0,0);
    sourceformat.CreateData(false);
    source:= TLazIntfImage.Create(0,0);
    source.SetRawImage(sourceformat);
    source.LoadFromFile(aFile);
    if not Assigned(source.PixelData) then
      raise EOutOfMemory.Create(SOutOfMemory);
    Result:= source;
  except
    FreeAndNil(source);
    raise;
  end;
end;

function imgConvertCompatibleGrey(source: TLazIntfImage): TLazIntfImage;
var
  grey: TLazIntfImage;
  py, px: Integer;
  lgrey, lsource: PRGBTriple;
  c: TRGBTriple;
begin
  try
    grey:= TLazIntfImage.CreateCompatible(source, source.Width, source.Height);
    grey.FillPixels(FPColor(0,0,0));
    for py:= 0 to source.Height-1 do begin
      lsource := source.GetDataLineStart(py);
      lgrey := grey.GetDataLineStart(py);
      for px:= 0 to source.Width-1 do begin
        c:= lsource[px];
        {
          When translating a color image to black and white (mode "L"),
          the library uses the ITU-R 601-2 luma transform::
        }
        lgrey[px].rgbtRed:= trunc(EnsureRange(c.rgbtRed * 299/1000 + c.rgbtGreen * 587/1000 + c.rgbtBlue * 114/1000, low(byte), high(byte)));
      end;
    end;
    Result:= grey;
  except
    FreeAndNil(grey);
    raise;
  end;
end;

function imgScaleCompatible(source: TLazIntfImage; Width, Height: integer;
  Interpolation: TFPCustomInterpolation): TLazIntfImage;
var
  scale: TLazIntfImage;
  scalecanvas: TLazCanvas;
begin
  try
    try
      scale:= TLazIntfImage.CreateCompatible(source, Width, Height);
      scalecanvas:= TLazCanvas.Create(scale);
      scalecanvas.Interpolation:= Interpolation;
      scalecanvas.DrawingMode:= dmOpaque;
      scalecanvas.StretchDraw(0, 0, scale.Width, scale.Height, source);
      Result:= scale;
    finally
      Interpolation.Free;
      FreeAndNil(scalecanvas);
    end;
  except
    FreeAndNil(scale);
    raise;
  end;
end;

procedure imgGetData(img: TLazIntfImage; dest: PByte);
var
  py, px: Integer;
  lsource: PRGBTriple;
begin
  for py:=0 to img.Height-1 do begin
    lsource := img.GetDataLineStart(py);
    for px:=0 to img.Width-1 do begin
      dest[px+py*img.Width]:= lsource[px].rgbtRed;
    end;
  end;
end;

procedure imgDataRotate90R(source, dest: PByte);
var
  py, px: Integer;
begin
  for py:= 0 to HASH_SIZE - 1 do
    for px:= 0 to HASH_SIZE - 1 do begin
      dest[px+py*HASH_SIZE]:= source[(HASH_SIZE-1-py)+px*HASH_SIZE];
    end;
end;

procedure hashDHash(id: PByte; hash: PImageHash);
var
  py, px: Integer;
begin
  for py:= 0 to HASH_SIZE - 1 do
    for px:= 0 to HASH_SIZE - 1 do begin
      hash^[px+py*HASH_SIZE]:= id[px+py*HASH_SIZE] < id[px+1+py*HASH_SIZE];
    end;
end;

procedure hashDHashP(id: PByte; hash: PImageHash);
var
  py, px,
  k, kright: Integer;
begin
  for py:= 0 to HASH_SIZE - 1 do
    for px:= 0 to HASH_SIZE - 1 do begin
      // the local vars are important for correct x86_64 codegen
      k:= px+py*HASH_SIZE;
      kright:= (px+1) mod HASH_SIZE+py*HASH_SIZE;
      hash^[k]:= id[k] < id[kright];
    end;
end;

procedure hashDHashSq3(id: PByte; hash00, hash90, hash180: PImageHash);
var
  id1, id2: TImageData;
begin
  hashDHashP(id, hash00);
  imgDataRotate90R(id, @id1);
  hashDHashP(@id1, hash90);

  imgDataRotate90R(@id1, @id2);
  hashDHashP(@id2, hash180);
end;

function BitReverse(b: Byte): Byte;
const
  Lookup:array[0..15] of byte = ($0,$8,$4,$c,$2,$a,$6,$e,$1,$9,$5,$d,$3,$b,$7,$f);
begin
  Result:= (Lookup[b and $f] shl 4) or Lookup[b shr 4];
end;

procedure hashFromFileName(aFile: string; const hash00, hash90, hash180: PImageHash; thumbsize: integer;
  thumb: PLazIntfImage; ImgW: PInteger; ImgH: PInteger);
var
  source, inter, scale, grey: TLazIntfImage;
  fac: Single;
  id: TImageData;
  FreeInter: Boolean;
begin
  FreeInter:= false;

  source:= imgLoadFromFile(aFile);

  if Assigned(ImgW) then begin
    ImgW^:= source.Width;
    ImgH^:= source.Height;
  end;

  // decimate to something useful first, but keep enough information so that downscaling later does not change the DHash
  fac:= Min(source.Width, source.Height) / Max(HASH_SIZE*40, thumbsize);
  if fac > 1 then begin
    inter:= imgScaleCompatible(source, trunc(source.Width/fac), trunc(source.Height/fac), TFPBoxInterpolation.Create);
    FreeInter:= true;
  end
  else
    inter:= source;

  // create thumbs
  if thumbsize>0 then begin    
    fac:= Max(inter.Width, inter.Height) / thumbsize;
    thumb^:= imgScaleCompatible(inter, Round(inter.Width/fac), Round(inter.Height/fac), TMitchelInterpolationOpt.Create);
  end;

  // create greyscale for DHash
  scale:= imgScaleCompatible(inter, HASH_SIZE, HASH_SIZE, TMitchelInterpolationOpt.Create);
  grey:= imgConvertCompatibleGrey(scale);
  imgGetData(grey, @id);
  hashDHashSq3(id, hash00, hash90, hash180);

  if FreeInter then
    FreeAndNil(inter);
  FreeAndNil(scale);
  FreeAndNil(grey);
  FreeAndNil(source);
end;

function hashToString(hash: PImageHash): string;
const
  HexTbl : array[0..15] of char='0123456789ABCDEF';
var
  i: Integer;
  b: Byte;
begin
  SetLength(Result{%H-}, HASH_BITS div 8 * 2);
  for i:= 0 to HASH_BITS div 8 - 1 do begin
    b:= BitReverse(PByte(hash)[i]);
    //b:= PBYTE(hash)[i];
    Result[1+i*2  ]:= HexTbl[b shr 4];
    Result[1+i*2+1]:= HexTbl[b and $f];
  end;
end;



end.

