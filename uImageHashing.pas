unit uImageHashing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, IntfGraphics, FPimage, lcltype, lazcanvas, GraphType,
  fpcanvas;

function imgLoadFromFile(aFile: string): TLazIntfImage;
function imgConvertCompatibleGrey(source: TLazIntfImage): TLazIntfImage;
function imgScaleCompatible(source: TLazIntfImage; Width, Height: integer; Interpolation: TFPCustomInterpolation = nil): TLazIntfImage;
function imgGetData(img: TLazIntfImage): PByte;

type
  TImageHash =  bitpacked array[0..32768-1] of boolean;
  PImageHash = ^TImageHash;

  PLazIntfImage = ^TLazIntfImage;

procedure hashDHash(id: PByte; hash: PImageHash; hashsize: integer);
procedure hashDHashSq3(id: PByte; hash00, hash90, hash180: PImageHash; hashsize: integer);

procedure hashFromFileName(aFile: string; hashsize: integer; const hash00, hash90, hash180: PImageHash;
     out hashbits: integer; thumbsize:integer=0; thumb: PLazIntfImage = nil;
     ImgW: PInteger = nil; ImgH: PInteger = nil);

function hashToString(hash: PImageHash; hashbits: integer): string;

implementation

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

function imgGetData(img: TLazIntfImage): PByte;
var
  py, px: Integer;
  lsource: PRGBTriple;
begin
  GetMem(Result, img.Width*img.Height);
  for py:=0 to img.Height-1 do begin
    lsource := img.GetDataLineStart(py);
    for px:=0 to img.Width-1 do begin
      Result[px+py*img.Width]:= lsource[px].rgbtRed;
    end;
  end;
end;

procedure imgDataRotate90R(source, dest: PByte; hashsize: integer);
var
  py, px: Integer;
begin
  for py:= 0 to hashsize - 1 do
    for px:= 0 to hashsize - 1 do begin
      dest[px+py*hashsize]:= source[(hashsize-1-py)+px*hashsize];
    end;
end;

procedure hashDHash(id: PByte; hash: PImageHash; hashsize: integer);
var
  py, px: Integer;
begin
  for py:= 0 to hashsize - 1 do
    for px:= 0 to hashsize - 1 do begin
      hash^[px+py*hashsize]:= id[px+py*hashsize] < id[px+1+py*hashsize];
    end;
end;

procedure hashDHashP(id: PByte; hash: PImageHash; hashsize: integer);
var
  py, px: Integer;
begin
  for py:= 0 to hashsize - 1 do
    for px:= 0 to hashsize - 1 do begin
      hash^[px+py*hashsize]:= id[px+py*hashsize] < id[(px+1) mod hashsize+py*hashsize];
    end;
end;

procedure hashDHashSq3(id: PByte; hash00, hash90, hash180: PImageHash; hashsize: integer);
var
  id1, id2: PByte;
begin
  hashDHashP(id, hash00, hashsize);
  GetMem(id1, hashsize*hashsize);
  GetMem(id2, hashsize*hashsize);
  try
    imgDataRotate90R(id, id1, hashsize); 
    hashDHashP(id1, hash90, hashsize);

    imgDataRotate90R(id1, id2, hashsize); 
    hashDHashP(id2, hash180, hashsize);
  finally
    Freemem(id1);
    Freemem(id2);
  end;
end;

function BitReverse(b: Byte): Byte;
const
  Lookup:array[0..15] of byte = ($0,$8,$4,$c,$2,$a,$6,$e,$1,$9,$5,$d,$3,$b,$7,$f);
begin
  Result:= (Lookup[b and $f] shl 4) or Lookup[b shr 4];
end;

procedure hashFromFileName(aFile: string; hashsize: integer; const hash00,
  hash90, hash180: PImageHash; out hashbits: integer; thumbsize: integer;
  thumb: PLazIntfImage; ImgW: PInteger; ImgH: PInteger);
var
  source, inter, scale, grey: TLazIntfImage;
  fac: Single;
  id: PByte;
  FreeInter: Boolean;
begin
  FreeInter:= false;
  hashbits:= hashsize * hashsize;

  source:= imgLoadFromFile(aFile);
  // decimate to something useful first
  fac:= Max(1,Min(source.Width, source.Height)) / (hashsize*64);
  if fac > 1 then begin
    inter:= imgScaleCompatible(source, trunc(source.Width/fac), trunc(source.Height/fac), TFPBoxInterpolation.Create);
    FreeInter:= true;
  end
  else
    inter:= source;
  scale:= imgScaleCompatible(inter, hashsize, hashsize, TMitchelInterpolation.Create);
  grey:= imgConvertCompatibleGrey(scale);

  if thumbsize>0 then begin
    if source.Width > source.Height then
      thumb^:= imgScaleCompatible(inter, thumbsize, thumbsize * source.Height div source.Width)
    else
      thumb^:= imgScaleCompatible(inter, thumbsize * source.Width div source.Height, thumbsize)
  end;
  if Assigned(ImgW) then begin
    ImgW^:= source.Width;
    ImgH^:= source.Height;
  end;

  id:= imgGetData(grey);

  hashDHashSq3(id, hash00, hash90, hash180, hashsize);
  Freemem(id);

  if FreeInter then
    FreeAndNil(inter);
  FreeAndNil(scale);
  FreeAndNil(grey);
  FreeAndNil(source);
end;

function hashToString(hash: PImageHash; hashbits: integer): string;
const
  HexTbl : array[0..15] of char='0123456789ABCDEF';
var
  i: Integer;
  b: Byte;
begin
  SetLength(Result{%H-}, hashbits div 8 * 2);
  for i:= 0 to hashbits div 8 - 1 do begin
    b:= BitReverse(PByte(hash)[i]);
    //b:= PBYTE(hash)[i];
    Result[1+i*2  ]:= HexTbl[b shr 4];
    Result[1+i*2+1]:= HexTbl[b and $f];
  end;
end;



end.

