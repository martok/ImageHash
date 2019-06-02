unit uFrmAutoMark;

{$mode objfpc}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TfrmAutoMark = class(TForm)
    Label1: TLabel;
    cbLargerResolution: TCheckBox;
    cbCompressedFile: TCheckBox;
    cbLargerFile: TCheckBox;
    cbDirectoryOrder: TCheckBox;
    btnMark: TButton;
    Button2: TButton;
    cbOnlyUnmarked: TCheckBox;
    cbOlderFile: TCheckBox;
    procedure btnMarkClick(Sender: TObject);
  private

  public

  end;

var
  frmAutoMark: TfrmAutoMark;

implementation

{$R *.lfm}

uses
  uThreadClassifier, Unit1, uThreadHashing, uUtils;

const
  CompressedFileExtensions = '.jpeg.jpg.jpe.jfif.png.gif.tif.tiff';

{ TfrmAutoMark }

procedure TfrmAutoMark.btnMarkClick(Sender: TObject);
var
  list: TListBox;
  c: TCluster;
  skipstate: set of TImageMark;
  idx, i, j, m: integer;
  im: PImageInfoItem;
  b: boolean;

  function Compare(const Item1, Item2: integer): integer;
  var
    im1, im2: PImageInfoItem;
    a1, a2: TSearchRec;
    x,y: integer;
  begin
    im1:= @Form1.ImageInfos[Item1];
    im2:= @Form1.ImageInfos[Item2];
    if cbLargerResolution.Checked then begin
      x:= im1^.ImgW*im1^.ImgH;
      y:= im2^.ImgW*im2^.ImgH;
      if x>y then
        Exit(-1);
      if x<y then
        Exit(1);
    end;    
    if cbCompressedFile.Checked then begin
      x:= Pos(ExtractFileExt(im1^.Filename), CompressedFileExtensions);
      y:= Pos(ExtractFileExt(im2^.Filename), CompressedFileExtensions);
      if (x>0) and (y=0) then
        Exit(-1);
      if (x=0) and (y<0) then
        Exit(1);
    end;
    if cbLargerFile.Checked or cbOlderFile.Checked then begin
      GetFileInfos(ConcatPaths([Form1.mePaths.Lines[im1^.Sourcedir], im1^.Filename]), a1);
      GetFileInfos(ConcatPaths([Form1.mePaths.Lines[im2^.Sourcedir], im2^.Filename]), a2);
    end;
    if cbLargerFile.Checked then begin
      if a1.Size>a2.Size then
        Exit(-1);
      if a1.Size<a2.Size then
        Exit(1);
    end;
    if cbOlderFile.Checked then begin
      if a1.TimeStamp<a2.TimeStamp then
        Exit(-1);
      if a1.TimeStamp>a2.TimeStamp then
        Exit(1);
    end;
    if cbDirectoryOrder.Checked then begin
      x:= im1^.Sourcedir;
      y:= im2^.Sourcedir;
      if x<y then
        Exit(-1);
      if y>x then
        Exit(1);
    end;
    Result:= 0;
  end;

begin
  list:= Form1.lbClusters as TListBox;
  for idx:= 0 to list.Count - 1 do begin
    c:= list.Items.Objects[idx] as TCluster;
    if cbOnlyUnmarked.Checked then
      skipstate:= [imIgnore, imDelete]
    else
      skipstate:= [imIgnore];
    b:= false;
    for i in c do begin
      im:= @Form1.ImageInfos[i];
      b:= im^.Mark in skipstate;
      if b then
        break;
    end;
    if b then
      continue;
    if (specialize TListTool<Integer>).FindSmallestValue(c, @Compare, m) >= 0 then begin
      for i in c do begin
        im:= @Form1.ImageInfos[i];  
        if i = m then
          im^.Mark:= imUnmarked
        else
          im^.Mark:= imDelete;
      end;
    end;
  end;
  Form1.lbClusters.Refresh;
end;

end.

