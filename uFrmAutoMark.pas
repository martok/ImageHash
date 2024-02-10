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
  uThreadClassifier, uFrmMain, uThreadHashing, uUtils;

const
  CompressedFileExtensions = '.jpeg.jpg.jpe.jfif.png.gif.tif.tiff';

{ TfrmAutoMark }

procedure TfrmAutoMark.btnMarkClick(Sender: TObject);
var
  list: TListBox; 
  sourcePaths: TStrings;
  imageInfos: PImageInfoList;
  c: TCluster;
  skipstate: set of TImageMark;
  idx, i, j, m, cluster_seen, cluster_skipped, marked_delete: integer;
  im: PImageInfoItem;
  b: boolean;

  function Compare(const Item1, Item2: integer): integer;
  var
    im1, im2: PImageInfoItem;
    a1, a2: TSearchRec;
    x,y: integer;
  begin
    im1:= @imageInfos[Item1];
    im2:= @imageInfos[Item2];
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
      GetFileInfos(im1^.FullName(sourcePaths), a1);
      GetFileInfos(im2^.FullName(sourcePaths), a2);
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
  list:= fmMain.lbClusters as TListBox;
  imageInfos:= PImageInfoList(@fmMain.ImageInfos[0]);
  sourcePaths:= fmMain.SourcePaths;
  if cbOnlyUnmarked.Checked then
    skipstate:= [imIgnore, imDelete]
  else
    skipstate:= [imIgnore];
  cluster_seen:= 0;
  cluster_skipped:= 0;
  marked_delete:= 0;
  for idx:= 0 to list.Count - 1 do begin
    c:= list.Items.Objects[idx] as TCluster;
    inc(cluster_seen);
    b:= false;
    for i in c do begin
      im:= @imageInfos[i];
      b:= im^.Mark in skipstate;
      if b then
        break;
    end;
    if b then begin
      inc(cluster_skipped);
      continue;
    end;
    if (specialize TListTool<Integer>).FindSmallestValue(c, @Compare, m) >= 0 then begin
      for i in c do begin
        im:= @imageInfos[i];
        if i = m then
          im^.Mark:= imUnmarked
        else begin
          im^.Mark:= imDelete;
          inc(marked_delete);
        end;
      end;
    end;
  end;
  list.Invalidate;
  MessageDlg(Format('Evaluated clusters: %d'+sLineBreak+
             'Skipped clusters: %d'+sLineBreak+
             'Marked for deletion: %d', [cluster_seen, cluster_skipped, marked_delete]),
              mtInformation, [mbOK], 0);
end;

end.

