unit uFrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, GraphType, CheckLst, Spin, ComCtrls, uThreadHashing, uThreadClassifier, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    clbFilter: TCheckListBox;
    mePaths: TMemo;
    Panel2: TPanel;
    pbClassifier: TProgressBar;
    btnStartScanners: TButton;
    seThreads: TSpinEdit;
    pbLoader: TProgressBar;
    seThumbSize: TSpinEdit;
    btnStopScanner: TButton;
    lbStatus: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Panel3: TPanel;
    lbHoverfile: TLabel;
    imHoverImage: TImage;
    Panel4: TPanel;
    lbClusters: TListBox;
    meLog: TMemo;
    Splitter1: TSplitter;
    Bevel1: TBevel;
    btnRecompare: TButton;
    Label3: TLabel;
    seTolerance: TSpinEdit;
    seMinDimension: TSpinEdit;
    Label4: TLabel;
    ilMarks: TImageList;
    ToolBar1: TToolBar;
    tbAutoMark: TToolButton;
    tbUnMark: TToolButton;
    tbUnIgnore: TToolButton;
    ToolButton1: TToolButton;
    tbMarkedTrash: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure btnStartScannersClick(Sender: TObject);
    procedure lbClustersDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbClustersMeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
    procedure btnStopScannerClick(Sender: TObject);
    procedure lbClustersMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure btnRecompareClick(Sender: TObject);
    procedure lbClustersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbAutoMarkClick(Sender: TObject);
    procedure tbUnMarkClick(Sender: TObject);
    procedure tbUnIgnoreClick(Sender: TObject);
    procedure tbMarkedTrashClick(Sender: TObject);
  private
    fClassifier: TClassifierThread;
    fImageInfos: TImageInfoList;
    fAbortFlag: boolean;
    procedure FreeData;
    procedure FreeClassifier;
    procedure ImageLoadFinished;
    procedure ImageClassifierFinished;
    function ImageAtXY(X, Y: integer; out ICluster, IImage: integer): boolean;
    procedure PrintMemStats;
  public
    property ImageInfos: TImageInfoList read fImageInfos;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math, IntfGraphics, FPimage, lcltype, LCLIntf, fpcanvas, uUtils, uImageHashing, uThreadScanner,
  uFrmAutoMark;

const
  IMAGE_MARK_DELETE = 0;
  IMAGE_MARK_IGNORE = 1;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  tn, ex: String;
begin
  clbFilter.Items.Clear;
  for i:= 0 to ImageHandlers.Count-1 do begin
    tn:= ImageHandlers.TypeNames[i];
    ex:= StringReplace('*.' + ImageHandlers.Extensions[tn], ';', ';*.', [rfReplaceAll]);
    clbFilter.Items.Add('%s (%s)', [tn, ex]);
    if not Assigned(ImageHandlers.ImageReader[tn]) then
      clbFilter.ItemEnabled[i]:= false
    else
      clbFilter.Checked[i]:= true;
  end;

  Caption:= Application.Title;

  seThreads.Value:= Max(1, TThread.ProcessorCount-1);

  meLog.Clear;
  lbHoverfile.Caption:= '';
  lbStatus.Caption:= 'Waiting...';

  mePaths.Clear;
  mePaths.Lines.Add(ExpandFileName(ConcatPaths([ExtractFilePath(ParamStr(0)),'..\data'])));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeClassifier;
  FreeData;
end;

procedure TForm1.ImageLoadFinished;
begin
  pbLoader.Position:= pbLoader.Position + 1;
  lbStatus.Caption:= Format('%d/%d',[pbLoader.Position,pbLoader.Max]);
  if Assigned(fClassifier) then
    fClassifier.WakeEvent.SetEvent;
end;

procedure TForm1.ImageClassifierFinished;
var
  c: TCluster;
  oldtop: integer;
  clusters: TClusterList;
begin
  pbClassifier.Position:= pbClassifier.Position + 1;

  { TODO : Locking }
  clusters:= TClusterList.Create;
  try
    for c in fClassifier.Clusters do begin
      if c.Count > 1 then
        clusters.Add(c);
    end;
    if clusters.Count <> lbClusters.Count then begin
      oldtop:= lbClusters.TopIndex;
      lbClusters.Items.BeginUpdate;
      try
        lbClusters.Items.Clear;
        for c in clusters do begin
          lbClusters.AddItem(inttostr(lbClusters.Items.Count), c);
        end;
      finally
        lbClusters.Items.EndUpdate;
      end;
      lbClusters.TopIndex:= oldtop;
    end else
      lbClusters.Invalidate;
  finally
    FreeAndNil(clusters);
  end;

end;

procedure TForm1.FreeData;
var
  i: integer;
  im: PImageInfoItem;
begin
  lbClusters.Clear;
  for i:= 0 to high(fImageInfos) do begin
    im:= @fImageInfos[i];
    im^.Done;
  end;
  SetLength(fImageInfos, 0);
end;

procedure TForm1.FreeClassifier;
begin
  if Assigned(fClassifier) then begin
    fClassifier.Terminate;
    fClassifier.WaitFor;
    FreeAndNil(fClassifier);
  end;
  lbClusters.Clear;
end;

procedure TForm1.btnStartScannersClick(Sender: TObject);
var
  scanners: array of TFileScannerThread;
  i, j, k: Integer;
  filter, ex: String;
  loaders: array of TImageHashThread;
  t1,t2: Int64;
  loadermemerror: Boolean;
begin
  meLog.Clear;
  FreeClassifier;
  FreeData;

  btnStartScanners.Enabled:= false;
  btnStopScanner.Enabled:= true;
  mePaths.Enabled:= false;
  try
    fAbortFlag:= false;
    filter:= '';
    for i:= 0 to clbFilter.Count-1 do begin
      if clbFilter.Checked[i] then begin
        ex:= StringReplace('*.' + ImageHandlers.Extensions[ImageHandlers.TypeNames[i]], ';', ';*.', [rfReplaceAll]);
        filter += ex + ';';
      end;
    end;
    SetLength(Filter, Length(Filter)-1);

    lbStatus.Caption:= 'Creating file list...';
    SetLength(scanners, mePaths.Lines.Count);
    for i:= 0 to High(scanners) do begin
      scanners[i]:= TFileScannerThread.Create;
      scanners[i].Path:= mePaths.Lines[i];
      scanners[i].Filter:= filter;
      scanners[i].Start;
    end;
    WaitForMultipleThreads(@scanners[0], length(scanners), @Application.ProcessMessages);

    SetLength(fImageInfos, 0);
    for i:= 0 to High(scanners) do begin
      k:= Length(fImageInfos);
      SetLength(fImageInfos, k + scanners[i].List.Count);
      for j:= 0 to scanners[i].List.Count-1 do begin
        with fImageInfos[j+k] do begin
          Init;
          Sourcedir:= i;
          Filename:= scanners[i].List[j];
        end;
      end;
    end;
    for i:= 0 to High(scanners) do
      scanners[i].Free;

    pbLoader.Position:= 0;
    pbLoader.Max:= length(fImageInfos);

    lbStatus.Caption:= 'Setup classifier...';
    SetLength(loaders, seThreads.Value);
    t1:= GetTickCount64;
    btnRecompare.Click;
    lbStatus.Caption:= 'Setup loader...';
    for i:= 0 to High(loaders) do begin
      loaders[i]:= TImageHashThread.Create;
      loaders[i].Prefixes:= mePaths.Lines;
      loaders[i].List:= PImageInfoList(@fImageInfos[0]);
      loaders[i].Count:= Length(fImageInfos);
      loaders[i].ThumbSize:= seThumbSize.Value;
      loaders[i].OnImageFinish:= @ImageLoadFinished;
      loaders[i].Start;
    end;
    WaitForMultipleThreads(@loaders[0], length(loaders), @Application.ProcessMessages, @fAbortFlag);
    loadermemerror:= false;
    for i:= 0 to High(loaders) do begin
      loadermemerror:= loadermemerror or loaders[i].SawMemoryError;
      loaders[i].Free;
    end;
    if loadermemerror then begin
      meLog.Lines.Add('- had EOutOfMemory, rerunning errors single-threaded');
      // reflag all that are still at "working" - threads are finished, they are all SawMemoryError cases
      for i:= 0 to high(fImageInfos) do
        InterlockedCompareExchange(fImageInfos[i].Status, STATUS_NONE, STATUS_WORKING);
      // rerun a single loader to redo all that failed because of OutOfMemory errors
      SetLength(loaders, 1);
      loaders[0]:= TImageHashThread.Create;
      loaders[0].Prefixes:= mePaths.Lines;
      loaders[0].List:= PImageInfoList(@fImageInfos[0]);
      loaders[0].Count:= Length(fImageInfos);
      loaders[0].ThumbSize:= seThumbSize.Value;
      loaders[0].FinalMemoryError:= true;
      loaders[0].OnImageFinish:= @ImageLoadFinished;
      loaders[0].Start;
    end;
    WaitForMultipleThreads(@loaders[0], length(loaders), @Application.ProcessMessages, @fAbortFlag);
    WaitForMultipleThreads(@fClassifier, 1, @Application.ProcessMessages, @fAbortFlag);
    t2:= GetTickCount64;
    meLog.Lines.Add('time:  %dms',[t2-t1]);
    meLog.Lines.Add('clusters: %d',[fClassifier.Clusters.Count]);
    k:= 0;
    for i:= 0 to fClassifier.Clusters.Count-1 do begin
      if fClassifier.Clusters[i].Count > 1 then
        inc(k);
    end;
    meLog.Lines.Add('clusters>1: %d',[k]);
    PrintMemStats;
  finally
    mePaths.Enabled:= True;
    btnStopScanner.Enabled:= false;
    btnStartScanners.Enabled:= true;
  end;
end;

procedure TForm1.btnStopScannerClick(Sender: TObject);
begin
  fAbortFlag:= true;
end;

procedure TForm1.btnRecompareClick(Sender: TObject);
begin
  FreeClassifier;
  pbClassifier.Position:= 0;
  pbClassifier.Max:= length(fImageInfos);
  fClassifier:= TClassifierThread.Create;
  fClassifier.List:= PImageInfoList(@fImageInfos[0]);
  fClassifier.Count:= Length(fImageInfos);
  fClassifier.Limit:= seTolerance.Value;
  fClassifier.MinDimension:= seMinDimension.Value;
  fClassifier.OnImageFinish:= @ImageClassifierFinished;
  fClassifier.Start;
end;


function TForm1.ImageAtXY(X, Y: integer; out ICluster, IImage: integer): boolean;
var
  c: TCluster;
  i, j: integer;
begin
  Result:= false;
  ICluster:= -1;
  IImage:= -1;
  i:= lbClusters.GetIndexAtY(Y);
  if (i>=0) and (i<lbClusters.Items.Count) and PtInRect(lbClusters.ItemRect(i), Point(x,y)) then begin
    c:= lbClusters.Items.Objects[i] as TCluster;
    j:= (X - 5) div (seThumbSize.Value+1);
    if (j>=0) and (j<c.Count) then begin
      ICluster:= i;
      IImage:= j;
      Result:= true;
    end;
  end;
end;

procedure TForm1.PrintMemStats;
  function StrSize(s: String): PtrUInt;
  begin
    if s='' then
      Exit(0);
    Result:= Length(s) * StringElementSize(s) + sizeof(ptrint)*2;
  end;

var
  szAll, szList, szClusters: PtrUInt;
  hs: THeapStatus;
  i: Integer;
begin
  // count up
  szlist:= 0;
  for i:= 0 to High(fImageInfos) do begin
    inc(szList, sizeof(TImageInfoItem));
    inc(szList, StrSize(fImageInfos[i].Error));
    inc(szList, StrSize(fImageInfos[i].Filename));
    if Assigned(fImageInfos[i].Thumbnail) then begin
      inc(szList, fImageInfos[i].Thumbnail.InstanceSize);
      inc(szList, fImageInfos[i].Thumbnail.Height * fImageInfos[i].Thumbnail.DataDescription.BytesPerLine);
    end;
  end;
  szClusters:= 0;
  for i:= 0 to fClassifier.Clusters.Count-1 do begin
    inc(szClusters, fClassifier.Clusters[i].InstanceSize);
    inc(szClusters, fClassifier.Clusters[i].Capacity * fClassifier.Clusters[i].ItemSize);
  end;
  szAll:= szList + szClusters;
  meLog.Lines.Add('UsedAll = %d k', [szAll shr 10]);
  meLog.Lines.Add('UsedList = %d k', [szList shr 10]);
  meLog.Lines.Add('UsedClusters = %d k', [szClusters shr 10]);
  hs:= GetHeapStatus;
  meLog.Lines.Add('TotalAddrSpace = %d k', [hs.TotalAddrSpace shr 10]);
  meLog.Lines.Add('TotalAllocated = %d k', [hs.TotalAllocated shr 10]);
  meLog.Lines.Add('TotalFree = %d k', [hs.TotalFree shr 10]);
end;

procedure TForm1.lbClustersMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  index, i: Integer;
  c: TCluster;
  im: PImageInfoItem;
  img: TLazIntfImage;
  fullname: RawByteString;
  sr: TRawByteSearchRec;

  procedure Clear;
  begin
    imHoverImage.Picture.Clear;
    imHoverImage.Tag:= 0;
    lbHoverfile.Caption:= '';
  end;

begin
  if not ImageAtXY(X, Y, index, i) then begin
    Clear;
    exit;
  end;

  c:= lbClusters.Items.Objects[Index] as TCluster;
  im:= @fClassifier.List[c[i]];
  if imHoverImage.Tag = PtrInt(im) then
    exit;
  imHoverImage.Tag:= PtrInt(im);
  imHoverImage.Picture.Bitmap.Assign(im^.Thumbnail);
  imHoverImage.Refresh;
  fullname:= im^.FullName(mePaths.Lines);
  lbHoverfile.Caption:= Format('%s', [fullname]);
  if FindFirst(fullname, faAnyFile, sr) = 0 then begin
    img:= imgLoadFromFile(fullname);
    try
        lbHoverfile.Caption:= Format('%s (%dkb - %dx%dpx - %s)', [fullname, sr.Size div 1024, img.Width, img.Height, DateTimeToStr(sr.TimeStamp)]);
      imHoverImage.Picture.Bitmap.LoadFromIntfImage(img);
    finally
      FreeAndNil(img);
    end;
    FindClose(sr);
  end else
    imHoverImage.Picture.Clear;
end;

procedure TForm1.lbClustersDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  c: TCluster;
  i: integer;
  im: PImageInfoItem;
  bmp: TBitmap;
  sub:TRect;
begin
  c:= lbClusters.Items.Objects[Index] as TCluster;
  with lbClusters.Canvas do begin
    Brush.Color:= lbClusters.Color;
    FillRect(ARect);
    bmp:= TBitmap.Create;
    try
      for i:= 0 to c.Count-1 do begin
        im:= @fClassifier.List[c[i]];
        sub:= Bounds(ARect.Left + 5 + i*(seThumbSize.Value+1), ARect.Top, ARect.Height, ARect.Height);
        bmp.LoadFromIntfImage(im^.Thumbnail);
        Draw(sub.Left + (sub.Width-bmp.Width) div 2, sub.Top + (sub.Height-bmp.Height) div 2, bmp);
        case im^.Mark of
          imUnmarked: ;
          imDelete: ilMarks.Draw(lbClusters.Canvas, sub.Right-ilMarks.Width, sub.Top, IMAGE_MARK_DELETE);
          imIgnore: ilMarks.Draw(lbClusters.Canvas, sub.Right-ilMarks.Width, sub.Top, IMAGE_MARK_IGNORE);
        end;
      end;
    finally
      FreeAndNil(bmp);
    end;
  end;
end;

procedure TForm1.lbClustersMeasureItem(Control: TWinControl; Index: Integer;
  var AHeight: Integer);
begin
  AHeight:= seThumbSize.Value + 2;
end;

procedure TForm1.lbClustersMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  index, i: integer;
  c: TCluster;
  im: PImageInfoItem;
begin
  if not ImageAtXY(X, Y, index, i) then
    exit;

  c:= lbClusters.Items.Objects[Index] as TCluster;
  im:= @fClassifier.List[c[i]];
  case Button of
    mbLeft: if im^.Mark = imDelete then im^.Mark:= imUnmarked else im^.Mark:= imDelete;
    mbMiddle: im^.Mark:= imIgnore;
    mbRight: OpenDocument(im^.FullName(mePaths.Lines));
  end;
  lbClusters.Invalidate;
end;

procedure TForm1.tbAutoMarkClick(Sender: TObject);
begin
  frmAutoMark.ShowModal;
  lbClusters.Invalidate;
end;

procedure TForm1.tbUnMarkClick(Sender: TObject);
var
  idx, i: Integer;
  c: TCluster;
  im: PImageInfoItem;
begin
  for idx:= 0 to lbClusters.Count - 1 do begin
    c:= lbClusters.Items.Objects[idx] as TCluster;
    for i in c do begin
      im:= @ImageInfos[i];
      if im^.Mark<>imIgnore then
        im^.Mark:= imUnmarked;
    end;
  end;
  lbClusters.Invalidate;
end;

procedure TForm1.tbUnIgnoreClick(Sender: TObject);
var
  idx, i: Integer;
  c: TCluster;
  im: PImageInfoItem;
begin
  for idx:= 0 to lbClusters.Count - 1 do begin
    c:= lbClusters.Items.Objects[idx] as TCluster;
    for i in c do begin
      im:= @ImageInfos[i];
      if im^.Mark = imIgnore then
        im^.Mark:= imUnmarked;
    end;
  end;
  lbClusters.Invalidate;
end;

procedure TForm1.tbMarkedTrashClick(Sender: TObject);
var
  marked: TCluster;
  idx, i: Integer;
  c: TCluster;
  im: PImageInfoItem;
begin
  marked:= TCluster.Create;
  try
    for idx:= 0 to lbClusters.Count - 1 do begin
      c:= lbClusters.Items.Objects[idx] as TCluster;
      for i in c do begin
        im:= @ImageInfos[i];
        if im^.Mark = imDelete then
          marked.Add(i);
      end;
    end;
    if marked.Count = 0 then begin
      MessageDlg('No items selected for deletion!', mtInformation, [mbOK], 0);
      exit;
    end;
    if MessageDlg(Format('%d items selected for deletion, continue?', [marked.Count]), mtInformation, mbYesNo, 0) <> mrYes then
      exit;
    marked.Sort;
    for i:= marked.Count-1 downto 0 do begin
      idx:= marked[i];
      im:= @ImageInfos[idx];
      if SendFilesToTrash([im^.FullName(mePaths.Lines)]) then begin
        im^.Done;
        Delete(fImageInfos, idx, 1);
      end;
    end;
  finally
    FreeAndNil(marked);
  end;
  btnRecompare.Click;
end;

end.

