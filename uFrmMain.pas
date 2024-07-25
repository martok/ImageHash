unit uFrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, GraphType, CheckLst, Spin, ComCtrls,
  Buttons, uNotifier, uThreadHashing, uThreadClassifier, uFrmPathEditor, uProgramInfoDialog, Types;

type

  { TfmMain }

  TfmMain = class(TForm)
    pnWorkspace: TPanel;
    clbFilter: TCheckListBox;
    pbClassifier: TProgressBar;
    seThreads: TSpinEdit;
    seThumbSize: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel3: TPanel;
    lbHoverfile: TLabel;
    imHoverImage: TImage;
    lbClusters: TListBox;
    meLog: TMemo;
    Splitter1: TSplitter;
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
    pcSidebar: TPageControl;
    tsScanSetup: TTabSheet;
    tsScanResults: TTabSheet;
    GroupBox1: TGroupBox;
    Panel5: TGroupBox;
    Panel1: TPanel;
    GroupBox2: TGroupBox;
    Splitter2: TSplitter;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    pbLoader: TProgressBar;
    lbStatus: TLabel;
    Splitter3: TSplitter;
    frmPathEditor1: TfrmPathEditor;
    btnStartStopLoader: TBitBtn;
    ilButtons: TImageList;
    btnRecompare: TBitBtn;
    Bevel1: TBevel;
    ToolButton2: TToolButton;
    tbMarkInfo: TToolButton;
    InfoDialog1: TInfoDialog;
    btnInfo: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure lbClustersDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbClustersMeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
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
    procedure btnStartStopLoaderClick(Sender: TObject);
    procedure tbMarkInfoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure btnInfoClick(Sender: TObject);
    procedure lbClustersMouseLeave(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    fClassifier: TClassifierThread;
    fImageInfos: TImageInfoList;
    fSourcePaths: TStrings;
    fAbortFlag: boolean;
    fNotifier: TThreadStatusNotifier;
    fHoverImage: PImageInfoItem;
    procedure FreeData;
    procedure FreeClassifier;
    function GetImageInfo(const Index: Integer): PImageInfoItem;
    function GetCheckedFilters: string;
    procedure RunLoaderAndWait;
    procedure RunClassifier;
    procedure NotifyProgress;
    function ImageAtXY(X, Y: integer; out ICluster, IImage: integer): boolean;
    procedure HoveredClear;
    procedure PrintMemStats;
  public
    property ImageInfos: TImageInfoList read fImageInfos;
    property SourcePaths: TStrings read fSourcePaths;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

uses
  Math, IntfGraphics, FPimage, lcltype, LCLIntf, fpcanvas, uwinImports,
  uUtils, uImageHashing, uThreadScanner, uFrmAutoMark;

const
  IMAGE_MARK_DELETE = 0;
  IMAGE_MARK_IGNORE = 1;   
  IMAGE_SCAN_START = 0;
  IMAGE_SCAN_STOP  = 1;
  IMAGE_SCAN_RERUN = 2;

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
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
  pcSidebar.ActivePage:= tsScanSetup;

  seThreads.Value:= Max(1, TThread.ProcessorCount-1);

  meLog.Clear;
  lbHoverfile.Caption:= '';
  lbStatus.Caption:= 'Waiting...';
  btnStartStopLoader.ImageIndex:= IMAGE_SCAN_START;
  btnRecompare.Enabled:= false;  

  fNotifier:= TThreadStatusNotifier.Create;
  fNotifier.OnUpdateUIEvent:= @NotifyProgress;

  frmPathEditor1.Clear;
  {$IfDef DEBUG}
  frmPathEditor1.Add(ExpandFileName(ConcatPaths([ExtractFilePath(ParamStr(0)),'..\data'])));
  {$EndIf}
  fSourcePaths:= TStringList.Create;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeClassifier;
  FreeData;
  FreeAndNil(fNotifier);
end;


procedure TfmMain.FreeData;
var
  i: integer;
begin
  lbClusters.Clear;
  for i:= 0 to high(fImageInfos) do begin
    fImageInfos[i].Done;
  end;
  SetLength(fImageInfos, 0);
end;

procedure TfmMain.FreeClassifier;
begin
  if Assigned(fClassifier) then begin
    fClassifier.Terminate;
    fClassifier.WaitFor;
    FreeAndNil(fClassifier);
  end;
  lbClusters.Clear;
end;   

function TfmMain.GetImageInfo(const Index: Integer): PImageInfoItem;
begin
  Result:= @fImageInfos[Index];
end;

function TfmMain.GetCheckedFilters: string;
var
  i: Integer;
  ex: String;
begin
  Result:= '';
  for i:= 0 to clbFilter.Count-1 do begin
    if clbFilter.Checked[i] then begin
      ex:= StringReplace('*.' + ImageHandlers.Extensions[ImageHandlers.TypeNames[i]], ';', ';*.', [rfReplaceAll]);
      Result += ex + ';';
    end;
  end;
  SetLength(Result, Length(Result)-1);
end;

procedure TfmMain.RunLoaderAndWait;
var
  scanners: array of TFileScannerThread;
  scanner: TFileScannerThread;
  i, j, k: Integer;
  filter: String;
  loaders: array of TImageHashThread;
  t1,t2: Int64;
  loadermemerror: Boolean;
begin
  meLog.Clear;
  FreeClassifier;
  FreeData;

  btnStartStopLoader.ImageIndex:= IMAGE_SCAN_STOP;
  tsScanSetup.Enabled:= false;
  try
    fAbortFlag:= false;
    filter:= GetCheckedFilters;

    lbStatus.Caption:= 'Creating file list...';
    scanners:= nil;
    fSourcePaths.Assign(frmPathEditor1.Items);
    for i:= 0 to fSourcePaths.Count - 1 do begin
      if not DirectoryExists(fSourcePaths[i]) then
        continue;
      scanner:= TFileScannerThread.Create;
      scanner.Path:= fSourcePaths[i];
      scanner.Filter:= filter;
      scanner.Start;
      Insert(scanner, scanners, length(scanners));
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

    lbStatus.Caption:= 'Initialize...';
    fNotifier.StartProcess(Length(fImageInfos));

    if Length(fImageInfos) = 0 then begin
      lbStatus.Caption:= 'No files found';
      Exit;
    end;

    t1:= GetTickCount64;

    lbStatus.Caption:= 'Setup classifier...';
    RunClassifier;
    btnRecompare.Enabled:= true;

    lbStatus.Caption:= 'Setup loader...';
    SetLength(loaders, seThreads.Value);
    for i:= 0 to High(loaders) do begin
      loaders[i]:= TImageHashThread.Create;
      loaders[i].Prefixes:= fSourcePaths;
      loaders[i].List:= PImageInfoList(GetImageInfo(0));
      loaders[i].Count:= Length(fImageInfos);
      loaders[i].ThumbSize:= seThumbSize.Value;
      loaders[i].Notifier:= fNotifier;
      loaders[i].Start;
    end;
    WaitForMultipleThreads(@loaders[0], length(loaders), @Application.ProcessMessages, @fAbortFlag);
    loadermemerror:= false;
    for i:= 0 to High(loaders) do begin
      loadermemerror:= loadermemerror or loaders[i].SawMemoryError;
      loaders[i].Free;
    end;
    SetLength(loaders, 0);

    if loadermemerror then begin
      meLog.Lines.Add('- had EOutOfMemory, rerunning errors single-threaded');
      // reflag all that are still at "working" - threads are finished, they are all SawMemoryError cases
      for i:= 0 to high(fImageInfos) do
        InterlockedCompareExchange(fImageInfos[i].Status, STATUS_NONE, STATUS_WORKING);
      // rerun a single loader to redo all that failed because of OutOfMemory errors
      SetLength(loaders, 1);
      loaders[0]:= TImageHashThread.Create;
      loaders[0].Prefixes:= fSourcePaths;
      loaders[0].List:= PImageInfoList(GetImageInfo(0));
      loaders[0].Count:= Length(fImageInfos);
      loaders[0].ThumbSize:= seThumbSize.Value;
      loaders[0].FinalMemoryError:= true;
      loaders[0].Notifier:= fNotifier;
      loaders[0].Start;
      WaitForMultipleThreads(@loaders[0], length(loaders), @Application.ProcessMessages, @fAbortFlag);
    end;
    WaitForMultipleThreads(@fClassifier, 1, @Application.ProcessMessages, @fAbortFlag);

    t2:= GetTickCount64;
    meLog.Lines.Add('time:  %dms',[t2-t1]);
    meLog.Lines.Add('clusters: %d / %d',[fClassifier.Clusters.Count, Length(fImageInfos)]);
    k:= 0;
    for i:= 0 to fClassifier.Clusters.Count-1 do begin
      if fClassifier.Clusters[i].Count > 1 then
        inc(k);
    end;
    meLog.Lines.Add('clusters>1: %d',[k]);
    {$IfDef DEBUG}
    PrintMemStats;
    {$EndIf}
  finally
    tsScanSetup.Enabled:= True;
    btnStartStopLoader.ImageIndex:= IMAGE_SCAN_START;
  end;
end;

procedure TfmMain.RunClassifier;
begin
  FreeClassifier;
  fNotifier.Classified:= 0;
  fClassifier:= TClassifierThread.Create;
  fClassifier.List:= PImageInfoList(GetImageInfo(0));
  fClassifier.Count:= Length(fImageInfos);
  fClassifier.Limit:= seTolerance.Value;
  fClassifier.MinDimension:= seMinDimension.Value;
  fClassifier.Notifier:= fNotifier;
  fClassifier.Start;
end;   

procedure TfmMain.NotifyProgress;  
var
  c: TCluster;
  oldtop: integer;
  clusters: TClusterList;
begin                      
  if pbLoader.Max <> fNotifier.TotalCount then begin
    pbLoader.Max:= fNotifier.TotalCount;
    pbClassifier.Max:= fNotifier.TotalCount;
  end;

  if pbLoader.Position <> fNotifier.Loaded then begin
    pbLoader.Position:= fNotifier.Loaded;
    lbStatus.Caption:= Format('%d/%d', [pbLoader.Position, pbLoader.Max]);
  end;

  if pbClassifier.Position <> fNotifier.Classified then begin
    pbClassifier.Position:= fNotifier.Classified;
    clusters:= TClusterList.Create;
    try
      fClassifier.GetClusters(clusters);
      if clusters.Count <> lbClusters.Count then begin
        oldtop:= lbClusters.TopIndex;
        lbClusters.Items.BeginUpdate;
        try
          lbClusters.Items.Clear;
          for c in clusters do begin
            lbClusters.AddItem(IntToStr(lbClusters.Items.Count), c);
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
end;

procedure TfmMain.btnStartStopLoaderClick(Sender: TObject);
begin
  if btnStartStopLoader.ImageIndex = IMAGE_SCAN_START then begin
    pcSidebar.ActivePage:= tsScanSetup;
    if not frmPathEditor1.ValidatePaths then
      exit;

    pcSidebar.ActivePage:= tsScanResults;
    RunLoaderAndWait;
  end else begin
    fAbortFlag:= true;
  end;
end;

procedure TfmMain.btnRecompareClick(Sender: TObject);
begin
  RunClassifier;
end;

function TfmMain.ImageAtXY(X, Y: integer; out ICluster, IImage: integer): boolean;
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

procedure TfmMain.PrintMemStats;
  function StrSize(s: String): PtrUInt;
  begin
    if s='' then
      Exit(0);
    Result:= Length(s) * StringElementSize(s) + sizeof(ptrint)*2;
  end;

var
  szAll, szList, szClusters: PtrUInt;
  hs: TFPCHeapStatus;
  i: Integer;
  im: PImageInfoItem;
begin
  // count up
  szlist:= 0;
  for i:= 0 to High(fImageInfos) do begin
    im:= GetImageInfo(i);
    inc(szList, sizeof(TImageInfoItem));
    inc(szList, StrSize(im^.Error));
    inc(szList, StrSize(im^.Filename));
    if Assigned(im^.Thumbnail) then begin
      inc(szList, im^.Thumbnail.InstanceSize);
      inc(szList, im^.Thumbnail.Height * im^.Thumbnail.DataDescription.BytesPerLine);
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
  hs:= GetFPCHeapStatus;
  meLog.Lines.Add('MaxHeapSize = %d k', [hs.MaxHeapSize shr 10]);
  meLog.Lines.Add('MaxHeapUsed = %d k', [hs.MaxHeapUsed shr 10]);
  meLog.Lines.Add('CurrHeapSize = %d k', [hs.CurrHeapSize shr 10]);
  meLog.Lines.Add('CurrHeapUsed = %d k', [hs.CurrHeapUsed shr 10]);
end;

procedure TfmMain.HoveredClear;
begin
  imHoverImage.Picture.Clear;
  fHoverImage:= nil;
  lbHoverfile.Caption:= '';
  lbClusters.Invalidate;
end;

procedure TfmMain.lbClustersMouseLeave(Sender: TObject);
begin
  HoveredClear;
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fAbortFlag:= true;
end;


procedure TfmMain.lbClustersMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  index, i: Integer;
  c: TCluster;
  im: PImageInfoItem;
  img: TLazIntfImage;
  fullname: RawByteString;
  sr: TRawByteSearchRec;
begin
  if not ImageAtXY(X, Y, index, i) then begin
    HoveredClear;
    exit;
  end;

  c:= lbClusters.Items.Objects[Index] as TCluster;
  im:= GetImageInfo(c[i]);
  if fHoverImage = im then
    exit;
  fHoverImage:= im;
  imHoverImage.Picture.Bitmap.Assign(im^.Thumbnail);
  imHoverImage.Refresh;
  fullname:= im^.FullName(fSourcePaths);
  lbHoverfile.Caption:= Format('%s', [fullname]);
  if FindFirst(fullname, faAnyFile, sr) = 0 then begin
    img:= imgLoadFromFile(fullname);
    try
        lbHoverfile.Caption:= Format('%s - %dkb - %dx%dpx'+sLineBreak+
                                     '%s',
                                     [DateTimeToStr(sr.TimeStamp), sr.Size div 1024, img.Width, img.Height, fullname]);
      imHoverImage.Picture.Bitmap.LoadFromIntfImage(img);
    finally
      FreeAndNil(img);
    end;
    FindClose(sr);
  end else
    imHoverImage.Picture.Clear;
  lbClusters.Invalidate;
end;

procedure TfmMain.lbClustersDrawItem(Control: TWinControl; Index: Integer;
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
        im:= GetImageInfo(c[i]);
        sub:= Bounds(ARect.Left + 5 + i*(seThumbSize.Value+1), ARect.Top, ARect.Height, ARect.Height);
        bmp.LoadFromIntfImage(im^.Thumbnail);
        Draw(sub.Left + (sub.Width-bmp.Width) div 2, sub.Top + (sub.Height-bmp.Height) div 2, bmp);
        // ilMarks.Draw() breaks drawing the focus rect if called on the selected item; get the bitmap and Draw it instead
        bmp.Clear;
        case im^.Mark of
          imUnmarked: ;
          imDelete: ilMarks.GetBitmap(IMAGE_MARK_DELETE, bmp);
          imIgnore: ilMarks.GetBitmap(IMAGE_MARK_IGNORE, bmp);
        end;
        Draw(sub.Right-ilMarks.Width, sub.Top, bmp);
        if fHoverImage = im then
          DrawFocusRect(sub);
      end;
    finally
      FreeAndNil(bmp);
    end;
  end;
end;

procedure TfmMain.lbClustersMeasureItem(Control: TWinControl; Index: Integer;
  var AHeight: Integer);
begin
  AHeight:= seThumbSize.Value + 2;
end;

procedure TfmMain.lbClustersMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  index, i: integer;
  c: TCluster;
  im: PImageInfoItem;
begin
  if not ImageAtXY(X, Y, index, i) then
    exit;

  c:= lbClusters.Items.Objects[Index] as TCluster;
  im:= GetImageInfo(c[i]);
  case Button of
    mbLeft: if im^.Mark = imDelete then im^.Mark:= imUnmarked else im^.Mark:= imDelete;
    mbMiddle:
      try
        Screen.BeginWaitCursor;
        if ssCtrl in Shift then
          OpenFolderAndSelectFile(im^.FullName(fSourcePaths))
        else
          OpenDocument(im^.FullName(fSourcePaths));
      finally
        Screen.EndWaitCursor;
      end;
    mbRight: im^.Mark:= imIgnore;
  end;
  lbClusters.Invalidate;
end;

procedure TfmMain.tbAutoMarkClick(Sender: TObject);
begin
  frmAutoMark.ShowModal;
  lbClusters.Invalidate;
end;

procedure TfmMain.tbUnMarkClick(Sender: TObject);
var
  idx, i: Integer;
  c: TCluster;
  im: PImageInfoItem;
begin
  for idx:= 0 to lbClusters.Count - 1 do begin
    c:= lbClusters.Items.Objects[idx] as TCluster;
    for i in c do begin
      im:= GetImageInfo(i);
      if im^.Mark<>imIgnore then
        im^.Mark:= imUnmarked;
    end;
  end;
  lbClusters.Invalidate;
end;

procedure TfmMain.tbUnIgnoreClick(Sender: TObject);
var
  idx, i: Integer;
  c: TCluster;
  im: PImageInfoItem;
begin
  for idx:= 0 to lbClusters.Count - 1 do begin
    c:= lbClusters.Items.Objects[idx] as TCluster;
    for i in c do begin
      im:= GetImageInfo(i);
      if im^.Mark = imIgnore then
        im^.Mark:= imUnmarked;
    end;
  end;
  lbClusters.Invalidate;
end;

procedure TfmMain.tbMarkedTrashClick(Sender: TObject);
var
  marked: TCluster;
  idx, i: Integer;
  c: TCluster;
  im: PImageInfoItem;
  fn: String;
  names: TStringArray = nil;
begin
  marked:= TCluster.Create;
  try
    Screen.BeginWaitCursor;
    // collect marked and currently existing files
    for idx:= 0 to lbClusters.Count - 1 do begin
      c:= lbClusters.Items.Objects[idx] as TCluster;
      for i in c do begin
        im:= GetImageInfo(i);
        if im^.Mark = imDelete then begin
          fn:= im^.FullName(fSourcePaths);
          if FileExists(fn) then
            marked.Add(i);
        end;
      end;
    end;
    if marked.Count = 0 then begin
      MessageDlg('No items selected for deletion!', mtInformation, [mbOK], 0);
      exit;
    end;
    if MessageDlg(Format('%d items selected for deletion, continue?', [marked.Count]), mtInformation, mbYesNo, 0) <> mrYes then
      exit;
    // sort and collect names
    marked.Sort;
    SetLength(names, marked.Count);
    for i:= 0 to high(names) do
      names[i]:= GetImageInfo(marked[i])^.FullName(fSourcePaths);
    SendFilesToTrash(names);
    // files that were successfully deleted can be freed
    for i:= marked.Count-1 downto 0 do begin
      idx:= marked[i];
      im:= GetImageInfo(idx);
      if not FileExists(names[i]) then begin
        im^.Done;
        Delete(fImageInfos, idx, 1);
      end;
    end;
  finally
    Screen.EndWaitCursor;
    FreeAndNil(marked);
  end;
  RunClassifier;
end; 

procedure TfmMain.tbMarkInfoClick(Sender: TObject);
begin
  MessageDlg('Select images to remove or keep.' + sLineBreak +
             'LMB: toggle remove/keep' + sLineBreak +
             'RMB: ignore cluster for AutoMark' + sLineBreak +
             'MMB: open image in default app' + sLineBreak +
             'Ctrl+MMB: show in containing folder',
             mtInformation, [mbOK], 0);
end;

procedure TfmMain.FormResize(Sender: TObject);
var
  pt: TPoint;
begin
  pt:= pcSidebar.ActivePage.BoundsRect.TopLeft;
  pt:= pcSidebar.ClientToParent(pt);
  btnInfo.Width:= pt.X - GetSystemMetrics(SM_CXDLGFRAME);
  btnInfo.Height:= btnInfo.Width;
end;

procedure TfmMain.btnInfoClick(Sender: TObject);
begin
  InfoDialog1.AppTitle:= Application.Title;
  InfoDialog1.WebURL:= 'https://github.com/martok/ImageHash';
  InfoDialog1.Execute;
end;

end.

