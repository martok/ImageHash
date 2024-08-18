unit uFrmPathEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, Controls, Graphics, StdCtrls, EditBtn, Grids, Menus, fgl;

type

  { TDirectoryCellEditor }

  TDirectoryCellEditor = class(TDirectoryEdit)
  private
    FGrid: TCustomGrid;
    FCol, FRow: Integer;
  protected
    procedure msg_SetMask(var Msg: TGridMessage); message GM_SETMASK;
    procedure msg_SetValue(var Msg: TGridMessage); message GM_SETVALUE;
    procedure msg_GetValue(var Msg: TGridMessage); message GM_GETVALUE;
    procedure msg_SetGrid(var Msg: TGridMessage); message GM_SETGRID;
    procedure msg_SelectAll(var Msg: TGridMessage); message GM_SELECTALL;
    procedure msg_SetPos(var Msg: TGridMessage); message GM_SETPOS;
    procedure msg_GetGrid(var Msg: TGridMessage); message GM_GETGRID;
    procedure EditEditingDone; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditingDone; override;
    procedure RunDialog; override;
  end;

  TDirectoryEntry = class
    Path: String;
    IncludeSubdirs: boolean;
  end;

  TDirList = specialize TFPGObjectList<TDirectoryEntry>;

  { TfrmPathEditor }

  TfrmPathEditor = class(TFrame)
    ilPathList: TImageList;
    pmPathList: TPopupMenu;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miDelete: TMenuItem;
    Separator1: TMenuItem;
    miSelectAll: TMenuItem;
    DG: TDrawGrid;
    procedure DGDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DGColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
    procedure DGSelection(Sender: TObject; aCol, aRow: Integer);
    procedure DGSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure DGGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure DGSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure DGGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    procedure DGSetCheckboxState(Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
    procedure DGKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miCutClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
  private             
    fDirEditor: TDirectoryCellEditor;
    fList: TDirList;
    procedure VirtualCountUpdate;
    procedure ShowEditor(aShow: boolean);
    function InsertNew(Index: integer; Path: string; Subdirs:boolean): integer;
    function GetItemForRow(aGridRow: integer): TDirectoryEntry;
  public
    property List: TDirList read fList;
    function ValidatePaths: boolean;
    procedure Clear;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.lfm}

uses
  LCLType, LCLIntf, LMessages, FileCtrl, Clipbrd;

const
  COL_DRAG = 0;
  COL_PATH = 1;
  COL_SUB = 2;

{ TDirectoryCellEditor }

constructor TDirectoryCellEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize:= false;
  Text:= '';
  Visible:= False;
  Align:= alNone;
  BorderStyle:= bsNone;
end;

procedure TDirectoryCellEditor.msg_SetMask(var Msg: TGridMessage);
begin
  EditMask:= msg.Value;
end;

procedure TDirectoryCellEditor.msg_SetValue(var Msg: TGridMessage);
begin
  Directory:= Msg.Value;
  Modified:= false;
  SelectAll;
end;

procedure TDirectoryCellEditor.msg_GetValue(var Msg: TGridMessage);
begin
  Msg.Col:= FCol;
  Msg.Row:= FRow;
  Msg.Value:= Directory;
end;

procedure TDirectoryCellEditor.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:= Msg.Grid;
  Msg.Options:= EO_AUTOSIZE or EO_SELECTALL or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TDirectoryCellEditor.msg_SelectAll(var Msg: TGridMessage);
begin
  SelectAll;
end;

procedure TDirectoryCellEditor.msg_SetPos(var Msg: TGridMessage);
begin
  FCol:= Msg.Col;
  FRow:= Msg.Row;
end;

procedure TDirectoryCellEditor.msg_GetGrid(var Msg: TGridMessage);
begin
  Msg.Grid:= FGrid;
  Msg.Options:= EO_IMPLEMENTED;
end;

procedure TDirectoryCellEditor.EditEditingDone;
begin
  inherited EditEditingDone;
  EditingDone;
end;

procedure TDirectoryCellEditor.EditingDone;
begin
  inherited EditingDone;
  if (FGrid<>nil) and Modified then
    FGrid.EditorTextChanged(FCol, FRow, Directory);
end;

procedure TDirectoryCellEditor.RunDialog;
var
  before: String;
begin
  before:= Directory;
  inherited RunDialog;
  Modified:= Modified or (Directory <> before);
  EditingDone;
end;

{ TfrmPathEditor }

procedure TfrmPathEditor.AfterConstruction;
begin
  inherited AfterConstruction;
  fDirEditor:= TDirectoryCellEditor.Create(nil);

  fList:= TDirList.Create(true);
  VirtualCountUpdate;
end;

procedure TfrmPathEditor.BeforeDestruction;
begin             
  FreeAndNil(fList);
  FreeAndNil(fDirEditor);
  inherited BeforeDestruction;
end;

procedure TfrmPathEditor.Clear;
begin
  fList.Clear;
  VirtualCountUpdate;
end;

function TfrmPathEditor.ValidatePaths: boolean;
var
  i: integer;
begin
  Result:= true;
  // remove empty lines
  for i:= fList.Count - 1 downto 0 do begin
    if Trim(fList[i].Path) = '' then
      fList.Delete(i);
  end;
  VirtualCountUpdate;
  // any paths at all?
  if fList.Count = 0 then begin
    MessageDlg('No directory configured!', mtError, [mbOK], 0);
    DG.SetFocus;
    Result:= false;
  end;
  // alert on bad paths
  for i:= 0 to fList.Count - 1 do begin
    if not DirectoryExists(fList[i].Path) then begin
      DG.Row:= i + DG.FixedRows;
      MessageDlg('Not a directory:'+sLineBreak+fList[i].Path, mtError, [mbOK], 0);
      DG.SetFocus;
      Result:= false;
      exit;
    end;
  end;
end;

procedure TfrmPathEditor.VirtualCountUpdate;
var
  oldtop: integer;
begin
  oldtop:= DG.TopRow;
  DG.RowCount:= DG.FixedRows + fList.Count + 1;
  DG.Update;
  DG.TopRow:= oldtop;
end;

procedure TfrmPathEditor.ShowEditor(aShow: boolean);
begin
  if aShow then begin
    DG.Options:= DG.Options + [goAlwaysShowEditor];
  end else
    DG.Options:= DG.Options - [goAlwaysShowEditor];
end;

function TfrmPathEditor.InsertNew(Index: integer; Path: string; Subdirs: boolean): integer;
var
  itm: TDirectoryEntry;
begin
  itm:= TDirectoryEntry.Create;
  itm.Path:= Path;
  itm.IncludeSubdirs:= Subdirs;
  if Index < 0 then
    Result:= fList.Add(itm)
  else begin
    fList.Insert(index, itm);
    Result:= index;
  end;
end;

function TfrmPathEditor.GetItemForRow(aGridRow: integer): TDirectoryEntry;
begin
  if (aGridRow < DG.FixedRows) or (aGridRow > fList.Count) then
    Exit(nil);
  Result:= fList[aGridRow - DG.FixedRows];
end;

procedure TfrmPathEditor.DGDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  cnv: TCanvas;
  style: TTextStyle;
  itm: TDirectoryEntry;
  s: String;
begin
  cnv:= DG.Canvas;
  style:= Default(TTextStyle);
  if aRow < DG.FixedRows then
    exit;
  itm:= GetItemForRow(aRow);
  case aCol of
    COL_PATH: begin
      Inc(aRect.Left, GetSystemMetrics(SM_CXEDGE));
      if Assigned(itm) then begin
        s:= MinimizeName(itm.Path, cnv, aRect.Width);
        cnv.TextRect(aRect, aRect.Left, aRect.Top, s);
      end else begin
        cnv.Font:= CreateEmulatedTextHintFont(DG);
        cnv.TextRect(aRect, aRect.Left, aRect.Top, 'Add new line');
      end;
    end;
    COL_SUB: begin
      if not Assigned(itm) then
        cnv.FillRect(aRect);
    end;
  end;
end;

procedure TfrmPathEditor.DGMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  cell: TPoint;
  itm: TDirectoryEntry;
  newOpts: TGridOptions;
begin
  cell:= DG.MouseToCell(Point(X, Y));
  if cell.X < DG.FixedCols then begin
    itm:= GetItemForRow(cell.Y);
    newOpts:= DG.Options;
    if Assigned(itm) then begin
      if DG.EditorMode then begin
        if DG.Row<>cell.Y then
          Include(newOpts, goRowMoving);
      end else
        Include(newOpts, goRowMoving);
    end
    else
      Exclude(newOpts, goRowMoving);
    if goRowMoving in newOpts then begin
      Exclude(newOpts, goAlwaysShowEditor);
      DG.Row:= cell.Y;
    end;
    DG.Options:= newOpts;
  end;
end;

procedure TfrmPathEditor.DGColRowMoved(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer);
begin
  ShowEditor(False);

  dec(sIndex, DG.FixedRows);
  dec(tIndex, DG.FixedRows);

  if tIndex >= fList.Count then
    tIndex:= fList.Count-1;

  if tIndex <> sIndex then begin
    fList.Move(sIndex, tIndex);
    VirtualCountUpdate;
  end;
end;

procedure TfrmPathEditor.DGSelection(Sender: TObject; aCol, aRow: Integer);
begin
  ShowEditor(aRow > fList.Count);
end;

procedure TfrmPathEditor.DGSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
begin
 if aCol = COL_PATH then begin
    Editor:= fDirEditor;
  end;
end;

procedure TfrmPathEditor.DGGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
var
  itm: TDirectoryEntry;
begin
  if (ACol = COL_PATH) then begin
    itm:= GetItemForRow(aRow);
    if Assigned(itm) then begin
      Value:= itm.Path;
    end;
  end;
end;

procedure TfrmPathEditor.DGSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  ed: TDirectoryCellEditor;
  itm: TDirectoryEntry;
begin
  if DG.Editor is TDirectoryCellEditor then begin
    ed:= TDirectoryCellEditor(DG.Editor);
    if not ed.Modified then
      exit;
    if ed.Directory = '' then
      Exit;
    ed.Modified:= false;
    itm:= GetItemForRow(ed.FRow);
    if Assigned(itm) then begin
      itm.Path:= ed.Directory;
    end else begin
      // new row
      InsertNew(-1, ed.Directory, true);
      VirtualCountUpdate;
      DG.Row:= fList.Count;
      ShowEditor(true);
    end;
  end;
end;

procedure TfrmPathEditor.DGGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
const
  BoolToState: array[boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
var
  itm: TDirectoryEntry;
begin
  Value:= cbGrayed;
  itm:= GetItemForRow(ARow);
  if Assigned(itm) then begin
    case ACol of
      COL_SUB: Value:= BoolToState[itm.IncludeSubdirs];
    end;
  end;
end;

procedure TfrmPathEditor.DGSetCheckboxState(Sender: TObject; ACol, ARow: Integer; const Value: TCheckboxState);
var
  itm: TDirectoryEntry;
begin
  itm:= GetItemForRow(ARow);
  if Assigned(itm) then begin
    case ACol of
      COL_SUB: itm.IncludeSubdirs:= Value = cbChecked;
    end;
  end;
end;

procedure TfrmPathEditor.DGKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  msg: TLMKey;
begin
  msg.KeyData:= ShiftStateToKeys(Shift);
  msg.CharCode:= Key;
  if pmPathList.IsShortcut(msg) then
    Key:= 0;
end;

procedure TfrmPathEditor.miCutClick(Sender: TObject);
begin
  miCopyClick(Sender);
  miDeleteClick(Sender);
end;

procedure TfrmPathEditor.miCopyClick(Sender: TObject);
var
  items: TStringList;
  InsertPt, i: Integer;
  itm: TDirectoryEntry;
begin
  items:= TStringList.Create;
  try
    for i:= DG.FixedRows to DG.RowCount - 2 do begin
      if DG.IsCellSelected[COL_PATH, i] then begin
        itm:= GetItemForRow(i);
        items.Add(itm.Path);
      end;
    end;
    Clipboard.AsText:= items.Text;
  finally
    FreeAndNil(items);
  end;
end;

procedure TfrmPathEditor.miPasteClick(Sender: TObject);
var
  items: TStringList;
  InsertPt, i: Integer;
begin
  items:= TStringList.Create;
  try
    items.Text:= Clipboard.AsText;
    InsertPt:= DG.Row - DG.FixedRows;

    for i:= 0 to items.Count - 1 do begin
      InsertNew(InsertPt, items[i], true);
      inc(InsertPt);
    end;
    VirtualCountUpdate;
  finally
    FreeAndNil(items);
  end;
end;

procedure TfrmPathEditor.miDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  for i:= DG.RowCount - 2 downto DG.FixedRows do
    if DG.IsCellSelected[COL_PATH, i] then
      fList.Delete(i - DG.FixedRows);
  VirtualCountUpdate;
end;

procedure TfrmPathEditor.miSelectAllClick(Sender: TObject);
begin
  VirtualCountUpdate;
  DG.ClearSelections;
  DG.Row:= 0;
  DG.Selection:= Rect(0, DG.FixedRows, DG.ColCount, DG.RowCount - 2);
end;

end.

