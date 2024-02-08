unit uFrmPathEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, Controls, StdCtrls, EditBtn;

type

  { TfrmPathEditor }

  TfrmPathEditor = class(TFrame)
    mePaths: TMemo;
    dePaths: TDirectoryEdit;
    procedure dePathsAcceptDirectory(Sender: TObject; var Value: String);
    procedure mePathsChange(Sender: TObject);
    procedure mePathsClick(Sender: TObject);
    procedure mePathsKeyPress(Sender: TObject; var Key: char);
    procedure mePathsEditingDone(Sender: TObject);
    procedure mePathsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    function GetItems: TStrings; inline;
    procedure SetLine(Line: integer);
    function GetLine(out Line: integer): boolean;
    procedure SelectLine(Line: integer);
    procedure CursorMoved;
  public
    property Items: TStrings read GetItems;
    function ValidatePaths: boolean;
    procedure Clear;
    procedure Add(Path: string);
    procedure AfterConstruction; override;
  end;

implementation

{$R *.lfm}

uses
  LCLType;

{ TfrmPathEditor }

procedure TfrmPathEditor.AfterConstruction;
begin
  inherited AfterConstruction;
end;

function TfrmPathEditor.GetItems: TStrings;
begin
  Result:= mePaths.Lines;
end;

procedure TfrmPathEditor.Clear;
begin
  mePaths.Clear;
end;

procedure TfrmPathEditor.Add(Path: string);
begin
  SelectLine(mePaths.Lines.Add(Path));
end;

procedure TfrmPathEditor.SetLine(Line: integer);
begin
  if (0<=Line) and (Line<Items.Count) then begin
    mePaths.CaretPos:= Point(0, Line);
    CursorMoved;
  end;
end;

function TfrmPathEditor.GetLine(out Line: integer): boolean;
begin
  Line:= mePaths.CaretPos.Y;
  Result:= (0 <= Line) and (Line < Items.Count);
end;

procedure TfrmPathEditor.SelectLine(Line: integer);
var
  se, ss: Integer;
begin
  mePaths.SelLength:= 0;
  mePaths.SelStart:= Length(mePaths.Text);
  if (0<=Line) and (Line<Items.Count) then begin
    mePaths.CaretPos:= Point(Length(Items[Line]), Line);
    se:= mePaths.SelStart;
    mePaths.CaretPos:= Point(0, Line);
    ss:= mePaths.SelStart;
    mePaths.SelLength:= se - ss;  
    CursorMoved;
  end;
end;

function TfrmPathEditor.ValidatePaths: boolean;
var
  i: integer;
begin
  Result:= true;
  // remove empty lines
  Items.BeginUpdate;
  try
    for i:= Items.Count - 1 downto 0 do begin
      Items[i]:= Trim(Items[i]);
      if Items[i] = '' then
        Items.Delete(i);
    end;
  finally
    Items.EndUpdate;
  end;
  // alert on bad paths
  for i:= 0 to Items.Count - 1 do begin
    if not DirectoryExists(Items[i]) then begin
      SelectLine(i);
      MessageDlg('Not a directory:'+sLineBreak+Items[i], mtError, [mbOK], 0);
      mePaths.SetFocus;
      Result:= false;
      exit;
    end;
  end;
end;

procedure TfrmPathEditor.CursorMoved;
var
  y: integer;
begin
  if GetLine(y) then
    dePaths.Directory:= Items[y];
end;

procedure TfrmPathEditor.dePathsAcceptDirectory(Sender: TObject; var Value: String);
var
  y: integer;
begin
  if GetLine(y) then
    Items[y]:= Value
  else
    Items.Add(Value);
  SelectLine(y);
end;

procedure TfrmPathEditor.mePathsChange(Sender: TObject);
begin
  CursorMoved;
end;

procedure TfrmPathEditor.mePathsClick(Sender: TObject);
begin
  CursorMoved;
end;

procedure TfrmPathEditor.mePathsKeyPress(Sender: TObject; var Key: char);
begin
  CursorMoved;
end;

procedure TfrmPathEditor.mePathsEditingDone(Sender: TObject);
var
  y: integer;
begin
  CursorMoved;           
  if GetLine(y) then
    SelectLine(y);
end;

procedure TfrmPathEditor.mePathsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  y: integer;
begin
  case Key of
    VK_DELETE: begin
      if [ssCtrl, ssShift] = Shift then begin
        if GetLine(y) then
          mePaths.Lines.Delete(y);
      end;
    end;
    VK_ESCAPE: begin
      if GetLine(y) then
        SelectLine(y);
    end;
    VK_UP,
    VK_DOWN: begin     
      if [ssCtrl, ssShift] = Shift then begin
        if GetLine(y) then begin
          if (y > 0) and (Key = VK_UP) then begin
            mePaths.Lines.Exchange(y, y-1);
            dec(y);
          end else
          if (y < mePaths.Lines.Count - 1) and (Key = VK_DOWN) then begin
            mePaths.Lines.Exchange(y, y+1);
            inc(y);
          end;
          SelectLine(y);
        end;
      end;
    end;
  end;
end;

end.

