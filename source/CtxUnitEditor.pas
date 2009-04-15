(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Designer form for TCtxUnit component. Can be used in design & run time.
(*
(*  Contains:
(*                TfrmCtxUnitEditor = class(TForm)
(*                procedure EditCtxUnit(CtxUnit: TCtxUnit);
(*
(*  Copyright (c) 2005 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : CtxUnitEditor.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 1.2
(*  DELPHI      : Delphi 5,6,7,2005
(*  ------------------------------------------------------------
(*  HISTORY     :
(*    2/14/2005    v1.2    	Released
(*
(*    No changes to this file since v1.2.
(*
(******************************************************************************)
unit CtxUnitEditor;

interface

uses
  Windows, Messages, SysUtils, {$IFnDEF VER130} Variants, {$ENDIF} Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, ToolWin,
  ImgList, ActnList, CtxScript, CtxUnit, Menus;

type
  TfrmCtxUnitEditor = class(TForm)
    tvFields: TTreeView;
    Splitter1: TSplitter;
    edtCode: TMemo;
    ToolBar1: TToolBar;
    StatusBar1: TStatusBar;
    Images: TImageList;
    ToolButton1: TToolButton;
    Actions: TActionList;
    actClose: TAction;
    actSaveAs: TAction;
    actOpen: TAction;
    actNewMethod: TAction;
    actDeleteMethod: TAction;
    actCompile: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    PopupMenu: TPopupMenu;
    NewMethod1: TMenuItem;
    Delete1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    actSave: TAction;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    procedure actCloseExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actNewMethodExecute(Sender: TObject);
    procedure tvFieldsDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actCompileExecute(Sender: TObject);
    procedure actDeleteMethodExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FCtxUnit: TCtxUnit;
    FMethod: TCtxScriptCode;
    procedure SaveChanges;
    procedure FillTreeView;
    procedure SelectMethod;
  end;

  procedure EditCtxUnit(CtxUnit: TCtxUnit);

resourcestring
  SConfirmDelete = 'Delete method %s?';

implementation

{$R *.dfm}

procedure EditCtxUnit(CtxUnit: TCtxUnit);
begin
  with TfrmCtxUnitEditor.Create(nil) do
  try
    FCtxUnit := CtxUnit;
    ShowModal;
  finally
    Free;
  end;
end;

{ TfrmCtxUnitEditor }

procedure TfrmCtxUnitEditor.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmCtxUnitEditor.actSaveAsExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    FCtxUnit.SaveToFile(SaveDialog.FileName);
end;

procedure TfrmCtxUnitEditor.actOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FCtxUnit.LoadFromFile(OpenDialog.FileName);
    FMethod := nil;
    FillTreeView;
  end;
end;

procedure TfrmCtxUnitEditor.actNewMethodExecute(Sender: TObject);
begin
  SaveChanges;
  FMethod := FCtxUnit.AddMethod('NewMethod');
  FillTreeView;
end;

procedure TfrmCtxUnitEditor.SaveChanges;
begin
  if FMethod = nil then exit;
  FMethod.Code := edtCode.Text;
  FMethod.ParseHeader;
end;

procedure TfrmCtxUnitEditor.FillTreeView;
var
  I: Integer;
  NewSel, N, ClassNode: TTreeNode;
begin
  tvFields.Items.BeginUpdate;
  try
    tvFields.Items.Clear;
    NewSel := nil;
    ClassNode := tvFields.Items.Add(nil, FCtxUnit.UnitName);
    ClassNode.ImageIndex := 7;
    ClassNode.SelectedIndex := 7;
    for I := 0 to FCtxUnit.MethodCount - 1 do
    begin
      N := tvFields.Items.AddChildObject(ClassNode,
        FCtxUnit.Methods[I].ScriptName, FCtxUnit.Methods[I]);
      N.ImageIndex := 5;
      N.SelectedIndex := 5;
      if FCtxUnit.Methods[I] = FMethod then
        NewSel := N;
    end;
    tvFields.Selected := NewSel;
    tvFields.FullExpand;
  finally
    tvFields.Items.EndUpdate;
  end;
  SelectMethod;
end;

procedure TfrmCtxUnitEditor.tvFieldsDblClick(Sender: TObject);
begin
  SaveChanges;
  SelectMethod;
  FillTreeView;
end;

procedure TfrmCtxUnitEditor.SelectMethod;
begin
  if Assigned(tvFields.Selected) and Assigned(tvFields.Selected.Data) then
  begin
    FMethod := TCtxScriptCode(tvFields.Selected.Data);
    edtCode.Text := FMethod.Code;
    edtCode.Enabled := True;
  end else begin
    edtCode.Text := '';
    edtCode.Enabled := False;
  end;
end;

procedure TfrmCtxUnitEditor.FormShow(Sender: TObject);
begin
  if FCtxUnit.MethodCount > 0 then
    FMethod := FCtxUnit.Methods[0];
  FillTreeView;
end;

procedure TfrmCtxUnitEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  SaveChanges;
end;

procedure TfrmCtxUnitEditor.actCompileExecute(Sender: TObject);
begin
  if FMethod = nil then exit;
  SaveChanges;  
  FMethod.ResetPCode;
  try
    FMethod.Compile;
  except
    on E: ECtxScriptCompileError do
    begin
      edtCode.SetFocus;
      {$IFnDEF VER130}
      edtCode.CaretPos := Point(E.Pos - 1, E.Line - 1);
      {$ENDIF}
      raise;
    end else
      raise;
  end;
end;

procedure TfrmCtxUnitEditor.actDeleteMethodExecute(Sender: TObject);
var
  M: TCtxScriptCode;
begin
  if Assigned(tvFields.Selected) and Assigned(tvFields.Selected.Data) then
  begin
    M := TCtxScriptCode(tvFields.Selected.Data);
    if MessageDlg(Format(SConfirmDelete, [M.ScriptName]), mtConfirmation,
      [mbOk, mbCancel], 0) <> mrOk
    then exit;
    if FMethod = M then
      FMethod := nil;
    FCtxUnit.DeleteMethod(M);
    FillTreeView;
  end;
end;

procedure TfrmCtxUnitEditor.actSaveExecute(Sender: TObject);
begin
  SaveChanges;
  FillTreeView;
end;

end.
