unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CtxScript, ActnList, Menus, ImgList, ToolWin,
  ComCtrls, ExtCtrls, SynEditHighlighter, IniFiles, Contnrs,
{$IFnDEF VER130}
  Variants,
{$ENDIF}
  SynHighlighterPas, SynEdit, SynAutoCorrect, SynEditPrint, StdCtrls,
  CtxUnit, CtxPkgClasses, CtxPkgSysUtils;

type
  TBreakpoint = class
  protected
    FMethod: TCtxScriptCode;
    FLineNo: Integer;
  public
    constructor Create(Method: TCtxScriptCode; LineNo: Integer);
    property Method: TCtxScriptCode read FMethod;
    property LineNo: Integer read FLineNo;
  end;

  TfrmCtxDemoIDE = class(TForm)
    ToolBar: TToolBar;
    MainMenu: TMainMenu;
    Images: TImageList;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    Actions: TActionList;
    actExit: TAction;
    actOpen: TAction;
    actSave: TAction;
    pnlDemos: TPanel;
    actPrint: TAction;
    tvFields: TTreeView;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    StatusBar: TStatusBar;
    SynAutoCorrect: TSynAutoCorrect;
    SynEditPrint: TSynEditPrint;
    SynPasSyn: TSynPasSyn;
    Script1: TMenuItem;
    Run1: TMenuItem;
    Stop1: TMenuItem;
    N3: TMenuItem;
    TriggerBreakpoint1: TMenuItem;
    TraceInto1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    actAbout: TAction;
    actRun: TAction;
    actScriptReset: TAction;
    actPause: TAction;
    actTraceInto: TAction;
    actStepOver: TAction;
    actBreakpoint: TAction;
    StepOver1: TMenuItem;
    ToolButton4: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    actSaveAs: TAction;
    pnlMain: TPanel;
    SynEdit: TSynEdit;
    Splitter1: TSplitter;
    actCompile: TAction;
    N4: TMenuItem;
    Compile1: TMenuItem;
    Splitter: TSplitter;
    actAddWatch: TAction;
    actDeleteWatch: TAction;
    Debug1: TMenuItem;
    AddWatch1: TMenuItem;
    DeleteWatch1: TMenuItem;
    popWatch: TPopupMenu;
    AddWatch2: TMenuItem;
    DeleteWatch2: TMenuItem;
    tsDebug: TPageControl;
    tabAsm: TTabSheet;
    edtASM: TMemo;
    tabWatch: TTabSheet;
    lbxWatch: TListBox;
    CtxScript: TCtxScript;
    actNew: TAction;
    actDelete: TAction;
    Demos1: TMenuItem;
    NewScript1: TMenuItem;
    DeleteScript1: TMenuItem;
    tabBreakpoints: TTabSheet;
    lbxBreakPoints: TListBox;
    TabSheet1: TTabSheet;
    lbxCallStack: TListBox;
    CtxUnit: TCtxUnit;
    CtxPkgSysUtils1: TCtxPkgSysUtils;
    CtxPkgClasses1: TCtxPkgClasses;
    procedure actExitExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure actBreakpointExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure actScriptResetExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actRunExecute(Sender: TObject);
    procedure actTraceIntoExecute(Sender: TObject);
    procedure SynEditGutterClick(Sender: TObject; Button: TMouseButton; X, Y, Line: Integer;
      mark: TSynEditMark);
    procedure actStepOverExecute(Sender: TObject);
    procedure actAddWatchExecute(Sender: TObject);
    procedure actDeleteWatchExecute(Sender: TObject);
    procedure CtxScriptDebugHook(Sender: TCtxScript; Exception: Boolean);
    procedure actNewExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tvFieldsDblClick(Sender: TObject);
    procedure lbxBreakPointsDblClick(Sender: TObject);
    procedure lbxCallStackClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FileName: String;
    Parameters: array of Variant;
    FMethod: TCtxScriptCode;

    ExecPoint: TSynEditMark;
    FBreakpoints: TObjectList;

    function IsSynEditBreakPoint(Line: Integer): Boolean;
    procedure RemoveBreakPoint(Line: Integer);
    procedure AddBreakPoint(Line: Integer);

    procedure DeleteBreakPoint(Method: TCtxScriptCode; Line: Integer);
    procedure SetupBreakpoints;
    function IsBreakpoint(Method: TCtxScriptCode; Line: Integer): Boolean;

    procedure OpenFile(const FileName: String);
    function SaveCurrentMethod: Boolean;
    procedure SaveChanges;
    procedure FillTreeView;
    procedure UpdateCallStack;
    procedure SelectMethod;
    procedure Locate(AMethod: TCtxScriptCode; LineNo: Integer);
    procedure UpdateBreakPoints;


    procedure DebugRun(TraceMode, TraceInto: Boolean);
    procedure UpdateWatches;
    procedure ResetWatches;
    procedure ClearWatches;
  end;

var
  frmCtxDemoIDE: TfrmCtxDemoIDE;

implementation

{$R *.DFM}

{ TBreakpoint }

constructor TBreakpoint.Create(Method: TCtxScriptCode; LineNo: Integer);
begin
  inherited Create;
  FMethod := Method;
  FLineNo := LineNo;
end;

{ TfrmCtxDemoIDE }

procedure TfrmCtxDemoIDE.FormCreate(Sender: TObject);
begin
  CtxScript.GlobalScope := Self;
  FBreakpoints := TObjectList.Create;
  FMethod := nil;
  ExecPoint := TSynEditMark.Create(SynEdit);
  ExecPoint.ImageIndex := 8;
  ExecPoint.Visible := False;
  ExecPoint.Line := 0;
  SynEdit.Marks.Add(ExecPoint);
end;

procedure TfrmCtxDemoIDE.FormDestroy(Sender: TObject);
begin
  ClearWatches;
  FBreakPoints.Free;
end;

procedure TfrmCtxDemoIDE.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  SaveChanges;
end;

procedure TfrmCtxDemoIDE.FormShow(Sender: TObject);
begin
  OpenFile(ChangeFileExt(Application.ExeName, '.csu'));
end;

procedure TfrmCtxDemoIDE.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmCtxDemoIDE.actAboutExecute(Sender: TObject);
begin
  // Show About Box
  ShowMessage('Context Scripting Demo IDE'#13#10#13#10'Copyright (c) 2005, Michael Baytalsky');
end;

procedure TfrmCtxDemoIDE.FillTreeView;
var
  I: Integer;
  NewSel, N, ClassNode: TTreeNode;
begin
  tvFields.Items.BeginUpdate;
  try
    tvFields.Items.Clear;
    NewSel := nil;
    ClassNode := tvFields.Items.Add(nil, CtxUnit.UnitName);
    ClassNode.ImageIndex := 10;
    ClassNode.SelectedIndex := 10;
    for I := 0 to CtxUnit.MethodCount - 1 do
    begin
      N := tvFields.Items.AddChildObject(ClassNode,
        CtxUnit.Methods[I].ScriptName, CtxUnit.Methods[I]);
      N.ImageIndex := 11;
      N.SelectedIndex := 11;
      if CtxUnit.Methods[I] = FMethod then
        NewSel := N;
    end;
    tvFields.Selected := NewSel;
    tvFields.FullExpand;
  finally
    tvFields.Items.EndUpdate;
  end;
  SelectMethod;
end;

procedure TfrmCtxDemoIDE.OpenFile(const FileName: String);
begin
  Self.FileName := FileName;
  FMethod := nil;
  FBreakPoints.Clear;
  CtxUnit.LoadFromFile(FileName);
  if CtxUnit.UnitName = '' then
    CtxUnit.UnitName := ChangeFileExt(ExtractFileName(FileName), '');
  FillTreeView;
  Caption := 'Context Scripting - ' + ExtractFileName(FileName);
end;

procedure TfrmCtxDemoIDE.actOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    OpenFile(OpenDialog.FileName);
end;

procedure TfrmCtxDemoIDE.actSaveExecute(Sender: TObject);
begin
  SaveChanges;
end;

procedure TfrmCtxDemoIDE.SaveChanges;
begin
  SaveCurrentMethod;
  // Save class to file
  CtxUnit.UnitName := ChangeFileExt(ExtractFileName(FileName), '');
  CtxUnit.SaveToFile(FileName);
  FillTreeView;
end;

procedure TfrmCtxDemoIDE.actSaveAsExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    SaveChanges;
    Caption := 'Context Scripting - ' + FileName;
  end;
end;

function TfrmCtxDemoIDE.SaveCurrentMethod: Boolean;
begin
  Result := (FMethod <> nil) and SynEdit.Modified;
  if not Result then exit;
  FMethod.Code := SynEdit.Text;
  FMethod.ParseHeader;
  SynEdit.Modified := False;
end;

procedure TfrmCtxDemoIDE.tvFieldsDblClick(Sender: TObject);
var
  MethodChanged: Boolean;
begin
  MethodChanged := SaveCurrentMethod;
  SelectMethod;
  if MethodChanged then
    FillTreeView;
end;

procedure TfrmCtxDemoIDE.SelectMethod;
begin
  if Assigned(tvFields.Selected) and Assigned(tvFields.Selected.Data) then
  begin
    FMethod := TCtxScriptCode(tvFields.Selected.Data);
    SynEdit.Text := FMethod.Code;
    SetupBreakpoints;
    SynEdit.Enabled := True;
  end else begin
    SynEdit.Text := '';
    SynEdit.Enabled := False;
    FMethod := nil;
  end;
  SynEdit.Modified := False;
end;

procedure TfrmCtxDemoIDE.Locate(AMethod: TCtxScriptCode; LineNo: Integer);
begin
  FMethod := AMethod;
  FillTreeView;
  SynEdit.CaretY := LineNo;
  SynEdit.SetFocus;
end;

procedure TfrmCtxDemoIDE.actPrintExecute(Sender: TObject);
begin
  SynEditPrint.Lines.Assign(SynEdit.Lines);
  SynEditPrint.Print;
end;

procedure TfrmCtxDemoIDE.SynEditSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  // if Current
  if ExecPoint.Visible and (Line = ExecPoint.Line) and (FMethod = CtxScript.PCode) then
  begin
    Special := True;
    BG := clNavy;
    FG := clWhite;
  end else
  // if bookmarked then red
  if IsSynEditBreakPoint(Line) then
  begin
    Special := True;
    BG := clRed;
    FG := clWhite;
  end;
end;

procedure TfrmCtxDemoIDE.AddBreakPoint(Line: Integer);
var
  BP: TSynEditMark;
begin
  BP := TSynEditMark.Create(SynEdit);
  BP.ImageIndex := 9;
  BP.Visible := True;
  BP.Line := Line;
  SynEdit.Marks.Add(BP);
end;

procedure TfrmCtxDemoIDE.RemoveBreakPoint(Line: Integer);
var
  Marks: TSynEditMarks;
  I: Integer;
begin
  SynEdit.Marks.GetMarksForLine(Line, Marks);
  for I := 1 to MAX_MARKS do
  begin
    if Marks[I] = nil then break;
    if Marks[I].ImageIndex = 9 then
      SynEdit.Marks.Remove(Marks[I]);
  end;
end;

procedure TfrmCtxDemoIDE.actBreakpointExecute(Sender: TObject);
var
  LineNo: Integer;
begin
  if FMethod = nil then exit;
  
  LineNo := SynEdit.CaretY;
  if not IsSynEditBreakPoint(LineNo) then
  begin
    AddBreakPoint(LineNo);
    FBreakpoints.Add(TBreakPoint.Create(FMethod, LineNo));
  end else begin
    RemoveBreakPoint(LineNo);
    DeleteBreakPoint(FMethod, LineNo);
  end;
  SynEdit.InvalidateLine(SynEdit.CaretY);
  SynEdit.Modified := True;
  UpdateBreakPoints;
end;

procedure TfrmCtxDemoIDE.actNewExecute(Sender: TObject);
begin
  SaveChanges;
  FMethod := CtxUnit.AddMethod('NewMethod');
  FillTreeView;
end;

procedure TfrmCtxDemoIDE.actDeleteExecute(Sender: TObject);
var
  M: TCtxScriptCode;
begin
  if Assigned(tvFields.Selected) and Assigned(tvFields.Selected.Data) then
  begin
    M := TCtxScriptCode(tvFields.Selected.Data);
    if MessageDlg(Format('Delete %s?', [M.ScriptName]), mtConfirmation,
      [mbOk, mbCancel], 0) <> mrOk
    then exit;

    DeleteBreakpoint(FMethod, -1); // Delete break points
    if FMethod = M then
      FMethod := nil;
    CtxUnit.DeleteMethod(M);
    FillTreeView;
  end;
end;

procedure TfrmCtxDemoIDE.actCompileExecute(Sender: TObject);
begin
  // Compile script -> Debug output to ASM window
  if FMethod = nil then exit;
  if SaveCurrentMethod then
    FillTreeView;
  try
    FMethod.Compile;
  except
    on E: ECtxScriptCompileError do
    begin
      SynEdit.CaretY := E.Line;
      SynEdit.CaretX := E.Pos;
      SynEdit.SetFocus;
      raise;
    end else
      raise;
  end;

  ResetWatches;
  edtASM.Lines.Text := FMethod.GetDebugText;
end;

procedure TfrmCtxDemoIDE.actScriptResetExecute(Sender: TObject);
begin
  CtxScript.Terminate;
end;

procedure TfrmCtxDemoIDE.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  // Setup actions related to script execution depending on script's state
  actScriptReset.Enabled := CtxScript.Suspended;
  actCompile.Enabled := not CtxScript.Suspended;
  Handled := True;
end;

function TfrmCtxDemoIDE.IsSynEditBreakPoint(Line: Integer): Boolean;
var
  I: Integer;
  Marks: TSynEditMarks;
begin
  SynEdit.Marks.GetMarksForLine(Line, Marks);
  Result := False;
  for I := 1 to MAX_MARKS do
  begin
    if Marks[I] = nil then exit;
    if Marks[I].ImageIndex = 9 then
    begin
      Result := True;
      exit;
    end;
  end;
end;

procedure TfrmCtxDemoIDE.actRunExecute(Sender: TObject);
begin
  if CtxScript.Suspended then
    CtxScript.Resume(False, False)
  else DebugRun(False, False);
end;

procedure TfrmCtxDemoIDE.actTraceIntoExecute(Sender: TObject);
begin
  if CtxScript.Suspended then
    CtxScript.Resume(True, True)
  else DebugRun(True, True);
end;

procedure TfrmCtxDemoIDE.actStepOverExecute(Sender: TObject);
begin
  if CtxScript.Suspended then
    CtxScript.Resume(True, False)
  else DebugRun(True, False);
end;

procedure TfrmCtxDemoIDE.DebugRun(TraceMode, TraceInto: Boolean);
var
  StartTime: Cardinal;
  I: Integer;
  S: String;
  ExecMethod: TCtxScriptCode;
begin
  if FMethod = nil then exit;
  if CtxScript.Suspended then exit;

  ExecMethod := FMethod;
  ExecPoint.Visible := False;
  SynEdit.InvalidateLine(ExecPoint.Line);
  actCompileExecute(nil);
  CtxScript.TraceMode := TraceMode;
  CtxScript.TraceInto := TraceInto;
  // Setup parameters
  with ExecMethod.Symbols do
  begin
    SetLength(Parameters, Count);

    for I := 0 to Count - 1 do
      if Items[I].SymbolType in [stInParam, stVarParam] then
      begin
        Parameters[I] := InputBox('Enter Parameter', Items[I].SymbolName,'');
        if Items[I].SymbolType = stVarParam then
          CtxScript.Push(VarRef(@Parameters[I]))
        else CtxScript.Push(Parameters[I]);
      end else if Items[I].SymbolType = stOutParam then
        CtxScript.Push(VarRef(@Parameters[I]));
  end;
  StartTime := GetTickCount;
  try
    // Execute method
    CtxScript.Execute(ExecMethod, CtxUnit);
  finally
    StartTime := GetTickCount - StartTime;
    StatusBar.SimpleText := Format('Time elapsed: %d msec', [StartTime]);
    UpdateWatches;
    FillTreeView;
    lbxCallStack.Clear;
    ExecPoint.Visible := False;
    SynEdit.InvalidateLine(ExecPoint.Line);
  end;
  // Display results is any
  S := '';
  with ExecMethod.Symbols do
    for I := 0 to Count - 1 do
      if Items[I].SymbolType in [stOutParam, stVarParam] then
        S := S + Items[I].SymbolName + ' = ' + VarToStrEx(Parameters[I]) + #13#10;
  if S <> '' then
    ShowMessage(S);
end;

procedure TfrmCtxDemoIDE.CtxScriptDebugHook(Sender: TCtxScript;
  Exception: Boolean);
begin
  if Exception or IsBreakpoint(CtxScript.PCode, CtxScript.LineNo)
    or CtxScript.TraceMode then
  begin
    // Display method being executed
    if FMethod <> CtxScript.PCode then
    begin
      FMethod := CtxScript.PCode;
      SynEdit.Lines.Text := FMethod.Code;
      edtASM.Lines.Text := FMethod.GetDebugText;
      SetupBreakpoints;
    end;
    // Upate call stack
    UpdateCallStack;
    // Turn on marking
    ExecPoint.Line := CtxScript.LineNo;
    ExecPoint.Visible := True;
    SynEdit.InvalidateLine(ExecPoint.Line);
    SynEdit.CaretY := CtxScript.LineNo;
    // Wait to resume
    Sender.Suspend;
    // Evaluate watch expressions
    UpdateWatches;
    // Wait to resume
    while Sender.Suspended and not Application.Terminated do
      Application.ProcessMessages;
    if Application.Terminated then
      Sender.Terminate;
    // Turn off marking and resume
    ExecPoint.Visible := False;
    SynEdit.InvalidateLine(ExecPoint.Line);
  end;
end;

procedure TfrmCtxDemoIDE.SynEditGutterClick(Sender: TObject; Button: TMouseButton; X, Y,
  Line: Integer; mark: TSynEditMark);
begin
  if Button = mbLeft then
  begin
    SynEdit.CaretY := Line;
    actBreakpointExecute(Self);
  end;
end;

procedure TfrmCtxDemoIDE.actAddWatchExecute(Sender: TObject);
var
  S: String;
  W: TCtxScriptCode;
begin
  S := InputBox('Watch expression', 'Expression: ', '');
  if S = '' then exit;
  W := TCtxScriptCode.Create;
  W.Code := S;
  W.IsExpression := True;
  lbxWatch.Items.AddObject(W.Code + ': ', W);
  UpdateWatches;
end;

procedure TfrmCtxDemoIDE.actDeleteWatchExecute(Sender: TObject);
begin
  with lbxWatch do
  if ItemIndex >= 0 then
  begin
    Items.Objects[ItemIndex].Free;
    Items.Delete(ItemIndex);
  end;
end;

procedure TfrmCtxDemoIDE.ClearWatches;
begin
  with lbxWatch do
  while Items.Count > 0 do
  begin
    Items.Objects[Items.Count - 1].Free;
    Items.Delete(Items.Count - 1);
  end;
end;

procedure TfrmCtxDemoIDE.UpdateWatches;
var
  I: Integer;
  W: TCtxScriptCode;
  S: String;
begin
  with lbxWatch do
  for I := 0 to Items.Count - 1 do
  begin
    W := TCtxScriptCode(Items.Objects[I]);
    if CtxScript.Suspended then
    try
      W.CodeScope := FMethod;
      S := VarToStrEx(CtxScript.Execute(W));
    except
      S := '[invalid value]';
    end else
      S := '[process not accessible]';
    Items[I] := W.Code + ': ' + S;
  end;
end;

procedure TfrmCtxDemoIDE.ResetWatches;
var
  I: Integer;
begin
  with lbxWatch do
  for I := 0 to Items.Count - 1 do
  with TCtxScriptCode(Items.Objects[I]) do
    ResetPCode;
end;

procedure TfrmCtxDemoIDE.SetupBreakpoints;
var
  I: Integer;
begin
  // Setup marks from breakpoints for this method
  for I := 0 to SynEdit.Lines.Count - 1 do
  begin
    if IsBreakPoint(FMethod, I) then
    begin
      if not IsSynEditBreakPoint(I) then
        AddBreakPoint(I);
    end else begin
      if IsSynEditBreakPoint(I) then
        RemoveBreakPoint(I);
    end;
  end;
  ExecPoint.Visible := FMethod = CtxScript.PCode;
end;

function TfrmCtxDemoIDE.IsBreakpoint(Method: TCtxScriptCode;
  Line: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FBreakpoints.Count - 1 do
    if (TBreakpoint(FBreakpoints[I]).Method = Method)
      and (TBreakpoint(FBreakpoints[I]).LineNo = Line)
    then exit;
  Result := False;
end;

procedure TfrmCtxDemoIDE.UpdateBreakPoints;
var
  I: Integer;
begin
  lbxBreakPoints.Items.BeginUpdate;
  try
    lbxBreakPoints.Items.Clear;
    for I := 0 to FBreakPoints.Count - 1 do
    with TBreakPoint(FBreakPoints[I]) do
      lbxBreakPoints.Items.AddObject(
        Format('%s at %d', [Method.ScriptName, LineNo]),
        FBreakPoints[I]);
  finally
    lbxBreakPoints.Items.EndUpdate;
  end;
end;

procedure TfrmCtxDemoIDE.DeleteBreakPoint(Method: TCtxScriptCode;
  Line: Integer);
var
  I: Integer;
begin
  // Reload all breakpoints for this method from Marks
  I := 0;
  while I < FBreakpoints.Count do
    if (TBreakpoint(FBreakpoints[I]).Method = Method)
      and ((Line < 0) or (TBreakpoint(FBreakpoints[I]).LineNo = Line))
    then FBreakpoints.Delete(I)
    else Inc(I);
end;

procedure TfrmCtxDemoIDE.UpdateCallStack;
var
  I: Integer;
begin
  lbxCallStack.Items.BeginUpdate;
  try
    lbxCallStack.Clear;
    for I := 0 to CtxScript.StateStackSize - 1 do
    with CtxScript.ScriptState[I] do
      lbxCallStack.Items.Add(Format('%s, Line: %d',
        [PCode.ScriptName, LineNo]));
  finally
    lbxCallStack.Items.EndUpdate;
  end;
end;

procedure TfrmCtxDemoIDE.lbxBreakPointsDblClick(Sender: TObject);
begin
  // Position at breakpoint
  if lbxBreakPoints.ItemIndex < 0 then exit;
  with lbxBreakPoints do
    with TBreakPoint(Items.Objects[ItemIndex]) do
      Locate(Method, LineNo);
end;

procedure TfrmCtxDemoIDE.lbxCallStackClick(Sender: TObject);
begin
  if lbxCallStack.ItemIndex < 0 then exit;
  with CtxScript.ScriptState[lbxCallStack.ItemIndex] do
    Locate(PCode, LineNo);
end;

{ Ctx Methods imlpementation }

procedure CtxShowMessage(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  ShowMessage(VarToStr(Sender.Pop));
end;

procedure CtxTDateTime(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarFromDateTime(TDateTime(Sender.Pop));
end;

procedure CtxLength(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := Length(VarToStr(Sender.Pop));
end;

procedure CtxCalcValue(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  SetVarByRef(Sender.Pop, 'Assigned Value!');
end;

initialization
  { Ctx Methods declaration }
  with TCtxCustomIntrospector.Create(TCtxSystemScope) do
  begin
    AddMethod('TDateTime', @CtxTDateTime, 1);
    AddMethod('ShowMessage', @CtxShowMessage, 1);
    AddMethod('Length', @CtxLength, 1);
    AddMethod('CalcValue', @CtxCalcValue, 1);
  end;
end.


