unit CtxSyntaxHighlighter;

interface

{.$DEFINE USE_SYNTAX_HIGHLIGHT}

uses Classes, Controls, StdCtrls, Graphics, Forms, Dialogs, CtxScript;

type
  PComponent = ^TComponent;

  procedure ReplaceMemo(Owner: TForm; Memo: TMemo);
  function  GetLines(Component: TComponent): TStrings;
  function  GetText(Component: TComponent): String;
  function  GetEditModified(Component: TComponent): boolean;
  procedure SetEditModified(Component: TComponent; Value: boolean);

  procedure SetContext(Component: TComponent; AScripting: TCtxScript; AnInstance: TObject = nil);
  function  GetIdent(Component: TComponent): string;

  function  DoSearchText(Component: TComponent; const SearchText: String;
    Options: TFindOptions; FindFirst: Boolean = False): Boolean;


implementation

{$IFDEF USE_SYNTAX_HIGHLIGHT}
uses
  Windows, SysUtils, SynEditHighlighter, SynHighlighterPas, SynEdit,
  SynEditKeyCmds, SynCompletionProposal, SynEditTypes, SynEditSearch;

type
  TCtxSynCompletionProposal = class (TSynCompletionProposal)
  protected
    FScripting: TCtxScript;
    FInstance: TObject;
  public
    procedure SynCompletionProposalExecute(
      Kind: SynCompletionType; Sender: TObject; var AString: String; var x,
      y: Integer; var CanExecute: Boolean);
    procedure SynCompletionProposalPaintItem(Sender: TObject; Index: Integer;
      ACanvas: TCanvas; Rect: TRect; var aCustomDraw: boolean);
  end;

function ExtractObjectName(const Value: String; var LeftPart: String): String;
var
  P: Integer;
  Quote: Char;
begin
  // Remove all quotes and brackets and schema name if there
  LeftPart := Trim(Value);
  Result := '';
  if LeftPart = '' then exit;
  // Extract quoted or unquoted item from the end till period
  P := Length(LeftPart);
  if LeftPart[P] in ['"', '`', ']'] then
  begin
    if LeftPart[P] = ']' then
      Quote := '['
    else Quote := LeftPart[P];
    Dec(P);
    while (P > 0) and (LeftPart[P] <> Quote) do
      Dec(P);
    Result := copy(LeftPart, P + 1, Length(LeftPart) - P - 1);
    Dec(P);
    LeftPart := copy(LeftPart, 1, P);
    if (P > 0) and (LeftPart[P] = '.') then
      Delete(LeftPart, P, 1);
  end else
  begin
    while (P > 0) and (LeftPart[P] in [#32..#255]-['.']) do
      Dec(P);
    if P > 0 then
    begin
      Result := copy(LeftPart, P + 1, MaxInt);
      LeftPart := copy(LeftPart, 1, P - 1);
    end else begin
      Result := LeftPart;
      LeftPart := '';
    end;
  end;
end;

procedure FillCompletionList(const AContext: String; AScripting: TCtxScript;
  Instance: TObject; ItemList, InsertList: TStrings);
var
  TempCtx: String;
  TempObj: TObject;
begin
  ItemList.BeginUpdate;
  InsertList.BeginUpdate;
  try
    ItemList.Clear;
    InsertList.Clear;
    TempCtx := Trim(AContext);

    if TempCtx <> '' then
    begin
      if TempCtx[Length(TempCtx)] = '.' then
        Delete(TempCtx, Length(TempCtx), 1);
    end;

    TempObj := nil;
    if TempCtx <> '' then
    try
      TempObj := VarToObject(AScripting.Evaluate(TempCtx, Instance));
    except
    end;
    if TempObj <> nil then
      Instance := TempObj;

    if Instance <> nil then
      GetNamesInScope(Instance, ItemList, [citGetMethodOrProp, citGetArrayProp]);

    if TempCtx = '' then
    begin
      if (AScripting <> nil) and (AScripting.GlobalScope <> nil) then
        GetNamesInScope(AScripting.GlobalScope, ItemList, [citGetMethodOrProp, citGetArrayProp]);

      GetNamesInScope(CtxSystemScope, ItemList, [citGetMethodOrProp, citGetArrayProp]);
    end;

    InsertList.Assign(ItemList);
  finally
    ItemList.EndUpdate;
    InsertList.EndUpdate;
  end;
end;

procedure TCtxSynCompletionProposal.SynCompletionProposalExecute(
  Kind: SynCompletionType; Sender: TObject; var AString: String; var x,
  y: Integer; var CanExecute: Boolean);
var
  I, TmpX: Integer;
  Context, LocLine: String;
begin
  with Editor do
  begin
    LocLine := LineText;
    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    if TmpX > Length(LocLine) then
      TmpX := Length(LocLine);
    // else Dec(TmpX);
    Context := '';
    if (Length(LocLine) > 0) and (LocLine[TmpX] = '.') then
    begin
      I := TmpX - 1;
      // while (I > 0) and not (LocLine[I] in [#0..' ', '(',')',',',';','+','-','/','*','|']) do
      while (I > 0) and not (LocLine[I] in [#0..' ']) do
        Dec(I);
      Context := copy(LocLine, I + 1, TmpX - (I - 1));
    end;
    InsertList.Clear;
    ItemList.Clear;
    FillCompletionList(Context, FScripting, FInstance, ItemList, InsertList);
    TSynCompletionProposal(Sender).ResetAssignedList;
  end;
  CanExecute := True;
end;

procedure TCtxSynCompletionProposal.SynCompletionProposalPaintItem(Sender: TObject;
  Index: Integer; ACanvas: TCanvas; Rect: TRect; var aCustomDraw: boolean);
begin
  //P := Owner as TSynCompletionProposal;
    (*
    Idx: TNodeType;
    Obj: TObject;
    Obj := InsertList.Objects[Index];
    if Obj is TTableDefinition then
    begin
      if TTableDefinition(Obj).IsView then
        Idx := ntView else
        Idx := ntTable;
    end else
    if Obj is TStoredProcDefinition then
      Idx := ntStoredProc else
    if Obj is TFieldDefinition then
      Idx := ntField else
      Idx := ntNil;
    if Idx > ntNil then
      dmSchema.SchemaIcons.Draw(ACanvas, Rect.Left + 2, Rect.Top, integer(Idx));
    *)
    ACanvas.TextOut(Rect.Left + 4, Rect.Top, ItemList[Index]);
  aCustomDraw := True;
end;

procedure ReplaceMemo(Owner: TForm; Memo: TMemo);
var
  SynHighlighter: TSynPasSyn;
  SynEdit: TSynEdit;
  MemoField: PComponent;
  SynCompletionProposal: TCtxSynCompletionProposal;
begin
  SynHighlighter := TSynPasSyn.Create(Owner);

  SynEdit := TSynEdit.Create(Owner);
  MemoField := Owner.FieldAddress(Memo.Name);
  with SynEdit do
  begin
    Parent := Memo.Parent;
    Align := Memo.Align;
    Left := Memo.Left;
    Top := Memo.Top;
    BorderStyle := bsNone;
    Width := Memo.Width;
    Height := Memo.Height;
    Anchors := Memo.Anchors;
    Cursor := crIBeam;
    Font.Height := -13;
    Font.Name := 'Courier New';
    TabOrder := Memo.TabOrder;
    Gutter.Font.Height := -11;
    Gutter.Font.Name := 'Terminal';
    Gutter.Font.Style := [];
    Gutter.Width := 16;
    Highlighter := SynHighlighter;
    WantTabs := True;
    PopupMenu := Memo.PopupMenu;
    OnDblClick := Memo.OnDblClick;
    SearchEngine := TSynEditSearch.Create(SynEdit);
  end;
  Memo.Free;
  PComponent(MemoField)^ := SynEdit;

  SynCompletionProposal := TCtxSynCompletionProposal.Create(SynEdit.Owner);
  with SynCompletionProposal do
  begin
    DefaultType := ctCode;
    Options := [scoLimitToMatchedText, scoUseBuiltInTimer, scoUseInsertList, scoEndCharCompletion, scoCompleteWithEnter];
    OnExecute := SynCompletionProposalExecute;
    OnPaintItem := SynCompletionProposalPaintItem;
    EndOfTokenChr := '"()[]. ';
    Font := SynEdit.Font;
    ClSelect := clSkyBlue;
    ClSelectedText := clWhite;
    TriggerChars := '.';
    TimerInterval := 500;
    Editor := SynEdit;
  end;
  SynEdit.Tag := Integer(SynCompletionProposal);
end;

procedure SetContext(Component: TComponent; AScripting: TCtxScript; AnInstance: TObject = nil);
begin
  // Assign context to highlighter helper
  with TCtxSynCompletionProposal(Component.Tag) do
  begin
    FScripting := AScripting;
    FInstance := AnInstance;
  end;
end;

function GetIdent(Component: TComponent): string;
var
  Edt: TSynEdit;
begin
  Edt := TSynEdit(Component);
  Result := Edt.GetWordAtRowCol(Edt.CaretXY);
end;

function GetLines(Component: TComponent): TStrings;
begin
  Result := TSynEdit(Component).Lines;
end;

function GetText(Component: TComponent): String;
begin
  Result := TSynEdit(Component).Text;
end;

function GetEditModified(Component: TComponent): boolean;
begin
  Result := TSynEdit(Component).Modified;
end;

procedure SetEditModified(Component: TComponent; Value: boolean);
begin
  TSynEdit(Component).Modified := Value;
end;

function DoSearchText(Component: TComponent; const SearchText: String; Options: TFindOptions; FindFirst: Boolean = False): Boolean;
var
  SearchOptions: TSynSearchOptions;
begin
  SearchOptions := [];
  if frWholeWord in Options then
    Include(SearchOptions, ssoWholeWord);
  if frMatchCase in Options then
    Include(SearchOptions, ssoMatchCase);
  if not (frDown in Options) then
    Include(SearchOptions, ssoBackwards);

  Result := TSynEdit(Component).SearchReplace(SearchText, SearchText, SearchOptions) <> 0;
end;

{$ELSE}
uses SysUtils, StrUtils;

procedure ReplaceMemo(Owner: TForm; Memo: TMemo);
begin
end;

function GetLines(Component: TComponent): TStrings;
begin
  Result := TMemo(Component).Lines;
end;

function GetText(Component: TComponent): String;
begin
  Result := TMemo(Component).Text;
end;

function GetEditModified(Component: TComponent): boolean;
begin
  Result := TMemo(Component).Modified;
end;

procedure SetEditModified(Component: TComponent; Value: boolean);
begin
  TMemo(Component).Modified := Value;
end;

procedure SetContext(Component: TComponent; AScripting: TCtxScript; AnInstance: TObject = nil);
begin
end;

function GetIdent(Component: TComponent): string;
begin
  Result := '';
end;

function SearchEdit(EditControl: TCustomEdit; const SearchString: String;
  Options: TFindOptions; FindFirst: Boolean = False): Boolean;
var
  Buffer, P: PChar;
  Size: Word;
  SearchOptions: TStringSearchOptions;
begin
  Result := False;
  if (Length(SearchString) = 0) then Exit;
  Size := EditControl.GetTextLen;
  if (Size = 0) then Exit;
  Buffer := StrAlloc(Size + 1);
  try
    SearchOptions := [];
    if frDown in Options then
      Include(SearchOptions, soDown);
    if frMatchCase in Options then
      Include(SearchOptions, soMatchCase);
    if frWholeWord in Options then
      Include(SearchOptions, soWholeWord);
    EditControl.GetTextBuf(Buffer, Size + 1);
    if FindFirst then
      P := SearchBuf(Buffer, Size, 0, EditControl.SelLength,
             SearchString, SearchOptions)
    else
      P := SearchBuf(Buffer, Size, EditControl.SelStart, EditControl.SelLength,
             SearchString, SearchOptions);
    if P <> nil then
    begin
      EditControl.SelStart := P - Buffer;
      EditControl.SelLength := Length(SearchString);
      Result := True;
    end;
  finally
    StrDispose(Buffer);
  end;
end;


function DoSearchText(Component: TComponent; const SearchText: String; Options: TFindOptions; FindFirst: Boolean = False): Boolean;
begin
  Result := SearchEdit(TMemo(Component), SearchText, Options, FindFirst);
end;

{$ENDIF}

end.


