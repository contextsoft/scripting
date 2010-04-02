(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Registration of some classes from VCL DB unit
(*
(*                TCtxPkgDB = class (TCtxPackage)
(*
(*  Copyright (c) 2010 Michael Baytalsky
(*
(******************************************************************************)
unit CtxPkgDB;

{$I CtxVer.inc}

interface

uses Classes, DB, CtxScript, Contnrs;

type
  TCtxPkgDB = class (TCtxPackage);

  TCtxDataSetIntrospector = class (TCtxIntrospector)
  public
    // Returns True if this introspector is able to resolve the name 
    // within the given scope. 
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; override;
    // Invokes set method by index 
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer;
      ParCount: Integer): Boolean; override;
  end;

implementation

{ TCtxDataSetIntrospector }

function TCtxDataSetIntrospector.ResolveName(CtxScript: TCtxScript;
  Scope: TObject; const Name: String; var Index: Integer): Boolean;
var
  Fld: TField;
begin
  Fld := TDataSet(Scope).FindField(Name);
  Result := Fld <> nil; // Return True if field is found
  if Result then Index := Fld.Index;
end;

function TCtxDataSetIntrospector.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; const Name: String;
  var Index: Integer; ParCount: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < TDataSet(Scope).FieldCount);
  if not Result then
    Result := ResolveName(CtxScript, Scope, Name, Index);
  if Result then
  case InvokeType of
    citGetMethodOrProp:
      CtxScript.Result := TDataSet(Scope).Fields[Index].Value;
    citSetProp:
      TDataSet(Scope).Fields[Index].Value := CtxScript.Result;
    citGetArrayProp, citSetArrayProp:
      InvokeDefaultArrayProp(CtxScript, InvokeType, Scope, Name, Index, ParCount);
    else CtxScriptErrorFmt(SInvalidInvokeType, [Name]);
  end;
end;

type
  TCtxDataSetIterator = class (TCtxObjectIterator)
  protected
    FDataSet: TDataSet;
    FBeyondEOF: Boolean;
    FBookmarks: TObjectList;
  public
    constructor Create(DataSet: TDataSet);
    destructor Destroy; override;

    function Current: TObject; override;
    function First: TObject; override;
    function Next: TObject; override;
    function Prior: TObject; override;
    procedure PushState; override;
    procedure PopState; override;
    function Count: Integer; override;
  end;

  TCtxBookmarkState = class
    {$IFDEF D2009_ORLATER}
    FBookmark: TBookmark;
    {$ELSE}
    FBookmark: TBookmarkStr;
    {$ENDIF}
    FFilter: String;
  end;

constructor TCtxDataSetIterator.Create(DataSet: TDataSet);
begin
  inherited Create;
  FBookmarks := nil;
  FDataSet := DataSet;
  FBeyondEOF := False;
  FDataSet.Active := True;
  First;
end;

destructor TCtxDataSetIterator.Destroy;
begin
  FBookmarks.Free;
  inherited;
end;

function TCtxDataSetIterator.Current: TObject;
begin
  if not FBeyondEOF then
    Result := FDataSet
  else Result := nil;
end;

function TCtxDataSetIterator.First: TObject;
begin
  if (FDataSet.Filter = '') or FDataSet.Filtered then
  begin
    FDataSet.First;
    FBeyondEOF := FDataSet.EOF;
  end else
    FBeyondEOF := not FDataSet.FindFirst;
  Result := Current;
end;

function TCtxDataSetIterator.Next: TObject;
begin
  if (FDataSet.Filter = '') or FDataSet.Filtered then
  begin
    FDataSet.Next;
    FBeyondEOF := FDataSet.EOF;
  end else
    FBeyondEOF := not FDataSet.FindNext;
  Result := Current;
end;

function TCtxDataSetIterator.Count: Integer;
begin
  if FDataSet.Filter = '' then
    Result := FDataSet.RecordCount
  else Result := -1;
end;

function TCtxDataSetIterator.Prior: TObject;
begin
  if FBeyondEOF then
    FBeyondEOF := False
  else begin
    if (FDataSet.Filter = '') or FDataSet.Filtered then
    begin
      FDataSet.Prior;
      FBeyondEOF := FDataSet.EOF;
    end else
      FBeyondEOF := not FDataSet.FindPrior;
  end;
  Result := Current;
end;

procedure TCtxDataSetIterator.PushState;
var
  BookmarkState: TCtxBookmarkState;
begin
  if FBookmarks = nil then
    FBookmarks := TObjectList.Create;

  BookmarkState := TCtxBookmarkState.Create;
  FBookmarks.Add(BookmarkState);
  BookmarkState.FBookmark := FDataSet.Bookmark;
  BookmarkState.FFilter := FDataSet.Filter;
end;

procedure TCtxDataSetIterator.PopState;
begin
  if (FBookmarks = nil) or (FBookmarks.Count < 2) then
    DatabaseError('Invalid call to PopState for DataSet iterator');
  try
    with TCtxBookmarkState(FBookmarks[FBookmarks.Count - 1]) do
    begin
      FDataSet.Filter := FFilter;
      FDataSet.Bookmark := FBookmark;
    end;
  finally
    FBookmarks.Delete(FBookmarks.Count - 1);
  end;
end;

{ TDataSet }

procedure _TDataSetInsert(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Insert;
end;

procedure _TDataSetEdit(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Edit;
end;

procedure _TDataSetDelete(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Delete;
end;

procedure _TDataSetAppend(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Append;
end;

procedure _TDataSetFirst(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).First;
end;

procedure _TDataSetLast(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Last;
end;

procedure _TDataSetNext(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Next;
end;

procedure _TDataSetPrior(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Prior;
end;

procedure _TDataSetEOF(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TDataSet(Instance).EOF;
end;

procedure _TDataSetBOF(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TDataSet(Instance).BOF;
end;

procedure _TDataSetRecordCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TDataSet(Instance).RecordCount;
end;

procedure _TDataSetPost(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Post;
end;

procedure _TDataSetCancel(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Cancel;
end;

procedure _TDataSetOpen(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Open;
end;

procedure _TDataSetClose(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Close;
end;

procedure _TDataSetLocate(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := TDataSet(Instance).Locate(GetParam(2), GetParam(1), []);
end;

procedure _TDataSetEnableControls(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).EnableControls;
end;

procedure _TDataSetDisableControls(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).DisableControls;
end;

procedure _TDataSetFieldByName(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarFromObject(TDataSet(Instance).FieldByName(Sender.Pop));
end;

procedure _TDataSetFieldCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TDataSet(Instance).FieldCount;
end;

procedure _TDataSetGetFields(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromObject(TDataSet(Instance).Fields[GetParam(1)]);
end;

procedure _TDataSetGetBookmark(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TDataSet(Instance).Bookmark;
end;

procedure _TDataSetSetBookmark(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TDataSet(Instance).Bookmark := Sender.Result;
end;

procedure _TDataSetGetIterator(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarFromObject(TCtxDataSetIterator.Create(TDataSet(Instance)));
end;

procedure _TDataSetFieldDefs(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarFromObject(TDataSet(Instance).FieldDefs);
end;

{ TField }

procedure _TFieldGetValue(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TField(Instance).Value;
end;

procedure _TFieldSetValue(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TField(Instance).Value := Sender.Result;
end;

initialization

  TCtxDataSetIntrospector.Create(TDataSet);

  with TCtxCustomIntrospector.Create(TDataSet) do
  begin
    // object methods
    AddMethod('Insert', @_TDataSetInsert, 0);
    AddMethod('Edit', @_TDataSetEdit, 0);
    AddMethod('Delete', @_TDataSetDelete, 0);
    AddMethod('Append', @_TDataSetAppend, 0);
    AddMethod('Post', @_TDataSetPost, 0);
    AddMethod('Cancel', @_TDataSetCancel, 0);
    AddMethod('First', @_TDataSetFirst, 0);
    AddMethod('Last', @_TDataSetLast, 0);
    AddMethod('Next', @_TDataSetNext, 0);
    AddMethod('Prior', @_TDataSetPrior, 0);
    AddMethod('Open', @_TDataSetOpen, 0);
    AddMethod('Close', @_TDataSetClose, 0);
    AddMethod('EOF', @_TDataSetEOF, 0);
    AddMethod('BOF', @_TDataSetBOF, 0);
    AddMethod('Locate', @_TDataSetLocate, 2);
    AddMethod('EnableControls', @_TDataSetEnableControls, 0);
    AddMethod('DisableControls', @_TDataSetDisableControls, 0);

    AddMethod('FieldByName', @_TDataSetFieldByName, 1);
    AddProp('FieldCount', @_TDataSetFieldCount);
    AddArrayProp('Fields', @_TDataSetGetFields, nil, 1);

    AddMethod('GetIterator', @_TDataSetGetIterator, 0);

    AddProp('RecordCount', @_TDataSetRecordCount);
    AddProp('Bookmark', @_TDataSetGetBookmark, @_TDataSetSetBookmark);
    AddProp('FieldDefs', @_TDataSetFieldDefs);
  end;

  with TCtxCustomIntrospector.Create(TField) do
  begin
    AddProp('Value', @_TFieldGetValue, @_TFieldSetValue);
  end;

end.
