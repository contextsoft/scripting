(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Registration of basic VCL classes from Classes unit
(*
(*                TCtxPkgClasses = class (TCtxPackage)
(*
(*  Copyright (c) 2005 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : CtxPkgClasses.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 1.3
(*  DELPHI      : Delphi 5,6,7,2005
(*  ------------------------------------------------------------
(*  HISTORY     :
(*
(*    2/14/2005    v1.2     Released
(*
(*    6/23/2005    v1.3     Added iterators for compatibility with 
(* 							CtxTextReporter.
(*
(*    No changes to this file since v1.3.
(*
(******************************************************************************)
unit CtxPkgClasses;

interface

uses SysUtils, Classes, {$IFnDEF VER130}Variants,{$ENDIF} CtxScript;

type
  {:: The only purpose of this component is to make sure CtxClassesLib unit is }
  {:: included into uses clause. }
  TCtxPkgClasses = class (TCtxPackage);

implementation

{ TCtxCollectionIterator }

type
  TCtxCollectionIterator = class (TCtxObjectIterator)
  protected
    FCollection: TCollection;
    FIndex: Integer;
  public
    constructor Create(Collection: TCollection);
    function Current: TObject; override;
    function First: TObject; override;
    function Next: TObject; override;
    function Prior: TObject; override;
    function Count: Integer; override;
  end;

constructor TCtxCollectionIterator.Create(Collection: TCollection);
begin
  inherited Create;
  FCollection := Collection;
  First;
end;

function TCtxCollectionIterator.Current: TObject;
begin
  if FIndex < FCollection.Count then
    Result := FCollection.Items[FIndex]
  else Result := nil;
end;

function TCtxCollectionIterator.First: TObject;
begin
  FIndex := 0;
  Result := Current;
end;

function TCtxCollectionIterator.Next: TObject;
begin
  if FIndex < FCollection.Count then
    Inc(FIndex);
  Result := Current;
end;

function TCtxCollectionIterator.Prior: TObject;
begin
  if FIndex >= 0 then
    Dec(FIndex);
  Result := Current;
end;

function TCtxCollectionIterator.Count: Integer;
begin
  Result := FCollection.Count;
end;

{ TCtxComponentIterator }

type
  TCtxComponentIterator = class (TCtxObjectIterator)
  protected
    FComponent: TComponent;
    FIndex: Integer;
  public
    constructor Create(Component: TComponent);
    function Current: TObject; override;
    function First: TObject; override;
    function Next: TObject; override;
    function Prior: TObject; override;
    function Count: Integer; override;
  end;

constructor TCtxComponentIterator.Create(Component: TComponent);
begin
  inherited Create;
  FComponent := Component;
  First;
end;

function TCtxComponentIterator.Current: TObject;
begin
  if FIndex < FComponent.ComponentCount then
    Result := FComponent.Components[FIndex]
  else Result := nil;
end;

function TCtxComponentIterator.First: TObject;
begin
  FIndex := 0;
  Result := Current;
end;

function TCtxComponentIterator.Next: TObject;
begin
  if FIndex < FComponent.ComponentCount then
    Inc(FIndex);
  Result := Current;
end;

function TCtxComponentIterator.Prior: TObject;
begin
  if FIndex >= 0 then
    Dec(FIndex);
  Result := Current;
end;

function TCtxComponentIterator.Count: Integer;
begin
  Result := FComponent.ComponentCount;
end;

type
  TCtxListIterator = class (TCtxObjectIterator)
  protected
    FList: TList;
    FIndex: Integer;
  public
    constructor Create(List: TList);
    function Current: TObject; override;
    function First: TObject; override;
    function Next: TObject; override;
    function Prior: TObject; override;
    function Count: Integer; override;
  end;

{ TCtxListIterator }

constructor TCtxListIterator.Create(List: TList);
begin
  inherited Create;
  FList := List;
end;

function TCtxListIterator.Count: Integer;
begin
  Result := FList.Count;
end;

function TCtxListIterator.Current: TObject;
begin
  if (FIndex >=0) and (FIndex < FList.Count) then
    Result := FList[FIndex]
  else Result := nil;
end;

function TCtxListIterator.First: TObject;
begin
  FIndex := 0;
  Result := Current;
end;

function TCtxListIterator.Next: TObject;
begin
  Inc(FIndex);
  Result := Current;
end;

function TCtxListIterator.Prior: TObject;
begin
  if FIndex >= 0 then
    Dec(FIndex);
  Result := Current;
end;

{ TList }

procedure _CreateList(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarFromObject(TList.Create);
end;

procedure _TListAdd(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  // Instance contains TList object. First and only parameter contains
  // objects to be added to list.
  Sender.Result := TList(Instance).Add(VarToObject(Sender.GetParam(1)));
end;

procedure _TListInsert(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  // Parameters are reversed on stack, so we should get them in
  // reverse order.
  with Sender do
    TList(Instance).Insert(GetParam(2), VarToObject(GetParam(1)));
end;

procedure _TListDelete(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TList(Instance).Delete(Sender.GetParam(1));
end;

procedure _TListGetCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TList(Instance).Count;
end;

procedure _TListSetCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TList(Instance).Count := Sender.Result;
end;

procedure _TListGetItems(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Sender.Result := VarFromObject(TObject(TList(Instance)[Pop]));
end;

procedure _TListSetItems(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    TList(Instance)[Pop] := VarToObject(Sender.Result);
end;

type
  TCtxStringsIterator = class (TCtxObjectIterator)
  protected
    FStrings: TStrings;
    FIndex: Integer;
    function GetItem: String;
    function GetKey: String;
    function GetValue: String;
  public
    constructor Create(Strings: TStrings);
    function Current: TObject; override;
    function First: TObject; override;
    function Next: TObject; override;
    function Prior: TObject; override;
    function Count: Integer; override;
  published
    property Item: String read GetItem;
    property Value: String read GetValue;
    property Key: String read GetKey;
  end;

{ TCtxStringsIterator }

constructor TCtxStringsIterator.Create(Strings: TStrings);
begin
  inherited Create;
  FStrings := Strings;
end;

function TCtxStringsIterator.Count: Integer;
begin
  Result := FStrings.Count;
end;

function TCtxStringsIterator.Current: TObject;
begin
  if (FIndex >=0) and (FIndex < FStrings.Count) then
    Result := Self
  else Result := nil;
end;

function TCtxStringsIterator.First: TObject;
begin
  FIndex := 0;
  Result := Current;
end;

function TCtxStringsIterator.Next: TObject;
begin
  Inc(FIndex);
  Result := Current;
end;

function TCtxStringsIterator.Prior: TObject;
begin
  if FIndex >= 0 then
    Dec(FIndex);
  Result := Current;
end;

function TCtxStringsIterator.GetItem: String;
begin
  Result := FStrings[FIndex];
end;

function TCtxStringsIterator.GetKey: String;
begin
  Result := FStrings.Names[FIndex];
end;

function ValueFromIndex(Strings: TStrings; Idx: Integer): String;
begin
  if Idx >= 0 then
    Result := Copy(Strings[Idx], Length(Strings.Names[Idx]) + 2, MaxInt) else
    Result := '';
end;

function TCtxStringsIterator.GetValue: String;
begin
  Result := ValueFromIndex(FStrings, FIndex);
end;

{ TStringList }

procedure _CreateStringList(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarFromObject(TStringList.Create);
end;

{ TStrings }

procedure _TStringsAdd(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TStrings(Instance).Add(Sender.GetParam(1));
end;

procedure _TStringsInsert(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    TStrings(Instance).Insert(GetParam(2), GetParam(1));
end;

procedure _TStringsClear(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    TStrings(Instance).Clear;
end;

procedure _TStringsDelete(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TStrings(Instance).Delete(Sender.GetParam(1));
end;

procedure _TStringsGetCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TStrings(Instance).Count;
end;

procedure _TStringsGetText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := TStrings(Instance).Text;
end;

procedure _TStringsSetText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TStrings(Instance).Text := Sender.Result;
end;

procedure _TStringsGetStrings(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := TStrings(Instance).Strings[GetParam(1)];
end;

procedure _TStringsSetStrings(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    TStrings(Instance).Strings[GetParam(1)] := Result;
end;

procedure _TStringsSaveToFile(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TStrings(Instance).SaveToFile(Sender.GetParam(1));
end;

procedure _TStringsLoadFromFile(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TStrings(Instance).LoadFromFile(Sender.GetParam(1));
end;

{ TCollection }

procedure _TCollectionAdd(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  Sender.Result := VarFromObject(TCollection(Instance).Add);
end;

procedure _TCollectionInsert(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromObject(TCollection(Instance).Insert(Pop));
end;

procedure _TCollectionCount(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := TCollection(Instance).Count;
end;

procedure _TCollectionGetItem(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromObject(TCollection(Instance).Items[Pop]);
end;

procedure _TCollectionSetItem(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    TCollection(Instance).Items[Pop] := VarToObject(Result) as TCollectionItem;
end;

procedure _TCollectionClear(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  TCollection(Instance).Clear;
end;

procedure _TCollectionDelete(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    TCollection(Instance).Delete(Pop);
end;

procedure _TCollectionGetIterator(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromObject(TCtxCollectionIterator.Create(TCollection(Instance)));
end;

procedure _TListGetIterator(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromObject(TCtxListIterator.Create(TList(Instance)));
end;

procedure _TStringsGetIterator(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromObject(TCtxStringsIterator.Create(TStrings(Instance)));
end;

{ TComponent }

procedure _TComponentGetIterator(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromObject(TCtxComponentIterator.Create(TComponent(Instance)));
end;

{ TStream }

function ReadLine(Stream: TStream): String;
var
  C: Char;
  Eof: Integer;
begin
  Result := '';
  Eof := Stream.Read(C, SizeOf(C));
  while Eof > 0 do
  begin
    if C = #13 then
    begin
      Stream.Read(C, SizeOf(C));
      exit;
    end;
    Result := Result + C;
    Eof := Stream.Read(C, SizeOf(C));
  end;
end;

procedure WriteLine(Stream: TStream; Str: String);
const
  EOLN = #13#10;
begin
  if Str <> '' then
    Stream.Write(Str[1], Length(Str));
  Stream.Write(EOLN[1], 2);
end;

procedure _TStreamSize(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := TStream(Instance).Size;
end;

procedure _TStreamReadLine(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := ReadLine(TStream(Instance));
end;

procedure _TStreamWriteLine(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    WriteLine(TStream(Instance), Pop);
end;

procedure _TStreamRead(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Str: String;
begin
  with Sender do
  begin
    Str := VarToStr(GetVarByRef(GetParam(2)));
    TStream(Instance).Read(Str[1], GetParam(1));
    SetVarByRef(GetParam(2), Str);
  end;
end;

procedure _TStreamWrite(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  Str: String;
begin
  with Sender do
  begin
    Str := VarToStr(GetParam(2));
    TStream(Instance).Write(Str[1], GetParam(1));
  end;
end;

{ TFileStream }
procedure _CreateFileStream(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  with Sender do
    Result := VarFromObject(TFileStream.Create(GetParam(2), GetParam(1)));
end;


initialization

  with TCtxCustomIntrospector.Create(TCtxSystemScope) do
  begin
    AddMethod('CreateList', @_CreateList, 0);
    AddMethod('CreateStringList', @_CreateStringList, 0);
    AddMethod('CreateFileStream', @_CreateFileStream, 2);
  end;

  with TCtxCustomIntrospector.Create(TList) do
  begin
    AddMethod('Add', @_TListAdd, 1);
    AddMethod('Delete', @_TListDelete, 1);
    AddMethod('Insert', @_TListInsert, 2);

    AddProp('Count', @_TListGetCount, @_TListSetCount);
    { Default array property }
    AddDefaultArrayProp(@_TListGetItems, @_TListSetItems, 1);

    AddMethod('GetIterator', @_TListGetIterator, 0);
  end;

  with TCtxCustomIntrospector.Create(TStrings) do
  begin
    AddMethod('Add', @_TStringsAdd, 1);
    AddMethod('Delete', @_TStringsDelete, 1);
    AddMethod('Insert', @_TStringsInsert, 2);
    AddMethod('Clear', @_TStringsClear, 0);

    AddMethod('SaveToFile', @_TStringsSaveToFile, 1);
    AddMethod('LoadFromFile', @_TStringsLoadFromFile, 1);

    AddProp('Count', @_TStringsGetCount);
    AddProp('Text', @_TStringsGetText, @_TStringsSetText);
    AddArrayProp('Strings', @_TStringsGetStrings, @_TStringsSetStrings, 1);
    { Default array property }
    AddDefaultArrayProp(@_TStringsGetStrings, @_TStringsSetStrings, 1);

    AddMethod('GetIterator', @_TStringsGetIterator, 0);
  end;

  with TCtxCustomIntrospector.Create(TCollection) do
  begin
    AddMethod('Add', @_TCollectionAdd, 0);
    AddMethod('Insert', @_TCollectionInsert, 1);
    AddMethod('Delete', @_TCollectionDelete, 1);
    AddMethod('Clear', @_TCollectionClear, 0);
    AddMethod('GetIterator', @_TCollectionGetIterator, 0);

    AddProp('Count', @_TCollectionCount);
    AddArrayProp('Items', @_TCollectionGetItem, @_TCollectionSetItem, 1);
    { Default array property }
    AddDefaultArrayProp(@_TCollectionGetItem, @_TCollectionSetItem, 1);
  end;

  with TCtxCustomIntrospector.Create(TStream) do
  begin
    AddMethod('ReadLine', @_TStreamReadLine, 0);
    AddMethod('WriteLine', @_TStreamWriteLine, 1);
    AddMethod('Read', @_TStreamRead, 2);
    AddMethod('Write', @_TStreamWrite, 2);

    AddProp('Size', @_TStreamSize);
  end;

  with TCtxCustomIntrospector.Create(TComponent) do
  begin
    AddMethod('GetIterator', @_TComponentGetIterator, 0);
  end;

end.