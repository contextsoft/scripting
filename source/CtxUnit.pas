(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Class TCtxUnit imlpementing a generic collection of methods
(*
(*  Contains:
(*                TCtxUnit = class (TComponent)
(*                TCtxUnitIntrospector = class (TCtxIntrospector)
(*
(*  Copyright (c) 2010 Michael Baytalsky
(*
(******************************************************************************)
unit CtxUnit;

interface

uses SysUtils, Classes, Contnrs, CtxScript;

type
  TCtxUnit = class (TComponent)
  protected
    FLanguage: String;
    FUnitName: String;
    FMethods: TObjectList; // of TCtxScriptCode

    function GetMethodCount: Integer;
    function GetMethods(Idx: Integer): TCtxScriptCode;
    procedure SetUnitName(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefineProperties(Filer: TFiler); override;

    procedure WriteMethods(Writer: TWriter);
    procedure ReadMethods(Reader: TReader);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);

    procedure ResetPCode;
    function AddMethod(const Name: String = ''): TCtxScriptCode;
    procedure DeleteMethod(Method: TCtxScriptCode); overload;
    procedure DeleteMethod(Idx: Integer); overload;
    function FindMethod(const Name: String): Integer;
    function MethodByName(const Name: String): TCtxScriptCode;

    property MethodCount: Integer read GetMethodCount;
    property Methods[Idx: Integer]: TCtxScriptCode read GetMethods;
  published
    property UnitName: String read FUnitName write SetUnitName;
    property Language: String read FLanguage write FLanguage;
  end;

  TCtxUnitIntrospector = class (TCtxIntrospector)
  public
    { Finds out wheather this introspector can be used to invoke the code }
    function ResolveName(CtxScript: TCtxScript; Scope: TObject;
      const Name: String; var Index: Integer): Boolean; override;
    { Invokes set method by index }
    function Invoke(CtxScript: TCtxScript; InvokeType: TCtxInvokeType;
      Scope: TObject; const Name: String; var Index: Integer;
      ParCount: Integer): Boolean; override;
  end;

resourcestring
  SMethodNotFoundInClass = 'Method %s not found in unit %s';

const
  CtxUnitVersion = 102;

implementation

{ TCtxClass }

constructor TCtxUnit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethods := TObjectList.Create;
end;

destructor TCtxUnit.Destroy;
begin
  FMethods.Free;
  inherited;
end;

function TCtxUnit.AddMethod(const Name: String = ''): TCtxScriptCode;
begin
  Result := TCtxScriptCode.Create;
  try
    Result.ScriptName := Name;
    Result.Language := Language;
    Result.Code := Result.GetCodeStub;
    FMethods.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCtxUnit.DeleteMethod(Idx: Integer);
begin
  FMethods.Delete(Idx);
end;

procedure TCtxUnit.DeleteMethod(Method: TCtxScriptCode);
begin
  FMethods.Remove(Method);
end;

function TCtxUnit.FindMethod(const Name: String): Integer;
begin
  for Result := 0 to FMethods.Count - 1 do
  begin
    // Methods[Result].ParseHeader;
    try Methods[Result].Compiled := True; except end;
    if AnsiSameText(Name, Methods[Result].ScriptName) then exit;
  end;
  Result := -1;
end;

function TCtxUnit.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
end;

function TCtxUnit.GetMethods(Idx: Integer): TCtxScriptCode;
begin
  Result := TCtxScriptCode(FMethods[Idx]);
end;

procedure TCtxUnit.ResetPCode;
var
  I: Integer;
begin
  for I := 0 to MethodCount - 1 do
    Methods[I].Compiled := False;
end;

function TCtxUnit.MethodByName(const Name: String): TCtxScriptCode;
var
  Idx: Integer;
begin
  Idx := FindMethod(Name);
  if Idx < 0 then
    CtxScriptErrorFmt(SMethodNotFoundInClass, [Name, UnitName]);
  Result := Methods[Idx]
end;

procedure TCtxUnit.LoadFromFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCtxUnit.SaveToFile(const FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCtxUnit.SaveToStream(Stream: TStream);
var
  Writer: TWriter;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    Writer.WriteInteger(CtxUnitVersion); // Reserved for backward compatiblity
    Writer.WriteString(UnitName);
    Writer.WriteString(Language);
    WriteMethods(Writer);
  finally
    Writer.Free;
  end;
end;

procedure TCtxUnit.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    Reader.ReadInteger;
    UnitName := Reader.ReadString;
    Language := Reader.ReadString;
    ReadMethods(Reader);
  finally
    Reader.Free;
  end;
end;

procedure TCtxUnit.WriteMethods(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to MethodCount - 1 do
  with Methods[I] do
  begin
    Writer.WriteString(ScriptName);
    Writer.WriteString(Language);
    Writer.WriteString(Code);
  end;
  Writer.WriteListEnd;
end;

procedure TCtxUnit.ReadMethods(Reader: TReader);
begin
  FMethods.Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    with AddMethod(Reader.ReadString) do
    begin
      Language := Reader.ReadString;
      Code := Reader.ReadString;
    end;
  Reader.ReadListEnd;
end;

procedure TCtxUnit.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Methods', ReadMethods, WriteMethods, MethodCount > 0);
end;

procedure TCtxUnit.SetUnitName(const Value: String);
begin
  if FUnitName <> Value then
  begin
    // Update const
    FUnitName := Value;
  end;
end;

{ TCtxUnitIntrospector }

function TCtxUnitIntrospector.ResolveName(CtxScript: TCtxScript;
  Scope: TObject; const Name: String; var Index: Integer): Boolean;
begin
  Index := TCtxUnit(Scope).FindMethod(Name) + 1;
  Result := Index > 0; // Return True if method is found
end;

function TCtxUnitIntrospector.Invoke(CtxScript: TCtxScript;
  InvokeType: TCtxInvokeType; Scope: TObject; const Name: String;
  var Index: Integer; ParCount: Integer): Boolean;
begin
  Result := True;
  if Index <= 0 then
    Result := ResolveName(CtxScript, Scope, Name, Index);
  if Result then
    case InvokeType of
      citGetMethodOrProp:
        CtxScript.Execute(TCtxUnit(Scope).Methods[Index - 1], Scope);
      citGetArrayProp, citSetArrayProp:
        InvokeDefaultArrayProp(CtxScript, InvokeType, Scope, Name, Index, ParCount);
      else CtxScriptErrorFmt(SInvalidInvokeType, [Name]);
    end;
end;

initialization
  TCtxUnitIntrospector.Create(TCtxUnit);
end.
