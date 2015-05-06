(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Package registration routines & components editors classes
(*
(*  Copyright (c) 2010 Michael Baytalsky
(*
(******************************************************************************)
unit CtxScriptReg;

interface

procedure Register;

implementation

{$I CtxVer.inc}

uses
  SysUtils, Classes, Controls, ActnList, Dialogs,
  {$IFDEF VER130}
  DsgnIntf,
  {$ELSE}
  DesignEditors, DesignIntf,
  {$ENDIF}
  {$IFDEF DXE3_ORLATER}Actions,{$ENDIF}
  CtxScript, CtxPasCompiler, CtxUnit, CtxUnitEditor, CtxActions, CtxTextReport,
  CtxPkgSysUtils, CtxPkgClasses, CtxPkgDB, CtxPgkGraphics;

type
  TCtxUnitComponentEditor = class (TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TCtxLanguageProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;
begin
  RegisterComponents('Context Scripting', [
    TCtxScript,
    TCtxPasCompiler,
    TCtxUnit,
    TCtxPkgSysUtils,
    TCtxPkgClasses,
    TCtxPkgDB,
    TCtxPkgGraphics,
    TCtxTextReporter
  ]);
  RegisterActions('Context', [TCtxTriggerAction, TCtxScriptAction], nil);

  RegisterPropertyEditor(TypeInfo(string), TCtxUnit, 'Language', TCtxLanguageProperty);
  RegisterComponentEditor(TCtxUnit, TCtxUnitComponentEditor);
end;

{ TCtxClassComponentEditor }

procedure TCtxUnitComponentEditor.ExecuteVerb(Index: Integer);
begin
  EditCtxUnit(Component as TCtxUnit);
  Designer.Modified;
end;

function TCtxUnitComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Edit Context Class';
end;

function TCtxUnitComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TCtxLanguageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TCtxLanguageProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to CtxCompilers.Count - 1 do
    Proc(CtxCompilers[I]);
end;

end.
