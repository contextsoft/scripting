(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Classes that implement some generic actions
(*
(*  Contains:
(*                TCtxTriggerAction = class (TAction)
(*                TCtxScriptAction = class (TAction)
(*
(*  Copyright (c) 2005 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : CtxActions.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 1.3
(*  DELPHI      : Delphi 5,6,7,2005
(*  ------------------------------------------------------------
(*  HISTORY     :
(*    2/14/2005    v1.1          Released
(*
(*    No changes to this file since v1.1.
(*
(******************************************************************************)
unit CtxActions;

interface

uses
  SysUtils, Classes, Controls, ActnList, Dialogs, CtxScript, Forms;

type
  {:: This action can be used to trigger a boolean property PropName of }
  {:: a component, set to Component property. This is usefull to show/hide }
  {:: or enable/disable interface controls. }
  TCtxTriggerAction = class (TAction)
  protected
    FPropName: String;
    FComponent: TComponent;
    FCheckedCaption: String;
    FUncheckedCaption: String;
    procedure SetComponent(const Value: TComponent);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;

    function Execute: Boolean; override;
    function Update: Boolean; override;
  published
    {:: Component whose Boolean property PropName will be triggered. }
    property Component: TComponent read FComponent write SetComponent;
    {:: Name of boolean property of the component specified in Component property. }
    property PropName: String read FPropName write FPropName;
    property CheckedCaption: String read FCheckedCaption write FCheckedCaption;
  end;

  {:: This action can be used to execute Context Script }
  {:: specified in Script property. This can be used to create user-defined }
  {:: macro or run-time customizable procedures. }
  TCtxScriptAction = class (TAction)
  private
    FScript: TStringList;
    FPCode: TCtxScriptCode;
    function GetScript: TStrings;
    procedure SetScript(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean; override;
    function Update: Boolean; override;
  published
    {:: Context Script that will be executed by this action. }
    property Script: TStrings read GetScript write SetScript;
  end;

implementation

uses TypInfo;

{ TCtxTriggerAction }

constructor TCtxTriggerAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponent := nil;
  FPropName := 'Enabled';
end;

function TCtxTriggerAction.Execute: Boolean;
begin
  Checked := not Checked;
  SetOrdProp(FComponent, FPropName, Byte(Checked));
  Result := True;
end;

procedure TCtxTriggerAction.Loaded;
begin
  inherited;
  FUncheckedCaption := Caption;
end;

procedure TCtxTriggerAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FComponent) and (Operation = opRemove) then
    FComponent := nil;
  inherited;
end;

procedure TCtxTriggerAction.SetComponent(const Value: TComponent);
begin
  if Value <> FComponent then
  begin
    FComponent := Value;
    if FComponent <> nil then
      FComponent.FreeNotification(Self);
  end;
end;

function TCtxTriggerAction.Update: Boolean;
begin
  Enabled := (FComponent <> nil) and (FPropName <> '');
  if Enabled then
  begin
    Checked := GetOrdProp(FComponent, FPropName) = Integer(True);
    if FCheckedCaption <> '' then
    begin
      if Checked then
        Caption := FCheckedCaption
      else Caption := FUncheckedCaption;
    end;
  end;
  Result := True;
end;

{ TCtxScriptAction }

constructor TCtxScriptAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPCode := TCtxScriptCode.Create;
  FScript := TStringList.Create;
  FScript.Add('procedure OnExecute;');
  FScript.Add('begin');
  FScript.Add('end;');
end;

destructor TCtxScriptAction.Destroy;
begin
  FScript.Free;
  FPCode.Free;
  inherited Destroy;
end;

function TCtxScriptAction.Execute: Boolean;
begin
  FPCode.Code := FScript.Text;
  GetCtxScripting.Execute(FPCode, Owner);
  Result := True;
end;

function TCtxScriptAction.GetScript: TStrings;
begin
  Result := FScript;
end;

procedure TCtxScriptAction.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

function TCtxScriptAction.Update: Boolean;
begin
  Enabled := FScript.Count > 0;
  Result := True;
end;

end.
