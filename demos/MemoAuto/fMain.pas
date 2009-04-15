unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, Menus, StdCtrls, ComCtrls, CtxScript, CtxUnit,
  CtxPkgSysUtils, CtxPkgClasses;

type
  TfrmMain = class(TForm)
    StatusBar1: TStatusBar;
    MainMenu: TMainMenu;
    Document: TMemo;
    File1: TMenuItem;
    mnuTools: TMenuItem;
    Exit1: TMenuItem;
    Customize1: TMenuItem;
    CtxScript: TCtxScript;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    CtxPkgClasses1: TCtxPkgClasses;
    CtxPkgSysUtils1: TCtxPkgSysUtils;
    CtxUnit: TCtxUnit;
    procedure actExitExecute(Sender: TObject);
    procedure Customize1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FillToolsMenu;
    procedure ExecuteMethod(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses CtxUnitEditor;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.Customize1Click(Sender: TObject);
begin
  EditCtxUnit(CtxUnit);
  CtxUnit.SaveToFile(ChangeFileExt(Application.ExeName, '.cxc'));
  FillToolsMenu;
end;

procedure TfrmMain.FillToolsMenu;
var
  I: Integer;
  Item: TMenuItem;
begin
  I := mnuTools.Count - 1;
  while mnuTools.Items[I].Tag > 0 do
  begin
    mnuTools.Items[I].Free;
    Dec(I);
  end;
  with CtxUnit do
  for I := 0 to MethodCount - 1 do
  begin
    Item := TMenuItem.Create(Self);
    // Parse header to get script name and display name
    Methods[I].ParseHeader;
    Item.Caption := Methods[I].Description;
    Item.Tag := I + 1;
    Item.OnClick := ExecuteMethod;
    mnuTools.Add(Item);
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  FillToolsMenu;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  CtxUnit.LoadFromFile(ChangeFileExt(Application.ExeName, '.cxc'));
  CtxScript.GlobalScope := Self;
end;

procedure TfrmMain.ExecuteMethod(Sender: TObject);
var
  Index: Integer;
  MethodName: String;
begin
  Index := (Sender as TMenuItem).Tag - 1;
  MethodName := CtxUnit.Methods[Index].ScriptName;
  // This call will invoke method by it's name on the Instance object
  //   CtxScript.Evaluate(MethodName, Instance);
  // It is like if we'd call: Instance.MethodName
  CtxScript.Evaluate(MethodName, CtxUnit);
end;

// We need to expose some methods of TMemo, representing our document
procedure _TMemoGetText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 0);
  Sender.Result := TMemo(Instance).Text;
end;

procedure _TMemoSetText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 0);
  TMemo(Instance).Text := Sender.Result;
end;

procedure _TMemoGetSelText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 0);
  Sender.Result := TMemo(Instance).SelText;
end;

procedure _TMemoSetSelText(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 0);
  TMemo(Instance).SelText := Sender.Result;
end;

// We can also add some more methods
procedure _TMemoSave(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 1);
  TMemo(Instance).Lines.SaveToFile(Sender.GetParam(1));
end;

procedure _TMemoLoad(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 1);
  TMemo(Instance).Lines.LoadFromFile(Sender.GetParam(1));
end;

procedure _TDialogExecute(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
begin
  CheckParams(ParCount, 0);
  Sender.Result := TCommonDialog(Instance).Execute;
end;

initialization
  with TCtxCustomIntrospector.Create(TMemo) do
  begin
    AddProp('Text', @_TMemoGetText, @_TMemoSetText);
    AddProp('SelText', @_TMemoGetSelText, @_TMemoSetSelText);
    AddMethod('Save', @_TMemoSave);
    AddMethod('Load', @_TMemoLoad);
  end;

  with TCtxCustomIntrospector.Create(TCommonDialog) do
  begin
    AddMethod('Execute', @_TDialogExecute);
  end;
end.
