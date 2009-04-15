unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFnDEF VER130} Variants, {$ENDIF}
  Dialogs, Menus, StdCtrls, CtxActions, ActnList;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    CtxTriggerAction1: TCtxTriggerAction;
    CtxScriptAction1: TCtxScriptAction;
    Button1: TButton;
    File1: TMenuItem;
    CtxScriptAction11: TMenuItem;
    CtxTriggerAction11: TMenuItem;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
