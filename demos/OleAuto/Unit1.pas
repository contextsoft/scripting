unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CtxScript, CtxPkgSysUtils;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    CtxScript1: TCtxScript;
    CtxPkgSysUtils1: TCtxPkgSysUtils;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CtxScript1.Execute(Memo1.Lines.Text);
end;

end.


