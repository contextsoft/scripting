unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CtxScript, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    CtxScript1: TCtxScript;
    Label1: TLabel;
    edtExpression: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Memo1: TMemo;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  ShowMessage(CtxScript1.Evaluate(edtExpression.Text, Self));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CtxScript1.Execute(Memo1.Lines.Text, Self);
end;

end.
