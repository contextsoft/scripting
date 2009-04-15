unit fSchemaRep;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, CtxTextReport, CtxPkgSysUtils, CtxPkgClasses, StdCtrls, dbSchema;

type
  TfrmTextReporting = class(TForm)
    memTemplate: TMemo;
    Panel1: TPanel;
    memResult: TMemo;
    Splitter1: TSplitter;
    Button1: TButton;
    CtxTextReporter: TCtxTextReporter;
    DatabaseSchema: TDatabaseSchema;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTextReporting: TfrmTextReporting;

implementation

{$R *.dfm}

procedure TfrmTextReporting.Button1Click(Sender: TObject);
begin
  CtxTextReporter.Template := memTemplate.Lines;
  CtxTextReporter.ProduceReport(memResult.Lines, DatabaseSchema);
end;


end.
