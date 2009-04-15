program DBSchemaReport;

uses
  Forms,
  fSchemaRep in 'fSchemaRep.pas' {frmTextReporting},
  CtxTextReport in '..\..\source\CtxTextReport.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTextReporting, frmTextReporting);
  Application.Run;
end.
