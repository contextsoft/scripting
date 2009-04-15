program TextReportDemo;

uses
  Forms,
  fMain in 'fMain.pas' {frmTextReportDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTextReportDemo, frmTextReportDemo);
  Application.Run;
end.
