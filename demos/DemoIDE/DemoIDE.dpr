program DemoIDE;

uses
  Forms,
  fMain in 'fMain.pas' {frmCtxDemoIDE};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Context Scripting Demo';
  Application.CreateForm(TfrmCtxDemoIDE, frmCtxDemoIDE);
  Application.Run;
end.
