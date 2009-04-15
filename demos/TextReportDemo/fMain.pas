unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, FileCtrl, OleCtrls, SHDocVw,
  CtxTextReport, CtxPkgDB, CtxPkgClasses, CtxScript, CtxPkgSysUtils, DB,
  DBTables;

type
  TfrmTextReportDemo = class(TForm)
    FileListBox: TFileListBox;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    pnlToolBar: TPanel;
    Splitter1: TSplitter;
    PageControl: TPageControl;
    tsTemplate: TTabSheet;
    tsResult: TTabSheet;
    edtTemplate: TMemo;
    edtResult: TMemo;
    tsHTML: TTabSheet;
    WebBrowser: TWebBrowser;
    btnOpen: TButton;
    btnSave: TButton;
    btnProduce: TButton;
    CtxTextReporter: TCtxTextReporter;
    CtxPkgSysUtils1: TCtxPkgSysUtils;
    CtxPkgClasses1: TCtxPkgClasses;
    CtxPkgDB1: TCtxPkgDB;
    Customers: TQuery;
    tblCategories: TTable;
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnProduceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FileListBoxDblClick(Sender: TObject);
    procedure tsHTMLShow(Sender: TObject);
    procedure WebBrowserDocumentComplete(Sender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTextReportDemo: TfrmTextReportDemo;

implementation

{$R *.dfm}

procedure TfrmTextReportDemo.btnOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    edtTemplate.Lines.LoadFromFile(OpenDialog.FileName);
    SaveDialog.FileName := OpenDialog.FileName;
    PageControl.ActivePage := tsTemplate;
  end;
end;

procedure TfrmTextReportDemo.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    edtTemplate.Lines.SaveToFile(SaveDialog.FileName);
  FileListBox.Update;
end;

procedure TfrmTextReportDemo.btnProduceClick(Sender: TObject);
var
  RepStr: String;
  T: Cardinal;
begin
  CtxTextReporter.OpenBracket := '{%';
  CtxTextReporter.CloseBracket := '%}';

  CtxTextReporter.Template := edtTemplate.Lines;
  T := GetTickCount;
  RepStr := CtxTextReporter.ProduceReport(Self);
  T := GetTickCount - T;
  StatusBar.SimpleText := Format('Elapsed: %d msecs', [T]);
  edtResult.Lines.Text := RepStr;
  if AnsiPos('<html>', RepStr) > 0 then
    PageControl.ActivePage := tsHTML
  else PageControl.ActivePage := tsResult;
end;

procedure TfrmTextReportDemo.FormShow(Sender: TObject);
begin
  PageControl.ActivePage := tsTemplate;
  FileListBox.Directory := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Reports';
  tblCategories.DatabaseName := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Data';
end;

procedure TfrmTextReportDemo.FileListBoxDblClick(Sender: TObject);
begin
  SaveDialog.FileName := FileListBox.FileName;
  edtTemplate.Lines.LoadFromFile(SaveDialog.FileName);
  PageControl.ActivePage := tsTemplate;
end;

procedure TfrmTextReportDemo.tsHTMLShow(Sender: TObject);
begin
  WebBrowser.Navigate('about:blank');
end;

procedure TfrmTextReportDemo.WebBrowserDocumentComplete(Sender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
var
  Doc: OleVariant;
begin
  Doc := WebBrowser.Document;
  Doc.Write(edtResult.Lines.Text);
end;

procedure _Categories(Sender: TCtxScript; InvokeType: TCtxInvokeType; Instance: TObject; ParCount: Integer);
var
  ParentCat: TDataSet;
begin
  with TfrmTextReportDemo(Instance) do
  begin
    ParentCat := VarToObject(Sender.GetParam(1)) as TDataSet;
    if ParentCat = nil then
      tblCategories.Filter := 'ParentID = 0'
    else tblCategories.Filter := 'ParentID = ' + ParentCat.FieldByName('ID').AsString;
    Sender.Result := VarFromObject(tblCategories);
  end;
end;

initialization

  with TCtxCustomIntrospector.Create(TfrmTextReportDemo) do
  begin
    AddMethod('Categories', @_Categories, 1);
  end;

end.
