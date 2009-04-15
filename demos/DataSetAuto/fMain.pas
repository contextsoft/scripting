unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, Grids, DBGrids, DBTables, StdCtrls, ComCtrls, CtxScript,
  CtxPkgDB;

type
  TForm1 = class(TForm)
    Table1: TTable;
    DataSource1: TDataSource;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    Edit1: TEdit;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    CtxScript: TCtxScript;
    lblResult: TLabel;
    Table1OrderNo: TFloatField;
    Table1CustNo: TFloatField;
    Table1SaleDate: TDateTimeField;
    Table1ItemsTotal: TCurrencyField;
    Table1TaxRate: TFloatField;
    Table1Freight: TCurrencyField;
    Table1AmountPaid: TCurrencyField;
    Table1MyCalcField: TCurrencyField;
    CtxPkgDB1: TCtxPkgDB;
    procedure Table1CalcFields(DataSet: TDataSet);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TForm1.Table1CalcFields(DataSet: TDataSet);
begin
  // Execute script for this data set
  Table1MyCalcField.Value := CtxScript.Evaluate(Edit1.Text, Table1);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Table1.Refresh;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Table1.Active := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  lblResult.Caption :=
    VarToStr(CtxScript.Execute(Memo1.Lines.Text, Table1));
end;

end.
