object Form1: TForm1
  Left = 196
  Top = 152
  Width = 549
  Height = 375
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    541
    348)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 524
    Height = 329
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Calculated Fields'
      DesignSize = (
        516
        301)
      object Label1: TLabel
        Left = 8
        Top = 24
        Width = 100
        Height = 13
        Caption = 'Calc Field Expression'
      end
      object DBGrid1: TDBGrid
        Left = 8
        Top = 52
        Width = 500
        Height = 237
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object Edit1: TEdit
        Left = 120
        Top = 20
        Width = 273
        Height = 21
        TabOrder = 1
        Text = 'ItemsTotal - AmountPaid'
      end
      object Button2: TButton
        Left = 404
        Top = 20
        Width = 75
        Height = 25
        Caption = 'Refresh'
        TabOrder = 2
        OnClick = Button2Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Automation'
      ImageIndex = 1
      object Label2: TLabel
        Left = 16
        Top = 12
        Width = 136
        Height = 13
        Caption = 'Execute the script for Table1'
      end
      object lblResult: TLabel
        Left = 344
        Top = 16
        Width = 43
        Height = 13
        Caption = 'RESULT'
      end
      object Memo1: TMemo
        Left = 16
        Top = 36
        Width = 485
        Height = 249
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'function CalcSum;'
          'var'
          '  B;'
          'begin'
          '  B := Bookmark;'
          '  DisableControls;'
          '  try'
          '    Result := 0;'
          '    First;'
          '    while not EOF do'
          '    begin'
          '      Result := Result + ItemsTotal - AmountPaid;'
          '      Next;'
          '    end;'
          '  finally'
          '    Bookmark := B;'
          '    EnableControls;'
          '  end;'
          'end;'
          '')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object Button1: TButton
        Left = 228
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Execute'
        TabOrder = 1
        OnClick = Button1Click
      end
    end
  end
  object Table1: TTable
    OnCalcFields = Table1CalcFields
    DatabaseName = 'DBDEMOS'
    TableName = 'orders.db'
    Left = 52
    Top = 152
    object Table1OrderNo: TFloatField
      FieldName = 'OrderNo'
    end
    object Table1CustNo: TFloatField
      FieldName = 'CustNo'
      Required = True
    end
    object Table1ItemsTotal: TCurrencyField
      FieldName = 'ItemsTotal'
    end
    object Table1AmountPaid: TCurrencyField
      FieldName = 'AmountPaid'
    end
    object Table1MyCalcField: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'MyCalcField'
      Calculated = True
    end
    object Table1SaleDate: TDateTimeField
      FieldName = 'SaleDate'
    end
    object Table1TaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object Table1Freight: TCurrencyField
      FieldName = 'Freight'
    end
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 68
    Top = 168
  end
  object CtxScript: TCtxScript
    TraceInto = False
    TraceMode = False
    DebugOptions = []
    Left = 340
    Top = 160
  end
  object CtxPkgDB1: TCtxPkgDB
    Left = 392
    Top = 160
  end
end
