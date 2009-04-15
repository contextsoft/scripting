object frmTextReportDemo: TfrmTextReportDemo
  Left = 374
  Top = 265
  Width = 696
  Height = 532
  Caption = 'Text Reporting Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 153
    Top = 33
    Height = 446
  end
  object FileListBox: TFileListBox
    Left = 0
    Top = 33
    Width = 153
    Height = 446
    Align = alLeft
    ItemHeight = 13
    Mask = '*.trt*'
    TabOrder = 0
    OnDblClick = FileListBoxDblClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 479
    Width = 688
    Height = 19
    Panels = <>
  end
  object pnlToolBar: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 33
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object btnOpen: TButton
      Left = 14
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Open...'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnSave: TButton
      Left = 94
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Save As...'
      TabOrder = 1
      OnClick = btnSaveClick
    end
    object btnProduce: TButton
      Left = 174
      Top = 4
      Width = 115
      Height = 25
      Caption = '&Produce Report'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = btnProduceClick
    end
  end
  object PageControl: TPageControl
    Left = 156
    Top = 33
    Width = 532
    Height = 446
    ActivePage = tsTemplate
    Align = alClient
    TabOrder = 3
    object tsTemplate: TTabSheet
      Caption = 'Template'
      object edtTemplate: TMemo
        Left = 0
        Top = 0
        Width = 524
        Height = 418
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsResult: TTabSheet
      Caption = 'Text Preview'
      ImageIndex = 1
      object edtResult: TMemo
        Left = 0
        Top = 0
        Width = 524
        Height = 425
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object tsHTML: TTabSheet
      Caption = 'HTML Preview'
      ImageIndex = 2
      OnShow = tsHTMLShow
      object WebBrowser: TWebBrowser
        Left = 0
        Top = 0
        Width = 524
        Height = 425
        Align = alClient
        TabOrder = 0
        OnDocumentComplete = WebBrowserDocumentComplete
        ControlData = {
          4C00000028360000ED2B00000000000000000000000000000000000000000000
          000000004C000000000000000000000001000000E0D057007335CF11AE690800
          2B2E126208000000000000004C0000000114020000000000C000000000000046
          8000000000000000000000000000000000000000000000000000000000000000
          00000000000000000100000000000000000000000000000000000000}
      end
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.trt'
    Filter = 
      'Text Reports (*.trt)|*.trt|Text Files (*.txt)|*.txt|All Files (*' +
      '.*)|*.*'
    Left = 232
    Top = 216
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.trt'
    Filter = 
      'Text Reports (*.trt)|*.trt|Text Files (*.txt)|*.txt|All Files (*' +
      '.*)|*.*'
    Left = 312
    Top = 224
  end
  object CtxTextReporter: TCtxTextReporter
    Params.Strings = (
      'a=1234'
      'b=7890')
    Prepared = False
    OpenBracket = '{%'
    CloseBracket = '%}'
    Left = 408
    Top = 161
  end
  object CtxPkgSysUtils1: TCtxPkgSysUtils
    Left = 208
    Top = 105
  end
  object CtxPkgClasses1: TCtxPkgClasses
    Left = 272
    Top = 105
  end
  object CtxPkgDB1: TCtxPkgDB
    Left = 336
    Top = 105
  end
  object Customers: TQuery
    DatabaseName = 'DBDEMOS'
    SQL.Strings = (
      'select * from customer order by Zip, City')
    Left = 480
    Top = 113
  end
  object tblCategories: TTable
    DatabaseName = 'E:\projects\sdk\Context\Scripting\demos\TextReportDemo\Data\'
    IndexName = 'ByParentIDName'
    TableName = 'Categories.db'
    Left = 488
    Top = 241
  end
end
