object Form1: TForm1
  Left = 192
  Top = 107
  Width = 566
  Height = 425
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    558
    398)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 16
    Top = 40
    Width = 535
    Height = 347
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'procedure WordAutomation;'
      'var'
      '  Word;'
      'begin'
      '  Word := CreateOleObject('#39'Word.Application'#39');'
      '  Word.Visible := True;'
      '  Word.Documents.Add;'
      '  Word.Selection.Font.Name := '#39'Arial'#39';'
      '  Word.Selection.Font.Size := 14;'
      '  Word.Selection.Font.Bold := True;'
      '  Word.Selection.TypeText('#39'Hello from Context Scripting!'#39');'
      '  Word.Selection.TypeParagraph;'
      '  Word.Selection.TypeParagraph;'
      '  Word.Selection.Font.Size := 12;'
      '  Word.Selection.Font.Bold := False;'
      
        '  Word.Selection.TypeText('#39'It is really easy to create Ole Autom' +
        'ation!'#39');'
      '  Word.ActiveDocument.SaveAs('#39'e:\test.rtf'#39', 6);'
      '  Word.Quit;'
      'end;')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 470
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Execute'
    TabOrder = 1
    OnClick = Button1Click
  end
  object CtxScript1: TCtxScript
    TraceInto = False
    TraceMode = False
    DebugOptions = []
    Left = 472
    Top = 168
  end
  object CtxPkgSysUtils1: TCtxPkgSysUtils
    Left = 472
    Top = 116
  end
end
