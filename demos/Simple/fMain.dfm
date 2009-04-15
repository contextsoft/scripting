object Form1: TForm1
  Left = 470
  Top = 304
  Width = 497
  Height = 398
  Caption = 'Simply Scripting'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 37
    Width = 54
    Height = 13
    Caption = 'Expression:'
  end
  object Label2: TLabel
    Left = 80
    Top = 16
    Width = 124
    Height = 13
    Caption = 'Based on form'#39's properties'
  end
  object edtExpression: TEdit
    Left = 80
    Top = 32
    Width = 225
    Height = 21
    TabOrder = 0
    Text = 'Left + Width'
  end
  object Button1: TButton
    Left = 312
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Evaluate'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 24
    Top = 96
    Width = 361
    Height = 249
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'procedure A;'
      'var'
      '  I, J;'
      'begin'
      '  for I := 0 to 100 do '
      '    for J := 1 to 1000 do'
      '      ProgressBar1.Position := I;'
      ''
      '  for I := 100 downto 0 do '
      '    for J := 1 to 1000 do'
      '      ProgressBar1.Position := I;'
      'end;')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button2: TButton
    Left = 312
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 3
    OnClick = Button2Click
  end
  object ProgressBar1: TProgressBar
    Left = 24
    Top = 352
    Width = 361
    Height = 16
    TabOrder = 4
  end
  object CtxScript1: TCtxScript
    TraceInto = False
    TraceMode = False
    DebugOptions = []
    Left = 408
    Top = 24
  end
end
