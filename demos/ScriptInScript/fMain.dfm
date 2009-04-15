object Form1: TForm1
  Left = 226
  Top = 127
  Width = 511
  Height = 321
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    503
    294)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 44
    Top = 12
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 268
    Top = 12
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 488
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '// procedure declaration is optional'
      'begin'
      '  // The expression will be evaluated.'
      '  // Caption is the forms caption'
      '  Label1.Caption := Evaluate('#39'Caption + '#39#39'+ New text'#39#39#39');'
      '  // Each object has Invoke method, so his methods'
      '  // can be invoked in its context.'
      '  Label2.Caption := Label1.Invoke('#39'Left'#39');'
      'end;')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 208
    Top = 264
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Execute'
    TabOrder = 1
    OnClick = Button1Click
  end
  object CtxScript: TCtxScript
    TraceInto = False
    TraceMode = False
    DebugOptions = []
    Left = 404
    Top = 188
  end
end
