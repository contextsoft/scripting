object Form1: TForm1
  Left = 248
  Top = 166
  Width = 303
  Height = 242
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 96
    Top = 104
    Width = 75
    Height = 25
    Action = CtxScriptAction1
    TabOrder = 0
  end
  object MainMenu1: TMainMenu
    Left = 120
    Top = 24
    object File1: TMenuItem
      Caption = 'Script'
      object CtxScriptAction11: TMenuItem
        Action = CtxScriptAction1
      end
      object CtxTriggerAction11: TMenuItem
        Action = CtxTriggerAction1
      end
    end
  end
  object ActionList1: TActionList
    Left = 200
    Top = 40
    object CtxTriggerAction1: TCtxTriggerAction
      Caption = 'Show Button'
      Component = Button1
      PropName = 'Visible'
      CheckedCaption = 'Hide Button'
    end
    object CtxScriptAction1: TCtxScriptAction
      Caption = 'Exec Script'
      Script.Strings = (
        'procedure OnExecute;'
        'begin'
        '  Left := Left + 10;'
        'end;')
    end
  end
end
