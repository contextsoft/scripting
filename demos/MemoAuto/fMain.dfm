object frmMain: TfrmMain
  Left = 293
  Top = 168
  Width = 541
  Height = 401
  Caption = 'Memo Automation Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 328
    Width = 533
    Height = 19
    Panels = <>
  end
  object Document: TMemo
    Left = 0
    Top = 0
    Width = 533
    Height = 328
    Align = alClient
    Lines.Strings = (
      
        'This Demo illustrates using Context Scripting for User Interface' +
        ' automation. '
      
        'Adding user defined scripts into menu (Tools menu), expose Delph' +
        'i methods'
      'and properties as well as functionality of standard VCL classes.'
      ''
      'User Defined methods are stored in external file MemoAuto.cxc.'
      'They can also be stored in dfm. You can edit UserDefined class'
      'by double-clicking on the component to add more methods. ')
    TabOrder = 1
  end
  object MainMenu: TMainMenu
    Left = 88
    Top = 48
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = actExitExecute
      end
    end
    object mnuTools: TMenuItem
      Caption = '&Tools'
      object Customize1: TMenuItem
        Caption = '&Customize...'
        OnClick = Customize1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
  end
  object CtxScript: TCtxScript
    TraceInto = False
    TraceMode = False
    DebugOptions = []
    Left = 456
    Top = 40
  end
  object OpenDialog: TOpenDialog
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Left = 136
    Top = 132
  end
  object SaveDialog: TSaveDialog
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Left = 208
    Top = 132
  end
  object CtxPkgClasses1: TCtxPkgClasses
    Left = 456
    Top = 140
  end
  object CtxPkgSysUtils1: TCtxPkgSysUtils
    Left = 456
    Top = 188
  end
  object CtxUnit: TCtxUnit
    UnitName = 'Macros'
    Left = 404
    Top = 48
    Methods = (
      'MoveFormLeft20Pixels'
      ''
      
        '// Move Form Left 20 Pixels'#13#10'procedure MoveFormLeft20Pixels;'#13#10'be' +
        'gin'#13#10'  Left := Left - 20;'#13#10'  S := 34;'#13#10'end;'
      'MoveFormRight20Pixels'
      ''
      
        '// Move Form Right 20 Pixels'#13#10'procedure MoveFormRight20Pixels;'#13#10 +
        'begin'#13#10'  Left := Left + 20;'#13#10'end;'
      'HelloWorld'
      ''
      
        '// Hello World!'#13#10'procedure HelloWorld;'#13#10'begin'#13#10'  Document.Text :' +
        '= Document.Text + #13#10'#39'Hello World!'#39'; '#13#10'end;'
      'Bold'
      ''
      
        'procedure Bold;'#13#10'begin'#13#10'  Document.SelText := '#39'<b>'#39'+Document.Sel' +
        'Text+'#39'</b>'#39';'#13#10'end;'
      'Italic'
      ''
      
        'procedure Italic;'#13#10'begin'#13#10'  Document.SelText := '#39'<i>'#39'+Document.S' +
        'elText+'#39'</i>'#39';'#13#10'end;'
      'SaveAs'
      ''
      
        '// Save Document As'#13#10'procedure SaveAs;'#13#10'begin'#13#10'  if SaveDialog.E' +
        'xecute then'#13#10'    Document.Save(SaveDialog.FileName);'#13#10'end;'
      'Open'
      ''
      
        '// Open Document'#13#10'procedure Open;'#13#10'begin'#13#10'  if OpenDialog.Execut' +
        'e then'#13#10'    Document.Load(OpenDialog.FileName);'#13#10'end;'
      'ExportToWord'
      ''
      
        '// Export Text to MS Word'#13#10'procedure ExportToWord;'#13#10'var'#13#10'  Word;' +
        #13#10'begin'#13#10'  Word := CreateOleObject('#39'Word.Application'#39');'#13#10'  Word.' +
        'Visible := True;'#13#10'  Word.Documents.Add;'#13#10'  Word.Selection.Font.N' +
        'ame := '#39'Arial'#39';'#13#10'  Word.Selection.Font.Size := 14;'#13#10'  Word.Selec' +
        'tion.Font.Bold := True;'#13#10'  Word.Selection.TypeText(Document.Text' +
        '); '#13#10'end;'
      'DefaultArrayProp'
      ''
      
        '// Using Default Array Property Lines'#13#10'procedure DefaultArrayPro' +
        'p;'#13#10'begin'#13#10'  Document.Lines.Insert(Document.Lines.Count, '#39'Assign' +
        'ed using TStrings[] default array property.'#39');'#13#10'end;')
  end
end
