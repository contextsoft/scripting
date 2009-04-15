procedure WordAutomation;
var
  Word;
begin
  Word := CreateOleObject('Word.Application');
  Word.Visible := True;
  Word.Documents.Add;
  Word.Selection.Font.Name := 'Arial';
  Word.Selection.Font.Size := 14;
  Word.Selection.Font.Bold := True;
  Word.Selection.TypeText('Hello from Context Scripting!');
  Word.Selection.TypeParagraph;
  Word.Selection.TypeParagraph;
  Word.Selection.Font.Size := 12;
  Word.Selection.Font.Bold := False;
  Word.Selection.TypeText('It is really easy to create Ole Automation!');
  // Export to RTF
  Word.ActiveDocument.SaveAs('c:\test.rtf', 6);
  Word.Quit;
end;