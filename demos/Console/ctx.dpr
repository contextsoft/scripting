program ctx;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  CtxScript,
  CtxPkgSysUtils,
  CtxPkgClasses;

var
  Script: TCtxScriptCode;
  FileName, Code: String;
  FileStream: TFileStream;

begin
  if ParamCount = 0 then
  begin
    writeln('Usage: ctx <script.ctx>');
    exit;
  end;
  try
    FileName := ParamStr(1);
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      SetLength(Code, FileStream.Size);
      FileStream.Read(Code[1], FileStream.Size);
    finally
      FileStream.Free;
    end;
    Script := TCtxScriptCode.Create;
    try
      Script.ScriptName := FileName;
      Script.Code := Code;
      GetCtxScripting.Execute(Script);
    finally
      Script.Free;
    end;
  except
    on E: Exception do
      writeln('Error: ', E.Message);
  end;
end.
