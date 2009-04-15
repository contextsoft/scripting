; Installs Context Scripting Package

[Setup]
AppName=Context Scripting
AppVerName=Context Scripting
AppCopyright=Copyright © 2004-2005, Michael Baytalsky
DefaultDirName={pf}\Context Software\Scripting
DefaultGroupName=Context Scripting
;UninstallDisplayIcon={app}\
LicenseFile=license.txt
Compression=lzma/ultra
SolidCompression=true
InternalCompressLevel=ultra
SourceDir=.\
OutputDir=-releases
OutputBaseFilename=ctxscript$$$

[Types]
Name: Default; Description: "Full installation"
Name: Custom; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "libd5"; Description: "Delphi 5 Units"; Types: Default
Name: "libd6"; Description: "Delphi 6 Units"; Types: Default
Name: "libd7"; Description: "Delphi 7 Units"; Types: Default
Name: "libd2005"; Description: "Delphi 2005 Units"; Types: Default
Name: "sources"; Description: "Source Code"; Types: Default
Name: "demos"; Description: "Demos"; Types: Default
Name: "help"; Description: "Help"; Types: Default

[Files]
Source: "source\*.*"; DestDir: "{app}\source"; CopyMode: alwaysoverwrite; Components: sources

Source: "lib\d5\*.*"; DestDir: "{app}\libd5"; CopyMode: alwaysoverwrite; Components: libd5
Source: "source\*.*"; Excludes: "*.pas,*D6.*,*D7.*,*D2005.*"; DestDir: "{app}\libd5"; CopyMode: alwaysoverwrite; Components: libd5

Source: "lib\d6\*.*"; DestDir: "{app}\libd6"; CopyMode: alwaysoverwrite; Components: libd6
Source: "source\*.*"; Excludes: "*.pas,*D5.*,*D7.*,*D2005.*"; DestDir: "{app}\libd6"; CopyMode: alwaysoverwrite; Components: libd6

Source: "lib\d7\*.*"; DestDir: "{app}\libd7"; CopyMode: alwaysoverwrite; Components: libd7
Source: "source\*.*"; Excludes: "*.pas,*D5.*,*D6.*,*D2005.*"; DestDir: "{app}\libd7"; CopyMode: alwaysoverwrite; Components: libd7

Source: "lib\d2005\*.*"; DestDir: "{app}\libd2005"; CopyMode: alwaysoverwrite; Components: libd2005
Source: "source\*.*"; Excludes: "*.pas,*D5.*,*D6.*,*D7.*"; DestDir: "{app}\libd2005"; CopyMode: alwaysoverwrite; Components: libd2005

Source: "demos\*.*"; DestDir: "{app}\demos"; Flags: recursesubdirs; Components: demos

Source: "..\setup\instlhlp.exe"; DestDir: "{app}\help"; Components: help
Source: "help\ctxscript.als"; DestDir: "{app}\help"; CopyMode: alwaysoverwrite; Components: help
Source: "help\ctxscript.cnt"; DestDir: "{app}\help"; CopyMode: alwaysoverwrite; Components: help
Source: "help\ctxscript.hlp"; DestDir: "{app}\help"; CopyMode: alwaysoverwrite; Components: help
Source: "readme.txt"; DestDir: "{app}"; Flags: isreadme; CopyMode: alwaysoverwrite;
Source: "license.txt"; DestDir: "{app}"; CopyMode: alwaysoverwrite;

[Icons]
Name: "{group}\Context Scripting Help"; Filename: "{app}\help\ctxscript.hlp"
Name: "{group}\Read Me"; Filename: "{app}\readme.txt"
Name: "{group}\License Agreement"; Filename: "{app}\license.txt"

[Tasks]
Name: helpd5; Description: "Install Help into Delphi 5 IDE"; Components: help;
Name: helpd6; Description: "Install Help into Delphi 6 IDE"; Components: help;
Name: helpd7; Description: "Install Help into Delphi 7 IDE"; Components: help;
Name: helpd2005; Description: "Install Help into Delphi 2005 IDE"; Components: help;

[Registry]
Root: HKCU; Subkey: "Software\Borland\Delphi\5.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd5\CtxScriptPkgD5.bpl"; Flags: uninsdeletevalue; Components: libd5
Root: HKCU; Subkey: "Software\Borland\Delphi\6.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd6\CtxScriptPkgD6.bpl"; Flags: uninsdeletevalue; Components: libd6
Root: HKCU; Subkey: "Software\Borland\Delphi\7.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd7\CtxScriptPkgD7.bpl"; Flags: uninsdeletevalue; Components: libd7
Root: HKCU; Subkey: "Software\Borland\BDS\3.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd2005\CtxScriptPkgD2005.bpl"; Flags: uninsdeletevalue; Components: libd2005

[Run]
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP delphi 5.0 libd5 source"; StatusMsg: "Adding library paths (Delphi 5)..."; Components: libd5
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP delphi 6.0 libd6 source"; StatusMsg: "Adding library paths (Delphi 6)..."; Components: libd6
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP delphi 7.0 libd7 source"; StatusMsg: "Adding library paths (Delphi 7)..."; Components: libd7
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP bds 3.0 libd2005 source"; StatusMsg: "Adding library paths (Delphi 2005)..."; Components: libd2005

Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxScriptHelp ctxscript.hlp delphi 5.0 delphi5"; StatusMsg: "Installing Help (Delphi 5)..."; Tasks: helpd5
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxScriptHelp ctxscript.hlp delphi 6.0 delphi6"; StatusMsg: "Installing Help (Delphi 6)..."; Tasks: helpd6
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxScriptHelp ctxscript.hlp delphi 7.0 d7"; StatusMsg: "Installing Help (Delphi 7)..."; Tasks: helpd7
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxScriptHelp ctxscript.hlp bds 3.0 d2005"; StatusMsg: "Installing Help (Delphi 2005)..."; Tasks: helpd2005

[UninstallRun]
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP delphi 5.0 libd5 source"; StatusMsg: "Removing library paths (Delphi 5)..."; Components: libd5
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP delphi 6.0 libd6 source"; StatusMsg: "Removing library paths (Delphi 6)..."; Components: libd6
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP delphi 7.0 libd7 source"; StatusMsg: "Removing library paths (Delphi 7)..."; Components: libd7
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP bds 3.0 libd2005 source"; StatusMsg: "Removing library paths (Delphi 2005)..."; Components: libd2005

Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxScriptHelp ctxscript.hlp delphi 5.0 delphi5"; StatusMsg: "Removing Help (Delphi 5)..."; Tasks: helpd5
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxScriptHelp ctxscript.hlp delphi 6.0 delphi6"; StatusMsg: "Removing Help (Delphi 6)..."; Tasks: helpd6
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxScriptHelp ctxscript.hlp delphi 7.0 d7"; StatusMsg: "Removing Help (Delphi 7)..."; Tasks: helpd7
;Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxScriptHelp ctxscript.hlp bds 3.0 d2005"; StatusMsg: "Removing Help (Delphi 2005)..."; Tasks: helpd2005

