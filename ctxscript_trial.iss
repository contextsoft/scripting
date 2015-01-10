; Installs Context Scripting Package

[Setup]
AppName=Context Scripting
AppVerName=Context Scripting v.1.50 Trial
AppCopyright=Copyright © 2004-2015, Michael Baytalsky
DefaultDirName={pf}\Context Software\Scripting
DefaultGroupName=Context Scripting
;UninstallDisplayIcon={app}\
LicenseFile=license.txt
Compression=lzma/ultra
SolidCompression=true
InternalCompressLevel=ultra
SourceDir=.\
OutputDir=-releases
OutputBaseFilename=ctxscript$$$t

[Types]
Name: Default; Description: "Full installation"
Name: Custom; Description: "Custom installation"; Flags: iscustom

[Components]
Name: "libd7"; Description: "Delphi 7 Units"; Types: Default
Name: "libd2005"; Description: "Delphi 2005 Units"; Types: Default
Name: "libd2006"; Description: "Delphi 2006 Units"; Types: Default
Name: "libd2007"; Description: "Delphi 2007 Units"; Types: Default
Name: "libd2009"; Description: "Delphi 2009 Units"; Types: Default
Name: "libd2010"; Description: "Delphi 2010 Units"; Types: Default
Name: "libd2011"; Description: "Delphi XE/RAD Studio XE Library"; Types: Default
Name: "libdXE2"; Description: "Delphi XE2/RAD Studio XE2 Library"; Types: Default
Name: "libdXE3"; Description: "Delphi XE3/RAD Studio XE3 Library"; Types: Default
Name: "libdXE4"; Description: "Delphi XE4/RAD Studio XE4 Library"; Types: Default
Name: "libdXE5"; Description: "Delphi XE5/RAD Studio XE5 Library"; Types: Default
Name: "libdXE6"; Description: "Delphi XE6/RAD Studio XE6 Library"; Types: Default
Name: "libdXE7"; Description: "Delphi XE7/RAD Studio XE7 Library"; Types: Default

Name: "sources"; Description: "Source Code"; Types: Default
Name: "demos"; Description: "Demos"; Types: Default
Name: "help"; Description: "Help"; Types: Default

[Files]
Source: "packages\*.*"; DestDir: "{app}\packages"; Flags: recursesubdirs ignoreversion; Components: sources
;Source: "source\*.*"; DestDir: "{app}\source"; CopyMode: alwaysoverwrite; Components: sources

Source: "lib\d7\*.*"; DestDir: "{app}\libd7"; CopyMode: alwaysoverwrite; Components: libd7
Source: "source\*.*"; Excludes: "*.pas,*D5.*,*D6.*,*D2005.*"; DestDir: "{app}\libd7"; CopyMode: alwaysoverwrite; Components: libd7

Source: "lib\d2005\*.*"; DestDir: "{app}\libd2005"; CopyMode: alwaysoverwrite; Components: libd2005
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2005"; CopyMode: alwaysoverwrite; Components: libd2005

Source: "lib\d2006\*.*"; DestDir: "{app}\libd2006"; CopyMode: alwaysoverwrite; Components: libd2006
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2006"; CopyMode: alwaysoverwrite; Components: libd2006

Source: "lib\d2007\*.*"; DestDir: "{app}\libd2007"; CopyMode: alwaysoverwrite; Components: libd2007
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2007"; CopyMode: alwaysoverwrite; Components: libd2007

Source: "lib\d2009\*.*"; DestDir: "{app}\libd2009"; CopyMode: alwaysoverwrite; Components: libd2009
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2009"; CopyMode: alwaysoverwrite; Components: libd2009

Source: "lib\d2010\*.*"; DestDir: "{app}\libd2010"; CopyMode: alwaysoverwrite; Components: libd2010
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2010"; CopyMode: alwaysoverwrite; Components: libd2010

Source: "lib\d2011\*.*"; DestDir: "{app}\libd2011"; CopyMode: alwaysoverwrite; Components: libd2011
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libd2011"; CopyMode: alwaysoverwrite; Components: libd2011

Source: "lib\dXE2\*.*"; DestDir: "{app}\libdXE2"; CopyMode: alwaysoverwrite; Components: libdXE2
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE2"; CopyMode: alwaysoverwrite; Components: libdXE2

Source: "lib\dXE3\*.*"; DestDir: "{app}\libdxe3"; CopyMode: alwaysoverwrite; Components: libdXE3
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE3"; CopyMode: alwaysoverwrite; Components: libdXE3

Source: "lib\dXE4\*.*"; DestDir: "{app}\libdXE4"; CopyMode: alwaysoverwrite; Components: libdXE4
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE4"; CopyMode: alwaysoverwrite; Components: libdXE4

Source: "lib\dXE5\*.*"; DestDir: "{app}\libdXE5"; CopyMode: alwaysoverwrite; Components: libdXE5
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE5"; CopyMode: alwaysoverwrite; Components: libdXE5

Source: "lib\dXE6\*.*"; DestDir: "{app}\libdXE6"; CopyMode: alwaysoverwrite; Components: libdXE6
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE6"; CopyMode: alwaysoverwrite; Components: libdXE6

Source: "lib\dXE7\*.*"; DestDir: "{app}\libdXE7"; CopyMode: alwaysoverwrite; Components: libdXE7
Source: "source\*.*"; Excludes: "*.pas"; DestDir: "{app}\libdXE7"; CopyMode: alwaysoverwrite; Components: libdXE7

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
Name: insthelp; Description: "Install Help into Delphi IDE"; Components: help;

[Registry]
;Root: HKCU; Subkey: "Software\Borland\Delphi\5.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd5\CtxScriptPkgD5.bpl"; Flags: uninsdeletevalue; Components: libd5
;Root: HKCU; Subkey: "Software\Borland\Delphi\6.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd6\CtxScriptPkgD6.bpl"; Flags: uninsdeletevalue; Components: libd6
Root: HKCU; Subkey: "Software\Borland\Delphi\7.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd7\CtxScriptPkgD7.bpl"; Flags: uninsdeletevalue; Components: libd7
Root: HKCU; Subkey: "Software\Borland\BDS\3.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd2005\CtxScriptPkgD2005.bpl"; Flags: uninsdeletevalue; Components: libd2005
Root: HKCU; Subkey: "Software\Borland\BDS\4.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd2006\CtxScriptPkgD2006.bpl"; Flags: uninsdeletevalue; Components: libd2006
Root: HKCU; Subkey: "Software\Borland\BDS\5.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd2007\CtxScriptPkgD2007.bpl"; Flags: uninsdeletevalue; Components: libd2007
Root: HKCU; Subkey: "Software\CodeGear\BDS\6.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd2009\CtxScriptPkgD2009.bpl"; Flags: uninsdeletevalue; Components: libd2009
Root: HKCU; Subkey: "Software\CodeGear\BDS\7.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd2010\CtxScriptPkgD2010.bpl"; Flags: uninsdeletevalue; Components: libd2010
Root: HKCU; Subkey: "Software\CodeGear\BDS\8.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libd2011\CtxScriptPkgD2011.bpl"; Flags: uninsdeletevalue; Components: libd2011
Root: HKCU; Subkey: "Software\CodeGear\BDS\9.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libdXE2\CtxScriptPkgDXE2.bpl"; Flags: uninsdeletevalue; Components: libdXE2
Root: HKCU; Subkey: "Software\CodeGear\BDS\10.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libdXE3\CtxScriptPkgDXE3.bpl"; Flags: uninsdeletevalue; Components: libdXE3
Root: HKCU; Subkey: "Software\CodeGear\BDS\11.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libdXE4\CtxScriptPkgDXE4.bpl"; Flags: uninsdeletevalue; Components: libdXE4
Root: HKCU; Subkey: "Software\CodeGear\BDS\12.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libdXE5\CtxScriptPkgDXE5.bpl"; Flags: uninsdeletevalue; Components: libdXE5
Root: HKCU; Subkey: "Software\CodeGear\BDS\14.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libdXE6\CtxScriptPkgDXE6.bpl"; Flags: uninsdeletevalue; Components: libdXE6
Root: HKCU; Subkey: "Software\CodeGear\BDS\15.0\Known Packages"; ValueType: string; ValueData: "Context Scripting"; ValueName: "{app}\libdXE7\CtxScriptPkgDXE7.bpl"; Flags: uninsdeletevalue; Components: libdXE7


[Run]
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\delphi 7.0 libd7 source"; StatusMsg: "Adding library paths (Delphi 7)..."; Components: libd7
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\bds 3.0 libd2005 source"; StatusMsg: "Adding library paths (Delphi 2005)..."; Components: libd2005
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\bds 4.0 libd2006 source cpp"; StatusMsg: "Adding library paths (Delphi 2006)..."; Components: libd2006
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP borland\bds 5.0 libd2007 source cpp"; StatusMsg: "Adding library paths (Delphi 2007)..."; Components: libd2007
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP codegear\bds 6.0 libd2009 source cpp"; StatusMsg: "Adding library paths (Delphi 2009)..."; Components: libd2009
Filename: "{app}\help\instlhlp.exe"; Parameters: "-IP codegear\bds 7.0 libd2010 source cpp"; StatusMsg: "Adding library paths (Delphi 2010)..."; Components: libd2010

Filename: "{app}\help\instlhlp.exe"; Parameters: "-IH CtxScriptHelp ctxscript.hlp borland\delphi 7.0 d7"; StatusMsg: "Installing Help (Delphi 7)..."; Check: DoInstHelp('libd7')

[UninstallRun]
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\delphi 7.0 libd7 source"; StatusMsg: "Removing library paths (Delphi 7)..."; Components: libd7
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\bds 3.0 libd2005 source"; StatusMsg: "Removing library paths (Delphi 2005)..."; Components: libd2005
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\bds 4.0 libd2006 source cpp"; StatusMsg: "Removing library paths (Delphi 2006)..."; Components: libd2006
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP borland\bds 5.0 libd2007 source cpp"; StatusMsg: "Removing library paths (Delphi 2007)..."; Components: libd2007
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP codegear\bds 6.0 libd2009 source cpp"; StatusMsg: "Adding library paths (Delphi 2009)..."; Components: libd2009
Filename: "{app}\help\instlhlp.exe"; Parameters: "-UP codegear\bds 7.0 libd2010 source cpp"; StatusMsg: "Adding library paths (Delphi 2010)..."; Components: libd2010

Filename: "{app}\help\instlhlp.exe"; Parameters: "-UH CtxScriptHelp ctxscript.hlp borland\delphi 7.0 d7"; StatusMsg: "Removing Help (Delphi 7)..."; Check: DoInstHelp('libd7')

[Code]
function DoInstHelp(const CompName: String): Boolean;
begin
  Result := IsTaskSelected('insthelp') and IsComponentSelected(CompName);
end;
