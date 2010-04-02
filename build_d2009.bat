@echo off

touch source\*.*

cd packages

SET package=CtxScriptPkg
SET dpath=e:\Programs\Borland

set buildto=..\lib\d2009

"%dpath%\..\CodeGear\RAD Studio\6.0\bin\dcc32.exe" -JL -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2009.dpk

move ..\source\*.hpp %buildto%
move ..\source\*.obj %buildto%
move ..\source\designtime\*.hpp %buildto%
move ..\source\designtime\*.obj %buildto%
move *.bpi %buildto%
move *.bpl %buildto%
move *.lib %buildto%
move *.obj %buildto%
move *.hpp %buildto%

del %buildto%\CtxGridView.*
del %buildto%\CtxPropView.*

cd ..