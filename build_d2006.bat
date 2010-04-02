@echo off

touch source\*.*

cd packages

SET package=CtxScriptPkg
SET dpath=e:\Programs\Borland

set buildto=..\lib\d2006

"%dpath%\BDS\4.0\Bin\dcc32.exe" -JL -NO%buildto% -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2006.dpk

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