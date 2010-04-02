@echo off

touch source\*.*

cd packages

SET package=CtxScriptPkg
SET dpath=e:\Programs\Borland
SET include=e:\projects\Context\Scripting\source\

set buildto=..\lib\d2005

"%dpath%\BDS\3.0\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2005.dpk

cd ..