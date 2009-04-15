@echo off

touch source\*.*

cd source

SET package=CtxScriptPkg
SET dpath=D:\Program Files\Borland

set buildto=..\lib\d5
"%dpath%\Delphi5\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D5.dpk

cd ..
