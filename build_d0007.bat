@echo off

touch source\*.*

cd packages

SET package=CtxScriptPkg
SET dpath=e:\programs\Borland

set buildto=..\lib\d7
"%dpath%\Delphi7\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D7.dpk

cd ..
