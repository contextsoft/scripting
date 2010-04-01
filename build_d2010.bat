@echo off

touch source\*.*

cd packages

SET package=CtxScriptPkg
SET dpath=e:\programs\Embarcadero

set buildto=..\lib\d2010
"%dpath%\RAD Studio\7.0\bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2010.dpk

cd ..
