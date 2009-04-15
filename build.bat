@echo off

touch source\*.*

cd source

SET package=CtxScriptPkg
SET dpath=D:\Program Files\Borland

set buildto=..\libt\d5
"%dpath%\Delphi5\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D5.dpk
set buildto=..\lib\d5
"%dpath%\Delphi5\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D5.dpk

set buildto=..\libt\d6
"%dpath%\Delphi6\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D6.dpk 
set buildto=..\lib\d6
"%dpath%\Delphi6\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D6.dpk 

set buildto=..\libt\d7
"%dpath%\Delphi7\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D7.dpk
set buildto=..\lib\d7
"%dpath%\Delphi7\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D7.dpk

set buildto=..\libt\d2005
"%dpath%\BDS\3.0\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2005.dpk
set buildto=..\lib\d2005
"%dpath%\BDS\3.0\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2005.dpk

cd ..
