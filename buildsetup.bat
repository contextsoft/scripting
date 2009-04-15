
@set package=ctxscript
@set innosetup="D:\Program Files\Inno Setup 4\ISCC.exe"

@echo Building %package%.iss...
@%innosetup% %package%.iss > %package%.log
@echo Done.

@echo Building %package%_trial.iss...
@%innosetup% %package%_trial.iss > %package%_trial.log
@echo Done.
