
@set package=ctxscript
@set innosetup="e:\programs\Inno Setup 5\ISCC.exe"

@echo Building %package%.iss...
@%innosetup% %package%.iss > %package%.log
@echo Done.

@echo Building %package%_trial.iss...
@%innosetup% %package%_trial.iss > %package%_trial.log
@echo Done.
