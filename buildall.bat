echo Clean up everything
@call cleanup.bat

echo Build everything
@call build.bat > build.log

echo Build setup
@call buildsetup.bat

@call release.bat 140