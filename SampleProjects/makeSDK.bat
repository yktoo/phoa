start /w %SAMPLE_ARCHIVER% _deploy_\phoa-sdk.zip ..\phPhoa.pas ..\phMetadata.pas phoadump\ReadMe.txt phoadump\*.cfg phoadump\*.dof phoadump\*.dpr phoadump\*.pas
if errorlevel == 1 goto :err

goto :success
:err
pause
:success
cd ..
