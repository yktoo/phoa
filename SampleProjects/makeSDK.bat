rem ********************************************************************************************************************
rem $Id: makeSDK.bat,v 1.4 2004-12-31 13:38:58 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright DK Software, http://www.dk-soft.org/
rem ********************************************************************************************************************
start /w %SAMPLE_ARCHIVER% _deploy_\phoa-sdk.zip ..\phPhoa.pas ..\phMetadata.pas phoadump\ReadMe.txt phoadump\*.cfg phoadump\*.dof phoadump\*.dpr phoadump\*.pas
if errorlevel == 1 goto :err

goto :success
:err
pause
:success
cd ..
