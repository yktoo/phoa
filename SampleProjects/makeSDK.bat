rem ********************************************************************************************************************
rem $Id: makeSDK.bat,v 1.3 2004-09-11 17:52:37 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright 2002-2004 DK Software, http://www.dk-soft.org/
rem ********************************************************************************************************************
start /w %SAMPLE_ARCHIVER% _deploy_\phoa-sdk.zip ..\phPhoa.pas ..\phMetadata.pas phoadump\ReadMe.txt phoadump\*.cfg phoadump\*.dof phoadump\*.dpr phoadump\*.pas
if errorlevel == 1 goto :err

goto :success
:err
pause
:success
cd ..
