rem ********************************************************************************************************************
rem $Id: makeSDK.bat,v 1.2 2004-04-15 12:54:11 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
rem ********************************************************************************************************************
start /w %SAMPLE_ARCHIVER% _deploy_\phoa-sdk.zip ..\phPhoa.pas ..\phMetadata.pas phoadump\ReadMe.txt phoadump\*.cfg phoadump\*.dof phoadump\*.dpr phoadump\*.pas
if errorlevel == 1 goto :err

goto :success
:err
pause
:success
cd ..
