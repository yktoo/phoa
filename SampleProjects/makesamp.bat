rem ********************************************************************************************************************
rem $Id: makesamp.bat,v 1.3 2004-09-11 17:52:37 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright 2002-2004 DK Software, http://www.dk-soft.org/
rem ********************************************************************************************************************
cd %1

del *.exe

%COMPILER% %1.dpr %OPTIONS% -$%SWITCHES% -U%LIBRARY_PATH%
if errorlevel == 1 goto :err

del *.~*
del *.dcu
del *.ddp

start /w %SAMPLE_ARCHIVER% ..\_deploy_\%1-src.zip ReadMe.txt *.cfg *.dof *.dpr *.pas
if errorlevel == 1 goto :err

start /w %SAMPLE_ARCHIVER% ..\_deploy_\%1-exe.zip ReadMe.txt %1.exe
if errorlevel == 1 goto :err
copy ReadMe.txt ..\_deploy_\%1-readme.txt
if errorlevel == 1 goto :err

goto :success
:err
pause
:success
cd ..
