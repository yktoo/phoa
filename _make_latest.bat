@echo off
rem ********************************************************************************************************************
rem $Id: _make_latest.bat,v 1.1 2005-03-07 10:36:55 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright DK Software, http://www.dk-soft.org/
rem ********************************************************************************************************************
rem ** Making a bundle of the latest build of the program and associated files

set ZIPNAME=phoa.zip

if exist %ZIPNAME% del %ZIPNAME%

rem Compile the application
call _make_.bat app

rem -m3    = compression normal
rem -afzip = create zip archive
start /w C:\Progra~1\WinRAR\WinRAR.exe a -m3 -afzip %ZIPNAME% phoa.exe CHANGELOG Language\Russian.lng
