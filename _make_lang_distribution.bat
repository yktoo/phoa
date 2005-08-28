@echo off
rem ********************************************************************************************************************
rem $Id: _make_lang_distribution.bat,v 1.12 2005-08-28 06:02:59 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright DK Software, http://www.dk-soft.org/
rem ********************************************************************************************************************
rem ** Making bundle of language files 

set VERSION=1.1.10beta
set ZIPNAME=phoa-lang-src-%VERSION%.zip

if exist IS-install\%ZIPNAME% del IS-install\%ZIPNAME%

rem -m3    = compression normal
rem -afzip = create zip archive
start /w C:\Progra~1\WinRAR\WinRAR.exe a -m3 -afzip IS-install\%ZIPNAME% phoa.dklang Lang-distribution-ReadMe.txt
