@echo off
rem ********************************************************************************************************************
rem $Id: _make_lang_distribution.bat,v 1.5 2004-11-09 15:07:46 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright 2002-2004 DK Software, http://www.dk-soft.org/
rem ********************************************************************************************************************
rem ** Making bundle of language files and the designer application

if exist IS-install\phoa-lang-src.zip del IS-install\phoa-lang-src.zip

rem -m3    = compression normal
rem -afzip = create zip archive
start /w C:\Progra~1\WinRAR\WinRAR.exe a -m3 -afzip IS-install\phoa-lang-src.zip phoa.dklang Lang-distribution-ReadMe.txt
