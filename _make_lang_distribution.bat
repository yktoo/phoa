@echo off
rem ********************************************************************************************************************
rem $Id: _make_lang_distribution.bat,v 1.3 2004-05-24 05:30:37 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
rem ********************************************************************************************************************
rem ** Making bundle of language files and the designer application

if exist IS-install\phoa-lang-src.zip del IS-install\phoa-lang-src.zip

rem -m3    = compression normal
rem -afzip = create zip archive
start /w C:\Progra~1\WinRAR\WinRAR.exe a -m3 -afzip IS-install\phoa-lang-src.zip *.xdtls ..\dttm\dttm.exe Lang-distribution-ReadMe.txt IS-install\SetupMessages.txt IS-install\eula-eng.rtf IS-install\eula-rus.rtf
