@echo off
rem ** Making bundle of language files and the designer application
rem ** (c)2004 Dmitry Kann

if exist IS-install\phoa-lang-src.zip del IS-install\phoa-lang-src.zip

rem -m3    = compression normal
rem -afzip = create zip archive
start /w C:\Progra~1\WinRAR\WinRAR.exe a -m3 -afzip IS-install\phoa-lang-src.zip *.dtls ..\dttm\dttm.exe Lang-distribution-ReadMe.txt IS-install\phoa.iss IS-install\eula-eng.rtf IS-install\eula-rus.rtf
