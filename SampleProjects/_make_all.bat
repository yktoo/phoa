@echo off
rem ********************************************************************************************************************
rem $Id: _make_all.bat,v 1.2 2004-04-15 12:54:11 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
rem ********************************************************************************************************************

rem -B = Rebuild all
rem -W = Output warning messages
rem -H = Output hint messages
set OPTIONS=-B -W -H
set SWITCHES=A8B-C-D-G+H+I+J-L-M-O+P+Q-R-T-U-V+W-X+Y-Z1
set DELPHI=C:\Progra~1\Borland\Delphi7
set LIBRARY_PATH=%DELPHI%\tb2k\Source;%DELPHI%\tbx;%DELPHI%\Graphics32;%DELPHI%\RX;%DELPHI%\RX\Units;c:\delphi\homepr~1\dtlang~1;%DELPHI%\vtv;%DELPHI%\vst\Source\Common;%DELPHI%\vst\Source\VirtualExplorerTree
set COMPILER=%DELPHI%\Bin\dcc32.exe

rem -m3    = compression normal
rem -afzip = create zip archive
set SAMPLE_ARCHIVER=C:\Progra~1\WinRAR\WinRAR.exe a -m3 -afzip
set MAKER_BATCH=makesamp.bat

rem /Q = no confirmation on wildcard deletion
del /Q _deploy_\*.*


call %MAKER_BATCH% phoadump
call %MAKER_BATCH% phoaxml
call makeSDK.bat
