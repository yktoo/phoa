@echo off
rem =======================================================
rem PhoA [Photo Album] makefile
rem Copyright ©2002-2004 Dmitry Kann, http://phoa.narod.ru
rem =======================================================

rem -B = Rebuild all
rem -W = Output warning messages
rem -H = Output hint messages
set OPTIONS=-B -W -H
set SWITCHES=A8B-C-D-G+H+I+J-L-M-O+P+Q-R-T-U-V+W-X+Y-Z1
set DELPHI=C:\Progra~1\Borland\Delphi7
set LIBRARY_PATH=%DELPHI%\tb2k\Source;%DELPHI%\tbx;%DELPHI%\Graphics32;%DELPHI%\RX;%DELPHI%\RX\Units;c:\delphi\homepr~1\dtlang~1;%DELPHI%\vtv;%DELPHI%\vst\Source\Common;%DELPHI%\vst\Source\VirtualExplorerTree
set COMPILER=%DELPHI%\Bin\dcc32.exe

set HELP_COMPILER="C:\Program Files\HTML Help Workshop\hhc.exe"

set SETUP_COMPILER="C:\Program Files\Inno Setup 4\iscc.exe"

rem == Compile Delphi DPR project ==
echo.
echo == Compile Delphi DPR project ==
%COMPILER% phoa.dpr %OPTIONS% -$%SWITCHES% -U%LIBRARY_PATH%
if errorlevel == 1 goto :err
del *.~*
del *.dcu
del *.ddp
del *.bkf
del *.bkm
del GraphicEx\*.dcu

rem == Compile Help CHM project ==
echo.
echo == Compile Help CHM project (English)
pushd
cd Help\en
%HELP_COMPILER% phoa-eng.hhp
if not errorlevel == 1 goto :err
move phoa-eng.chm ..\..
if errorlevel == 1 goto :err
echo.
echo == Compile Help CHM project (Russian)
cd ..\ru
%HELP_COMPILER% phoa-rus.hhp
if not errorlevel == 1 goto :err
move phoa-rus.chm ..\..
if errorlevel == 1 goto :err
popd

rem == Compile Installation ISS script ==
echo.
echo == Compile Installation ISS script ==
cd ..\..\IS-Install
%SETUP_COMPILER% phoa.iss
if errorlevel == 1 goto :err
goto :success
:err
pause
:success