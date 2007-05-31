@echo off
rem ********************************************************************************************************************
rem $Id: _make_.bat,v 1.21 2007-05-31 03:02:42 dale Exp $
rem --------------------------------------------------------------------------------------------------------------------
rem PhoA image arranging and searching tool
rem Copyright DK Software, http://www.dk-soft.org/
rem ********************************************************************************************************************

set CLEANER="C:\Delphi\CVS projects\dale\phoa\_cleanup.bat"

rem DK Software Text Preprocessor defines
set PREPROCESSOR="C:\Delphi\CVS projects\dale\txpproc\txpproc.exe"
set PREPROCESSOR_CFG="C:\Delphi\CVS projects\dale\phoa\phoa-txpproc.ini"
set PREPROCESSOR_OUT="C:\Delphi\CVS projects\dale\phoa\help\processed"

rem -B = Rebuild all
rem -W = Output warning messages
rem -H = Output hint messages
set OPTIONS=-B -W -H
set SWITCHES=A8B-C-D-G+H+I+J-L-M-O+P+Q-R-T-U-V+W-X+Y-Z1
set DELPHI=C:\Program Files\Borland\Delphi7
set LIBRARY_PATH=%DELPHI%\tb2k\Source;%DELPHI%\tbx;%DELPHI%\GraphicEx;%DELPHI%\Graphics32;%DELPHI%\RX\Units;%DELPHI%\vtv\Source;%DELPHI%\vst\Source;%DELPHI%\SynEdit\Source;c:\Delphi\CVSpro~1\dale\dklang
set COMPILER="%DELPHI%\Bin\dcc32.exe"

set HELP_COMPILER="C:\Program Files\HTML Help Workshop\hhc.exe"

set SETUP_COMPILER="C:\Program Files\Inno Setup 5\iscc.exe"

if "%1"=="app" goto compapp
if "%1"=="help" goto comphelp
if "%1"=="setup" goto compsetup

rem == Compile Delphi DPR project ==
:compapp
echo.
echo == Compile Delphi DPR project ==
%COMPILER% phoa.dpr %OPTIONS% -$%SWITCHES% "-U%LIBRARY_PATH%"
if errorlevel == 1 goto :err
if "%1"=="app" goto success

rem == Compile Help CHM project ==
rem --- English Help
:comphelp
cd Help\en
echo.
echo == Preprocess Help files (English)
rmdir /s /q %PREPROCESSOR_OUT%
%PREPROCESSOR% *.html %PREPROCESSOR_CFG% %PREPROCESSOR_OUT%
if errorlevel == 1 goto err
copy phoa-eng.hhp ..\processed
copy phoa-eng.hhc ..\processed
copy phoa-eng.hhk ..\processed
copy browse-mode-chart.png ..\processed
copy view-mode-chart.png ..\processed
copy pic-organization.png ..\processed

cd ..\processed
echo.
echo == Compile Help CHM project (English)
%HELP_COMPILER% phoa-eng.hhp
if not errorlevel == 1 goto err
move phoa-eng.chm ..\..
if errorlevel == 1 goto err

rem --- Russian Help
cd ..\ru
echo.
echo == Preprocess Help files (Russian)
rmdir /s /q %PREPROCESSOR_OUT%
%PREPROCESSOR% *.html %PREPROCESSOR_CFG% %PREPROCESSOR_OUT%
if errorlevel == 1 goto err
copy phoa-rus.hhp ..\processed
copy phoa-rus.hhc ..\processed
copy phoa-rus.hhk ..\processed
copy browse-mode-chart.png ..\processed
copy view-mode-chart.png ..\processed
copy pic-organization.png ..\processed

cd ..\processed
echo.
echo == Compile Help CHM project (Russian)
%HELP_COMPILER% phoa-rus.hhp
if not errorlevel == 1 goto err
move phoa-rus.chm ..\..
if errorlevel == 1 goto err

rem --- German Help
cd ..\de
echo.
echo == Preprocess Help files (German)
rmdir /s /q %PREPROCESSOR_OUT%
%PREPROCESSOR% *.html %PREPROCESSOR_CFG% %PREPROCESSOR_OUT%
if errorlevel == 1 goto err
copy phoa-deu.hhp ..\processed
copy phoa-deu.hhc ..\processed
copy phoa-deu.hhk ..\processed
copy browse-mode-chart.png ..\processed
copy view-mode-chart.png ..\processed
copy pic-organization.png ..\processed

cd ..\processed
echo.
echo == Compile Help CHM project (German)
%HELP_COMPILER% phoa-deu.hhp
if not errorlevel == 1 goto err
move phoa-deu.chm ..\..
if errorlevel == 1 goto err

cd ..\..
if "%1"=="help" goto success

rem == Compile Installation ISS script ==
:compsetup
echo.
echo == Compile Installation ISS script ==
cd IS-Install
%SETUP_COMPILER% phoa.iss
if errorlevel == 1 goto err
if "%1"=="setup" goto success

goto success
:err
pause
:success
rmdir /s /q %PREPROCESSOR_OUT%
call %CLEANER%