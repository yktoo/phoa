;***********************************************************************************************************************
;   $Id: phoa.iss,v 1.11 2004-11-08 10:28:02 dale Exp $
;-----------------------------------------------------------------------------------------------------------------------
;   PhoA image arranging and searching tool
;   Copyright 2002-2004 DK Software, http://www.dk-soft.org/
;***********************************************************************************************************************
[Setup]
  MinVersion             = 4.0,4.0
  AppName                = PhoA
  AppVersion             = 1.1.6 beta
  AppVerName             = PhoA v1.1.6 beta
  AppCopyright           = Copyright ©2002-2004 DK Software
  AppPublisher           = DK Software
  AppPublisherURL        = http://www.dk-soft.org/
  AppSupportURL          = http://www.dk-soft.org/forum/
  AppUpdatesURL          = http://www.dk-soft.org/redir.php?action=vercheck&product=phoa&version=116beta
  AppMutex               = PHOA_RUNNING_MUTEX
  AllowNoIcons           = yes
  ChangesAssociations    = yes
  DisableStartupPrompt   = yes
  DefaultDirName         = {pf}\DK Software\PhoA
  DefaultGroupName       = PhoA (Photo Album)
  OutputDir              = .
  OutputBaseFilename     = phoa-setup-1.1.6beta
  VersionInfoVersion     = 1.1.6
  VersionInfoTextVersion = 1.1.6 beta
  WizardImageFile        = SetupImage.bmp
  WizardSmallImageFile   = SetupSmallImage.bmp
  ; -- Compression
  SolidCompression       = yes
  Compression            = lzma

[Languages]
  Name: "en"; MessagesFile: compiler:Default.isl;             LicenseFile: eula-eng.rtf
  Name: "ru"; MessagesFile: compiler:Languages\Russian.isl;   LicenseFile: eula-rus.rtf
  Name: "de"; MessagesFile: compiler:Languages\German.isl;    LicenseFile: eula-eng.rtf
  Name: "br"; MessagesFile: compiler:BrazilianPortuguese.isl; LicenseFile: eula-eng.rtf
  Name: "ua"; MessagesFile: compiler:Ukrainian.isl;           LicenseFile: eula-eng.rtf
  
[Tasks]
  Name: desktopicon;        Description: {cm:CreateDesktopIcon};             GroupDescription: {cm:AdditionalIcons};
  Name: desktopicon\common; Description: {cm:IconsAllUsers};                 GroupDescription: {cm:AdditionalIcons}; Flags: exclusive
  Name: desktopicon\user;   Description: {cm:IconsCurUser};                  GroupDescription: {cm:AdditionalIcons}; Flags: exclusive unchecked
  Name: quicklaunchicon;    Description: {cm:CreateQuickLaunchIcon};         GroupDescription: {cm:AdditionalIcons};
  Name: associate;          Description: {cm:AssocFileExtension,PhoA,.phoa};

[Components]
  Name: main;    Description: {cm:CompMain};        Types: full compact custom; Flags: fixed
  Name: help;    Description: {cm:CompHelp};        Types: full
  Name: help\en; Description: {cm:CompHelpEn};      Types: full
  Name: help\ru; Description: {cm:CompHelpRu};      Types: full
  Name: sample;  Description: {cm:CompSampleAlbum}; Types: full
  Name: api;     Description: {cm:CompPhoaAPI};     Types: full

[Files]
;Application files
  Source: "..\phoa.exe";                DestDir: "{app}";              Components: main
  Source: "..\Language\Russian.lng";    DestDir: "{app}\Language";     Components: main
  Source: "..\phoa-eng.chm";            DestDir: "{app}";              Components: help\en
  Source: "..\phoa-rus.chm";            DestDir: "{app}";              Components: help\ru
;Sample content
  Source: "Sample album\sample.phoa";   DestDir: "{app}\Sample album"; Components: sample
  Source: "Sample album\goldgate.jpg";  DestDir: "{app}\Sample album"; Components: sample
  Source: "Sample album\river.jpg";     DestDir: "{app}\Sample album"; Components: sample
  Source: "Sample album\illusion.png";  DestDir: "{app}\Sample album"; Components: sample
;API file
  Source: "..\phMetadata.pas";          DestDir: "{app}\API";          Components: api
  Source: "..\phPhoa.pas";              DestDir: "{app}\API";          Components: api

[INI]
  Filename: "{app}\phoa.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://www.dk-soft.org/"

[Icons]
  Name: "{group}\PhoA";                       Filename: "{app}\phoa.exe";     Components: main;   Comment: {cm:PhoaDesc}
  Name: "{commondesktop}\PhoA";               Filename: "{app}\phoa.exe";     Components: main;   Comment: {cm:PhoaDesc}; Tasks: desktopicon\common
  Name: "{userdesktop}\PhoA";                 Filename: "{app}\phoa.exe";     Components: main;   Comment: {cm:PhoaDesc}; Tasks: desktopicon\user
  Name: "{code:QuickLaunch|{pf}}\PhoA";       Filename: "{app}\phoa.exe";     Components: main;   Comment: {cm:PhoaDesc}; Tasks: quicklaunchicon
  Name: "{group}\{cm:UninstallProgram,PhoA}"; Filename: "{uninstallexe}";     Components: main;
  Name: "{group}\{cm:ProgramOnTheWeb,PhoA}";  Filename: "{app}\phoa.url";     Components: main;
  Name: "{group}\{cm:SampleAlbum}";           Filename: "{app}\phoa.exe";     Components: sample; Parameters: """{app}\Sample album\sample.phoa"""; IconFilename: "{app}\phoa.exe"; IconIndex: 1
  Name: "{group}\{cm:HelpRu}";                Filename: "{app}\phoa-rus.chm"; Components: help\ru;
  Name: "{group}\{cm:HelpEn}";                Filename: "{app}\phoa-eng.chm"; Components: help\en;

[Registry]
  Root: HKCR; Subkey: ".phoa";                                                                       ValueType: string; ValueData: "phoa.photoalbum";           Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum";                                                             ValueType: string; ValueData: {cm:PhoaFmtName};            Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum\shell\open\command";                                          ValueType: string; ValueData: """{app}\phoa.exe"" ""%1"""; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum\DefaultIcon";                                                 ValueType: string; ValueData: """{app}\phoa.exe"",1";      Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCU; Subkey: "Software\DKSoftware\PhoA\Preferences"; ValueName: "@ISettingID_Gen_Language"; ValueType: string; ValueData: {cm:LangID};                 Flags: uninsdeletevalue uninsdeletekeyifempty

[Run]
  Filename: "{app}\phoa.exe"; Parameters: {code:PhoaStartupParams|}; Description: {cm:LaunchProgram,PhoA}; Flags: nowait postinstall skipifsilent

[UninstallDelete]
  Type: files;      Name: "{app}\phoa.url"
  Type: dirifempty; Name: "{app}"

[Messages]
BeveledLabel=DK Software

[CustomMessages]
#include "SetupMessages.txt"

[Code]

  function QuickLaunch(Default: String): String;
  begin
    Result := ExpandConstant('{userappdata}')+'\Microsoft\Internet Explorer\Quick Launch';
  end;
  
  function PhoaStartupParams(Default: String): String;
  begin
    if ShouldProcessEntry('sample', '')=srYes then Result := '"Sample album\sample.phoa"' else Result := '';
  end;

