;***********************************************************************************************************************
;   $Id: phoa.iss,v 1.2 2004-04-15 12:54:11 dale Exp $
;-----------------------------------------------------------------------------------------------------------------------
;   PhoA image arranging and searching tool
;   Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
;***********************************************************************************************************************
[Setup]
  MinVersion=4.0,4.0
  AppName=PhoA
  AppVersion=1.1.4 beta
  AppVerName=PhoA v1.1.4 beta
  AppCopyright=Copyright ©2002-2004 Dmitry Kann
  AppPublisher=DaleTech
  AppPublisherURL=http://phoa.narod.ru/
  AppSupportURL=mailto:phoa@narod.ru
  AppUpdatesURL=http://phoa.narod.ru/
  AppMutex=PHOA_RUNNING_MUTEX
  AllowNoIcons=yes
  ChangesAssociations=yes
  DisableStartupPrompt=yes
  DefaultDirName={pf}\DaleTech\PhoA
  DefaultGroupName=PhoA (Photo Album)
  OutputDir=.
  OutputBaseFilename=phoa-setup-1.1.4beta
  VersionInfoVersion=1.1.4
  WizardImageFile=Setup-Image.bmp
  WizardSmallImageFile=Setup-Small-Image.bmp
  ; -- Compression
  SolidCompression=yes
  Compression=lzma

[Languages]
  Name: "en"; MessagesFile: compiler:Default.isl;             LicenseFile: eula-eng.rtf
  Name: "ru"; MessagesFile: compiler:Russian.isl;             LicenseFile: eula-rus.rtf
  Name: "de"; MessagesFile: compiler:Languages\German.isl;    LicenseFile: eula-deu.rtf
  Name: "br"; MessagesFile: compiler:BrazilianPortuguese.isl; LicenseFile: eula-brp.rtf
  Name: "ua"; MessagesFile: compiler:Ukrainian.isl;           LicenseFile: eula-ukr.rtf
  
[Tasks]
;English entries
  Name: desktopicon;        Languages: en; Description: "Create a &Desktop icon";                                 GroupDescription: "Additional icons:";
  Name: desktopicon\common; Languages: en; Description: "For all users";                                          GroupDescription: "Additional icons:"; Flags: exclusive
  Name: desktopicon\user;   Languages: en; Description: "For the current user only";                              GroupDescription: "Additional icons:"; Flags: exclusive unchecked
  Name: quicklaunchicon;    Languages: en; Description: "Create a &Quick Launch icon";                            GroupDescription: "Additional icons:";
  Name: associate;          Languages: en; Description: "&Associate .phoa extension with PhoA (recommended)";     GroupDescription: "Other:";
;Russian entries
  Name: desktopicon;        Languages: ru; Description: "Создать значок на &Рабочем столе";                       GroupDescription: "Дополнительные значки:";
  Name: desktopicon\common; Languages: ru; Description: "Для всех пользователей";                                 GroupDescription: "Дополнительные значки:"; Flags: exclusive
  Name: desktopicon\user;   Languages: ru; Description: "Только для текущего пользователя";                       GroupDescription: "Дополнительные значки:"; Flags: exclusive unchecked
  Name: quicklaunchicon;    Languages: ru; Description: "Создать значок в панели &Быстрого запуска";              GroupDescription: "Дополнительные значки:";
  Name: associate;          Languages: ru; Description: "&Связать файлы .phoa с программой PhoA (рекомендуется)"; GroupDescription: "Прочее:";
;German entries
  Name: desktopicon;        Languages: de; Description: "Ein &Desktop Icon erstellen";                            GroupDescription: "Zusдtzliche Icons:";
  Name: desktopicon\common; Languages: de; Description: "Fьr alle Benutzer";                                      GroupDescription: "Zusдtzliche Icons:"; Flags: exclusive
  Name: desktopicon\user;   Languages: de; Description: "Nur fьr den aktuellen Benutzer";                         GroupDescription: "Zusдtzliche Icons:"; Flags: exclusive unchecked
  Name: quicklaunchicon;    Languages: de; Description: "Ein &Quick Launch Icon erstellen";                       GroupDescription: "Zusдtzliche Icons:";
  Name: associate;          Languages: de; Description: "Die D&ateinamenerweiterung .phoa mit PhoA verknьpfen (empfohlen)"; GroupDescription: "Mehr:";
;Brazilian Portuguese entries
  Name: desktopicon;        Languages: br; Description: "Criar нcone na бrea de &trabalho";                       GroupDescription: "Нcones adicionais:";
  Name: desktopicon\common; Languages: br; Description: "Para todos os usuбrios";                                 GroupDescription: "Нcones adicionais:"; Flags: exclusive
  Name: desktopicon\user;   Languages: br; Description: "Somente para usuбrio atual";                             GroupDescription: "Нcones adicionais:"; Flags: exclusive unchecked
  Name: quicklaunchicon;    Languages: br; Description: "Criar нcone de inicializaзгo &rбpida";                   GroupDescription: "Нcones adicionais:";
  Name: associate;          Languages: br; Description: "&Associar extensгo .phoa com PhoA (recomendado)";        GroupDescription: "Outro:";
;Ukraine entries
  Name: desktopicon;        Languages: ua; Description: "Створити позначку на  &Робочему столі";                  GroupDescription: "Додаткові позначки:";
  Name: desktopicon\common; Languages: ua; Description: "Для усіх користувачів";                                  GroupDescription: "Додаткові позначки:"; Flags: exclusive
  Name: desktopicon\user;   Languages: ua; Description: "Тільки для поточного користувача";                       GroupDescription: "Додаткові позначки:"; Flags: exclusive unchecked
  Name: quicklaunchicon;    Languages: ua; Description: "Створити позначку у панелі &Швидкого запуску";           GroupDescription: "Додаткові значки:";
  Name: associate;          Languages: ua; Description: "&Зв'язати файли .phoa з програмою PhoA (рекомендуєтся)"; GroupDescription: "Інше:";

[Components]
;English entries
  Name: main;    Languages: en; Description: "Main Files";                   Types: full compact custom; Flags: fixed
  Name: help;    Languages: en; Description: "Help Files";                   Types: full
  Name: help\en; Languages: en; Description: "English";                      Types: full
  Name: help\ru; Languages: en; Description: "Russian";                      Types: full
  Name: sample;  Languages: en; Description: "Sample photo album";           Types: full
  Name: api;     Languages: en; Description: "PhoA API (for developers)";    Types: full
;Russian entries
  Name: main;    Languages: ru; Description: "Основные файлы";               Types: full compact custom; Flags: fixed
  Name: help;    Languages: ru; Description: "Файлы Справочной системы";     Types: full
  Name: help\en; Languages: ru; Description: "Английский язык";              Types: full
  Name: help\ru; Languages: ru; Description: "Русский язык";                 Types: full
  Name: sample;  Languages: ru; Description: "Пример фотоальбома";           Types: full
  Name: api;     Languages: ru; Description: "PhoA API (для разработчиков)"; Types: full
;German entries
  Name: main;    Languages: de; Description: "Programmdateien";              Types: full compact custom; Flags: fixed
  Name: help;    Languages: de; Description: "Hilfedateien";                 Types: full
  Name: help\en; Languages: de; Description: "Englisch";                     Types: full
  Name: help\ru; Languages: de; Description: "Russisch";                     Types: full
  Name: sample;  Languages: de; Description: "Beispiel Fotoalbum";           Types: full
  Name: api;     Languages: de; Description: "PhoA API (for developers)";    Types: full
;Brazilian Portuguese entries
  Name: main;    Languages: br; Description: "Arquivos principais";          Types: full compact custom; Flags: fixed
  Name: help;    Languages: br; Description: "Arquivos de ajuda";            Types: full
  Name: help\en; Languages: br; Description: "Inglкs";                       Types: full
  Name: help\ru; Languages: br; Description: "Russo";                        Types: full
  Name: sample;  Languages: br; Description: "Бlbum exemplo";                Types: full
  Name: api;     Languages: br; Description: "PhoA API (for developers)";    Types: full
;Ukrainian entries
  Name: main;    Languages: ua; Description: "Основні файли";                Types: full compact custom; Flags: fixed
  Name: help;    Languages: ua; Description: "Файли  ДовідковоЇ системи";    Types: full
  Name: help\en; Languages: ua; Description: "Англійска мова";               Types: full
  Name: help\ru; Languages: ua; Description: "Російска мова";                Types: full
  Name: sample;  Languages: ua; Description: "Приклад фотоальбому";          Types: full
  Name: api;     Languages: ua; Description: "PhoA API (для розробників)";   Types: full


[Files]
;Application files
  Source: "..\phoa.exe";                DestDir: "{app}";              Components: main
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
  Filename: "{app}\phoa.url"; Languages: en de br; Section: "InternetShortcut"; Key: "URL"; String: "http://phoa.narod.ru/en/"
  Filename: "{app}\phoa.url"; Languages: ru ua;    Section: "InternetShortcut"; Key: "URL"; String: "http://phoa.narod.ru/"

[Icons]
;English entries
  Name: "{group}\PhoA";                       Languages: en; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Picture arranging program"
  Name: "{group}\PhoA help (Russian)";        Languages: en; Filename: "{app}\phoa-rus.chm";   Components: help\ru; Comment: "PhoA help (Russian)"
  Name: "{group}\PhoA help (English)";        Languages: en; Filename: "{app}\phoa-eng.chm";   Components: help\en; Comment: "PhoA help (English)"
  Name: "{group}\Sample photo album";         Languages: en; Filename: "{app}\phoa.exe";       Components: sample;  Comment: "Sample PhoA photo album"; Parameters: """{app}\Sample album\sample.phoa"""; IconFilename: "{app}\phoa.exe"; IconIndex: 1
  Name: "{group}\PhoA home site";             Languages: en; Filename: "{app}\phoa.url";       Components: main;    Comment: "phoa.narod.ru"
  Name: "{group}\Uninstall PhoA";             Languages: en; Filename: "{uninstallexe}";       Components: main;    Comment: "Completely remove PhoA and all its components"
  Name: "{commondesktop}\PhoA";               Languages: en; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Picture arranging program"; Tasks: desktopicon\common
  Name: "{userdesktop}\PhoA";                 Languages: en; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Picture arranging program"; Tasks: desktopicon\user
  Name: "{code:QuickLaunch|{pf}}\PhoA";       Languages: en; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Picture arranging program"; Tasks: quicklaunchicon
;Russian entries
  Name: "{group}\PhoA";                       Languages: ru; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Программа для каталогизации изображений и составления фотоальбомов"
  Name: "{group}\Справка по PhoA (Русская)";  Languages: ru; Filename: "{app}\phoa-rus.chm";   Components: help\ru; Comment: "Справка по программному продукту PhoA"
  Name: "{group}\Справка по PhoA (English)";  Languages: ru; Filename: "{app}\phoa-eng.chm";   Components: help\en; Comment: "PhoA help"
  Name: "{group}\Пример фотоальбома";         Languages: ru; Filename: "{app}\phoa.exe";       Components: sample;  Comment: "Образец фотоальбома PhoA"; Parameters: """{app}\Sample album\sample.phoa"""; IconFilename: "{app}\phoa.exe"; IconIndex: 1
  Name: "{group}\Сайт PhoA";                  Languages: ru; Filename: "{app}\phoa.url";       Components: main;    Comment: "phoa.narod.ru"
  Name: "{group}\Удалить PhoA";               Languages: ru; Filename: "{uninstallexe}";       Components: main;    Comment: "Полностью удалить PhoA и все компоненты программы"
  Name: "{commondesktop}\PhoA";               Languages: ru; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Программа для каталогизации изображений и составления фотоальбомов"; Tasks: desktopicon\common
  Name: "{userdesktop}\PhoA";                 Languages: ru; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Программа для каталогизации изображений и составления фотоальбомов"; Tasks: desktopicon\user
  Name: "{code:QuickLaunch|}\PhoA";           Languages: ru; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Программа для каталогизации изображений и составления фотоальбомов"; Tasks: quicklaunchicon
;German entries
  Name: "{group}\PhoA";                       Languages: de; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Bildverwaltungsprogramm"
  Name: "{group}\PhoA Hilfe (Russich)";       Languages: de; Filename: "{app}\phoa-rus.chm";   Components: help\ru; Comment: "PhoA Hilfe (Russisch)"
  Name: "{group}\PhoA help (English)";        Languages: de; Filename: "{app}\phoa-eng.chm";   Components: help\en; Comment: "PhoA Hilfe (Englisch)"
  Name: "{group}\Beispiel Fotoalbum";         Languages: de; Filename: "{app}\phoa.exe";       Components: sample;  Comment: "PhoA Beispiel Fotoalbum"; Parameters: """{app}\Sample album\sample.phoa"""; IconFilename: "{app}\phoa.exe"; IconIndex: 1
  Name: "{group}\PhoA Homepage";              Languages: de; Filename: "{app}\phoa.url";       Components: main;    Comment: "phoa.narod.ru"
  Name: "{group}\PhoA deinstallieren";        Languages: de; Filename: "{uninstallexe}";       Components: main;    Comment: "PhoA und alle zugehцrigen Komponenten komplett entfernen"
  Name: "{commondesktop}\PhoA";               Languages: de; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Bildverwaltungsprogramm"; Tasks: desktopicon\common
  Name: "{userdesktop}\PhoA";                 Languages: de; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Bildverwaltungsprogramm"; Tasks: desktopicon\user
  Name: "{code:QuickLaunch|{pf}}\PhoA";       Languages: de; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Bildverwaltungsprogramm"; Tasks: quicklaunchicon
;Brazilian Portuguese entries
  Name: "{group}\PhoA";                       Languages: br; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Programa para organizaзгo de imagens"
  Name: "{group}\Ajuda PhoA (Russo)";         Languages: br; Filename: "{app}\phoa-rus.chm";   Components: help\ru; Comment: "Ajuda PhoA (Russo)"
  Name: "{group}\PhoA help (English)";        Languages: br; Filename: "{app}\phoa-eng.chm";   Components: help\en; Comment: "PhoA help (English)"
  Name: "{group}\Бlbum exemplo";              Languages: br; Filename: "{app}\phoa.exe";       Components: sample;  Comment: "Бlbum exemplo"; Parameters: """{app}\Sample album\sample.phoa"""; IconFilename: "{app}\phoa.exe"; IconIndex: 1
  Name: "{group}\PhoA home page";             Languages: br; Filename: "{app}\phoa.url";       Components: main;    Comment: "phoa.narod.ru"
  Name: "{group}\Desinstalar PhoA";           Languages: br; Filename: "{uninstallexe}";       Components: main;    Comment: "Remover PhoA e todos os seus componenetes"
  Name: "{commondesktop}\PhoA";               Languages: br; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Programa para organizaзгo de imagens"; Tasks: desktopicon\common
  Name: "{userdesktop}\PhoA";                 Languages: br; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Programa para organizaзгo de imagens"; Tasks: desktopicon\user
  Name: "{code:QuickLaunch|{pf}}\PhoA";       Languages: br; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Programa para organizaзгo de imagens"; Tasks: quicklaunchicon
;Ukrainian entries
  Name: "{group}\PhoA";                       Languages: ua; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Програма для каталогізаціі зображень і складання фотоальбомів"
  Name: "{group}\Довідка по PhoA (Російска)"; Languages: ua; Filename: "{app}\phoa-rus.chm";   Components: help\ru; Comment: "Довідка по програмному продукту  PhoA"
  Name: "{group}\Довідка по PhoA (English)";  Languages: ua; Filename: "{app}\phoa-eng.chm";   Components: help\en; Comment: "PhoA help"
  Name: "{group}\Приклад фотоальбому";        Languages: ua; Filename: "{app}\phoa.exe";       Components: sample;  Comment: "Зразок фотоальбому PhoA"; Parameters: """{app}\Sample album\sample.phoa"""; IconFilename: "{app}\phoa.exe"; IconIndex: 1
  Name: "{group}\Сайт PhoA";                  Languages: ua; Filename: "{app}\phoa.url";       Components: main;    Comment: "phoa.narod.ru"
  Name: "{group}\Видалити PhoA";              Languages: ua; Filename: "{uninstallexe}";       Components: main;    Comment: "Цілком видалити PhoA і усі компоненти програми"
  Name: "{commondesktop}\PhoA";               Languages: ua; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Програма для каталогізаціі зображень і складання фотоальбомів"; Tasks: desktopicon\common
  Name: "{userdesktop}\PhoA";                 Languages: ua; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Програма для каталогізаціі зображень і складання фотоальбомів"; Tasks: desktopicon\user
  Name: "{code:QuickLaunch|}\PhoA";           Languages: ua; Filename: "{app}\phoa.exe";       Components: main;    Comment: "Програма для каталогізаціі зображень і складання фотоальбомів"; Tasks: quicklaunchicon


[Registry]
  Root: HKCR; Subkey: ".phoa";                                             ValueType: string; ValueData: "phoa.photoalbum";           Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum";                    Languages: en; ValueType: string; ValueData: "PhoA Photo Album";          Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum";                    Languages: ru; ValueType: string; ValueData: "Фотоальбом PhoA";           Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum";                    Languages: de; ValueType: string; ValueData: "PhoA Fotoalbum";            Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum";                    Languages: br; ValueType: string; ValueData: "PhoA Бlbum";                Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum";                    Languages: ua; ValueType: string; ValueData: "Фотоальбом PhoA";           Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum\shell\open\command";                ValueType: string; ValueData: """{app}\phoa.exe"" ""%1"""; Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
  Root: HKCR; Subkey: "phoa.photoalbum\DefaultIcon";                       ValueType: string; ValueData: """{app}\phoa.exe"",1";      Flags: uninsdeletevalue uninsdeletekeyifempty; Tasks: associate
;Default language
  Root: HKCU; Subkey: "Software\DaleTech\PhoA\Preferences"; Languages: en; ValueName: "@ISettingID_Gen_Language"; ValueType: string; ValueData: "1033"; Flags: uninsdeletevalue uninsdeletekeyifempty
  Root: HKCU; Subkey: "Software\DaleTech\PhoA\Preferences"; Languages: ru; ValueName: "@ISettingID_Gen_Language"; ValueType: string; ValueData: "1049"; Flags: uninsdeletevalue uninsdeletekeyifempty
  Root: HKCU; Subkey: "Software\DaleTech\PhoA\Preferences"; Languages: de; ValueName: "@ISettingID_Gen_Language"; ValueType: string; ValueData: "1031"; Flags: uninsdeletevalue uninsdeletekeyifempty
  Root: HKCU; Subkey: "Software\DaleTech\PhoA\Preferences"; Languages: br; ValueName: "@ISettingID_Gen_Language"; ValueType: string; ValueData: "1046"; Flags: uninsdeletevalue uninsdeletekeyifempty
  Root: HKCU; Subkey: "Software\DaleTech\PhoA\Preferences"; Languages: ua; ValueName: "@ISettingID_Gen_Language"; ValueType: string; ValueData: "1058"; Flags: uninsdeletevalue uninsdeletekeyifempty


[Run]
  Filename: "{app}\phoa.exe"; Languages: en; Parameters: {code:PhoaStartupParams|}; Description: "Run PhoA";       Flags: nowait postinstall skipifsilent
  Filename: "{app}\phoa.exe"; Languages: ru; Parameters: {code:PhoaStartupParams|}; Description: "Запустить PhoA"; Flags: nowait postinstall skipifsilent
  Filename: "{app}\phoa.exe"; Languages: de; Parameters: {code:PhoaStartupParams|}; Description: "PhoA starten";   Flags: nowait postinstall skipifsilent
  Filename: "{app}\phoa.exe"; Languages: br; Parameters: {code:PhoaStartupParams|}; Description: "Executar PhoA";  Flags: nowait postinstall skipifsilent
  Filename: "{app}\phoa.exe"; Languages: ua; Parameters: {code:PhoaStartupParams|}; Description: "Запустити PhoA"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
  Type: files;      Name: "{app}\phoa.url"
  Type: dirifempty; Name: "{app}"

[Messages]
BeveledLabel=http://phoa.narod.ru

[Code]

  function QuickLaunch(Default: String): String;
  begin
    Result := ExpandConstant('{userappdata}')+'\Microsoft\Internet Explorer\Quick Launch';
  end;
  
  function PhoaStartupParams(Default: String): String;
  begin
    if ShouldProcessEntry('sample', '')=srYes then Result := '"Sample album\sample.phoa"' else Result := '';
  end;
