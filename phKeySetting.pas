//**********************************************************************************************************************
//  $Id: phKeySetting.pas,v 1.3 2004-09-11 17:52:36 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phKeySetting;

interface
(*
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Registry, IniFiles, VirtualTrees, ConsVars, phSettings, phObj;

type
   //===================================================================================================================
   // TPhoaKeySetting - настройка, представляющая собой запись сочетаний клавиш для вызова команды
   //===================================================================================================================

  PPhoaKeySetting = ^TPhoaKeySetting;
  TPhoaKeySetting = class(TPhoaSetting)
  private
     // Состояние изменённости данных
    FModified: Boolean;
     // Prop storage
    FHint: String;
    FKind: TPhoaToolKind;
    FMasks: String;
    FRunCommand: String;
    FRunFolder: String;
    FRunParameters: String;
    FRunShowCommand: Integer;
    FUsages: TPhoaToolUsages;
     // Возвращает имя секции для сохранения/загрузки настроек
    function GetStoreSection: String;
     // Prop handlers
    procedure SetHint(const Value: String);
    procedure SetKind(Value: TPhoaToolKind);
    procedure SetMasks(const Value: String);
    procedure SetName(const Value: String);
    procedure SetRunCommand(const Value: String);
    procedure SetRunFolder(const Value: String);
    procedure SetRunParameters(const Value: String);
    procedure SetRunShowCommand(Value: Integer);
    procedure SetUsages(Value: TPhoaToolUsages);
  protected
    constructor CreateNew(AOwner: TPhoaSetting); override;
    function  GetModified: Boolean; override;
    procedure SetModified(Value: Boolean); override;
  public
    constructor Create(AOwner: TPhoaSetting; const sName, sHint, sRunCommand, sRunFolder, sRunParameters, sMasks: String;
                       AKind: TPhoaToolKind; iRunShowCommand: Integer; AUsages: TPhoaToolUsages);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPhoaSetting); override;
    procedure RegLoad(RegIniFile: TRegIniFile); override;
    procedure RegSave(RegIniFile: TRegIniFile); override;
    procedure IniLoad(IniFile: TIniFile); override;
    procedure IniSave(IniFile: TIniFile); override;
     // Возвращает True, если инструмент подходит всем файлам из PicLinks
    function  MatchesFiles(PicLinks: TPhoaPicLinks): Boolean;
     // Выполняет инструмент для заданных изображений
    procedure Execute(PicLinks: TPhoaPicLinks);
     // Props
     // -- Подсказка. Закодирована по правилам ConstValEx()
    property Hint: String read FHint write SetHint;
     // -- Вид инструмента
    property Kind: TPhoaToolKind read FKind write SetKind;
     // -- Наименование, доступное для записи. Закодировано по правилам ConstValEx()
    property Name read FName write SetName;
     // -- Команда запуска (для Kind=ptkCustom)
    property RunCommand: String read FRunCommand write SetRunCommand;
     // -- Каталог запуска (для Kind=ptkCustom)
    property RunFolder: String read FRunFolder write SetRunFolder;
     // -- Параметры запуска (для Kind=ptkCustom)
    property RunParameters: String read FRunParameters write SetRunParameters;
     // -- Команда показа при запуске (состояние окна SW_xxx)
    property RunShowCommand: Integer read FRunShowCommand write SetRunShowCommand;
     // -- Маски файлов, для которых применим инструмент
    property Masks: String read FMasks write SetMasks;
     // -- Где отображается пункт инструмента
    property Usages: TPhoaToolUsages read FUsages write SetUsages;
  end;

   //===================================================================================================================
   // Класс пункта-страницы с инструментами
   //===================================================================================================================

  TPhoaToolPageSetting = class(TPhoaPageSetting)
  private
     // Состояние изменённости данных
    FModified: Boolean;
  protected
    function  GetEditorClass: TWinControlClass; override;
    function  GetModified: Boolean; override;
    procedure SetModified(Value: Boolean); override;
  public
    procedure RegLoad(RegIniFile: TRegIniFile); override;
    procedure RegSave(RegIniFile: TRegIniFile); override;
    procedure IniLoad(IniFile: TIniFile); override;
    procedure IniSave(IniFile: TIniFile); override;
  end;
*)
implementation

end.
