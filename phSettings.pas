//**********************************************************************************************************************
//  $Id: phSettings.pas,v 1.4 2004-04-22 17:54:00 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phSettings;

interface
uses SysUtils, Windows, Classes, Graphics, Controls, Registry, IniFiles, VirtualTrees, TB2Dock, TBX, GR32, ConsVars;

type
   // Exception настроек
  EPhoaSettingError = class(EPhoaError);

   //===================================================================================================================
   // TPhoaSetting - базовый пункт настройки
   //===================================================================================================================

  PPhoaSetting = ^TPhoaSetting;
  TPhoaSetting = class(TObject)
  private
     // Список дочерних пунктов
    FChildren: TList;
     // Prop storage
    FOwner: TPhoaSetting;
    FName: String;
    FID: Integer;
     // Добавление / удаление пункта из списка дочерних пунктов
    procedure AddSetting(Item: TPhoaSetting);
    procedure RemoveSetting(Item: TPhoaSetting);
    procedure DeleteSetting(Index: Integer);
     // Очищает список дочерних пунктов
    procedure ClearSettings;
     // Ищет пункт по ID среди себя и своих детей. Если не найден, возвращает nil
    function  FindID(iID: Integer): TPhoaSetting;
     // Prop handlers
    function  GetChildCount: Integer;
    function  GetChildren(Index: Integer): TPhoaSetting;
    function  GetSettings(iID: Integer): TPhoaSetting;
  protected
     // "Простой" конструктор, не инициализирующий свойств, кроме Owner (используется в "нормальных" конструкторах и в
     //   Assign)
    constructor CreateNew(AOwner: TPhoaSetting); virtual;
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const sName: String);
    destructor Destroy; override;
     // Загрузка/сохранение в реестре значений с ID<>0
    procedure RegLoad(RegIniFile: TRegIniFile);
    procedure RegSave(RegIniFile: TRegIniFile);
     // Загрузка/сохранение в Ini-файле значений с ID<>0
    procedure IniLoad(IniFile: TIniFile);
    procedure IniSave(IniFile: TIniFile);
     // Копирует все установки (включая структуру дочерних узлов) с узла Source
    procedure Assign(Source: TPhoaSetting); virtual;
     // Props
     // -- Количество дочерних пунктов
    property ChildCount: Integer read GetChildCount;
     // -- Дочерние пункты по индексу
    property Children[Index: Integer]: TPhoaSetting read GetChildren; default;
     // -- ID пункта
    property ID: Integer read FID;
     // -- Наименование пункта. Если начинается с '@', то это имя константы из диалога Settings, если с '#' - то из fMain
    property Name: String read FName;
     // -- Пункт-владелец данного пункта
    property Owner: TPhoaSetting read FOwner;
     // -- Пункты по ID
    property Settings[iID: Integer]: TPhoaSetting read GetSettings;
  end;

  TPhoaSettingClass = class of TPhoaSetting;

   //===================================================================================================================
   // Пункт корневого пункта настроек, представляющий собой страницу
   //===================================================================================================================

  TPhoaPageSetting = class(TPhoaSetting)
  private
     // Prop storage
    FHelpContext: THelpContext;
    FImageIndex: Integer;
  protected
    constructor CreateNew(AOwner: TPhoaSetting); override;
    function  GetEditorClass: TWinControlClass; virtual; abstract;
  public
    constructor Create(AOwner: TPhoaSetting; iID, iImageIndex: Integer; const sName: String; AHelpContext: THelpContext);
    procedure Assign(Source: TPhoaSetting); override;
     // Props
     // -- Класс компонента-редактора настройки
    property EditorClass: TWinControlClass read GetEditorClass;
     // -- HelpContext ID пункта
    property HelpContext: THelpContext read FHelpContext;
     // -- ImageIndex пункта
    property ImageIndex: Integer read FImageIndex;
  end;

   // Событие декодирования текста пункта
  TPhoaSettingDecodeTextEvent = procedure(const sText: String; out sDecoded: String) of object;

   // Интерфейс редактора настроек
  IPhoaSettingEditor = interface(IInterface)
    ['{32018724-F48C-4EC4-B86A-81C5C5A1F75E}']
     // Инициализирует и встраивает редактор
    procedure InitAndEmbed(ParentCtl: TWinControl; AOnChange: TNotifyEvent; AOnDecodeText: TPhoaSettingDecodeTextEvent);
     // Prop handlers
    function  GetRootSetting: TPhoaPageSetting;
    procedure SetRootSetting(Value: TPhoaPageSetting);
     // Props
     // -- Корневой узел настройки - все его дети должны редактироваться редактором
    property RootSetting: TPhoaPageSetting read GetRootSetting write SetRootSetting;
  end;

   // Применяет пользовательские настройки к TVirtualStringTree
  procedure ApplyTreeSettings(Tree: TVirtualStringTree);
   // Применяет пользовательские настройки к докам/панелям инструментов
  procedure ApplyToolbarSettings(Dock: TTBXDock);

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses TypInfo, phUtils;

const
   // Сообщения об ошибках
  SSettingsErrMsg_InvalidSettingID   = 'Invalid setting ID (%d)';

   // Вызывает EPhoaSettingError
  procedure PhoaSettingError(const sMsg: String; const aParams: Array of const);

     function RetAddr: Pointer;
     asm
       mov eax, [ebp+4]
     end;

  begin
    raise EPhoaSettingError.CreateFmt(sMsg, aParams) at RetAddr;
  end;

   //===================================================================================================================
   // TPhoaSetting
   //===================================================================================================================

  procedure TPhoaSetting.AddSetting(Item: TPhoaSetting);
  begin
    if FChildren=nil then FChildren := TList.Create;
    FChildren.Add(Item);
  end;

  procedure TPhoaSetting.Assign(Source: TPhoaSetting);
  var
    i: Integer;
    SrcChild: TPhoaSetting;
  begin
     // Стираем детей
    ClearSettings;
     // Копируем настройки
    FID   := Source.FID;
    FName := Source.FName;
     // Повторяем то же для детей
    for i := 0 to Source.ChildCount-1 do begin
      SrcChild := Source.Children[i];
       // Вызываем виртуальный конструктор CreateNew того же класса, что и у SrcChild
      TPhoaSettingClass(SrcChild.ClassType).CreateNew(Self).Assign(SrcChild);
    end;
  end;

  procedure TPhoaSetting.ClearSettings;
  begin
    if FChildren<>nil then begin
      while FChildren.Count>0 do DeleteSetting(FChildren.Count-1);
      FreeAndNil(FChildren);
    end;
  end;

  constructor TPhoaSetting.Create(AOwner: TPhoaSetting; iID: Integer; const sName: String);
  begin
    CreateNew(AOwner);
    FID   := iID;
    FName := sName;
  end;

  constructor TPhoaSetting.CreateNew(AOwner: TPhoaSetting);
  begin
    inherited Create;
    FOwner := AOwner;
    if FOwner<>nil then FOwner.AddSetting(Self);
  end;

  procedure TPhoaSetting.DeleteSetting(Index: Integer);
  begin
    TPhoaSetting(FChildren[Index]).Free;
  end;

  destructor TPhoaSetting.Destroy;
  begin
     // Стираем и уничтожаем список дочерних пунктов
    ClearSettings;
    if FOwner<>nil then FOwner.RemoveSetting(Self);
    inherited Destroy;
  end;

  function TPhoaSetting.FindID(iID: Integer): TPhoaSetting;
  var i: Integer;
  begin
    if iID=FID then begin
      Result := Self;
      Exit;
    end else if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do begin
        Result := GetChildren(i).FindID(iID);
        if Result<>nil then Exit;
      end;
    Result := nil;
  end;

  function TPhoaSetting.GetChildCount: Integer;
  begin
    if FChildren=nil then Result := 0 else Result := FChildren.Count;
  end;

  function TPhoaSetting.GetChildren(Index: Integer): TPhoaSetting;
  begin
    Result := TPhoaSetting(FChildren[Index]);
  end;

  function TPhoaSetting.GetSettings(iID: Integer): TPhoaSetting;
  begin
    Result := FindID(iID);
    if Result=nil then PhoaSettingError(SSettingsErrMsg_InvalidSettingID, [iID]);
  end;

  procedure TPhoaSetting.IniLoad(IniFile: TIniFile);
  var
    s: String;
    i: Integer;
  begin
     // Получаем своё значение
    if FID<>0 then begin
      s := IniFile.ReadString(SRegPrefs, FName, GetValStr);
      if FDatatype=sdtFont then SetValueStr(s) else FData := StrToIntDef(s, FData);
    end;
     // Повторяем то же для детей
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).IniLoad(IniFile);
  end;

  procedure TPhoaSetting.IniSave(IniFile: TIniFile);
  var i: Integer;
  begin
     // Сохраняем своё значение
    if FID<>0 then IniFile.WriteString(SRegPrefs, FName, GetValStr);
     // Повторяем то же для детей
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).IniSave(IniFile);
  end;

  procedure TPhoaSetting.RegLoad(RegIniFile: TRegIniFile);
  var
    s: String;
    i: Integer;
  begin
     // Получаем своё значение
    if FID<>0 then begin
      s := RegIniFile.ReadString(SRegPrefs, FName, GetValStr);
      if FDatatype=sdtFont then SetValueStr(s) else FData := StrToIntDef(s, FData);
    end;
     // Повторяем то же для детей
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).RegLoad(RegIniFile);
  end;

  procedure TPhoaSetting.RegSave(RegIniFile: TRegIniFile);
  var i: Integer;
  begin
     // Сохраняем своё значение
    if FID<>0 then RegIniFile.WriteString(SRegPrefs, FName, GetValStr);
     // Повторяем то же для детей
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).RegSave(RegIniFile);
  end;

  procedure TPhoaSetting.RemoveSetting(Item: TPhoaSetting);
  begin
    FChildren.Remove(Item);
  end;

   //===================================================================================================================
   // TPhoaPageSetting 
   //===================================================================================================================

  procedure TPhoaPageSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaValPageSetting then begin
      FImageIndex  := TPhoaValPageSetting(Source).FImageIndex;
      FHelpContext := TPhoaValPageSetting(Source).FHelpContext;
    end;
  end;

  constructor TPhoaPageSetting.Create(AOwner: TPhoaSetting; iID, iImageIndex: Integer; const sName: String; AHelpContext: THelpContext);
  begin
    inherited Create(AOwner, iID, sName);
    FImageIndex  := iImageIndex;
    FHelpContext := AHelpContext;
  end;

  constructor TPhoaPageSetting.CreateNew(AOwner: TPhoaSetting);
  begin
    inherited CreateNew(AOwner);
    FImageIndex := -1;
  end;

end.
