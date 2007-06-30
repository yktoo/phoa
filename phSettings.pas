//**********************************************************************************************************************
//  $Id: phSettings.pas,v 1.19 2007-06-30 10:36:20 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phSettings;

interface
uses
  SysUtils, Windows, Classes, Graphics, Controls, IniFiles,
  TntSysUtils, TntClasses, 
  phIntf, phObj, ConsVars;

type
   // Exception настроек
  EPhoaSettingError = class(EPhoaWideException);

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
    FID: Integer;
     // Добавление / удаление пункта из списка дочерних пунктов
    procedure AddSetting(Item: TPhoaSetting);
    procedure RemoveSetting(Item: TPhoaSetting);
    procedure DeleteSetting(Index: Integer);
     // Ищет пункт по ID среди себя и своих детей. Если не найден, возвращает nil
    function  FindID(iID: Integer): TPhoaSetting;
     // Prop handlers
    function  GetChildCount: Integer;
    function  GetChildren(Index: Integer): TPhoaSetting;
    function  GetSettings(iID: Integer): TPhoaSetting;
    function  GetIndex: Integer;
    procedure SetIndex(Value: Integer);
  protected
     // Prop storage
    FName: WideString;
     // "Простой" конструктор, не инициализирующий свойств, кроме Owner (используется в "нормальных" конструкторах и в
     //   Assign)
    constructor CreateNew(AOwner: TPhoaSetting); virtual;
     // Prop handlers
    function  GetModified: Boolean; virtual;
    procedure SetModified(Value: Boolean); virtual;
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer; const wsName: WideString);
    destructor Destroy; override;
     // Стирает все дочерние пункты
    procedure ClearChildren;
     // Загрузка/сохранение в реестре значений с ID<>0. В базовом классе не делают ничего, кроме каскадных вызовов
     //   методов дочерних настроек
    procedure RegLoad(RegIniFile: TPhoaRegIniFile); virtual;
    procedure RegSave(RegIniFile: TPhoaRegIniFile); virtual;
     // Загрузка/сохранение в Ini-файле значений с ID<>0. В базовом классе не делают ничего, кроме каскадных вызовов
     //   методов дочерних настроек
    procedure IniLoad(IniFile: TIniFile); virtual;
    procedure IniSave(IniFile: TIniFile); virtual;
     // Копирует все установки (включая структуру дочерних узлов) с узла Source
    procedure Assign(Source: TPhoaSetting); virtual;
     // Props
     // -- Количество дочерних пунктов
    property ChildCount: Integer read GetChildCount;
     // -- Дочерние пункты по индексу
    property Children[Index: Integer]: TPhoaSetting read GetChildren; default;
     // -- ID пункта
    property ID: Integer read FID;
     // -- Индекс настройки внутри родителя
    property Index: Integer read GetIndex write SetIndex;
     // -- True, если значение настройки или любой из дочерних настроек модифицировано
    property Modified: Boolean read GetModified write SetModified;
     // -- Наименование пункта, закодированное по правилам ConstValEx()
    property Name: WideString read FName;
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
     // Prop handlers
    function  GetEditorClass: TWinControlClass; virtual; abstract;
    function  GetVisible: Boolean; virtual;
  public
    constructor Create(AOwner: TPhoaSetting; iID, iImageIndex: Integer; const sName: AnsiString; AHelpContext: THelpContext);
    procedure Assign(Source: TPhoaSetting); override;
     // Props
     // -- Класс компонента-редактора настройки
    property EditorClass: TWinControlClass read GetEditorClass;
     // -- HelpContext ID пункта
    property HelpContext: THelpContext read FHelpContext;
     // -- ImageIndex пункта
    property ImageIndex: Integer read FImageIndex;
     // -- True, если страница содержит видимые настройки; иначе False
    property Visible: Boolean read GetVisible;
  end;

   //===================================================================================================================
   // Невидимая страница настроек
   //===================================================================================================================

  TPhoaInvisiblePageSetting = class(TPhoaPageSetting)
  protected
    function  GetEditorClass: TWinControlClass; override;
    function  GetVisible: Boolean; override;
  public
    constructor Create(AOwner: TPhoaSetting; iID: Integer);
  end;

   //===================================================================================================================
   // Интерфейс редактора настроек
   //===================================================================================================================

  IPhoaSettingEditor = interface(IInterface)
    ['{32018724-F48C-4EC4-B86A-81C5C5A1F75E}']
     // Инициализирует и встраивает редактор
    procedure InitAndEmbed(ParentCtl: TWinControl; AOnChange: TNotifyEvent);
     // Prop handlers
    function  GetRootSetting: TPhoaPageSetting;
    procedure SetRootSetting(Value: TPhoaPageSetting);
     // Props
     // -- Корневой узел настройки - все его дети должны редактироваться редактором
    property RootSetting: TPhoaPageSetting read GetRootSetting write SetRootSetting;
  end;

var
   // Корневой пункт установки
  RootSetting: TPhoaSetting;

   // Функции/процедуры для доступа к значениям настроек
  function  SettingValueInt (iID: Integer): Integer;
  function  SettingValueBool(iID: Integer): Boolean;
  function  SettingValueWStr(iID: Integer): WideString;
  function  SettingValueRect(iID: Integer): TRect;
  procedure SetSettingValueInt (iID, iValue: Integer);
  procedure SetSettingValueBool(iID: Integer; bValue: Boolean);
  procedure SetSettingValueWStr(iID: Integer; const wsValue: WideString);
  procedure SetSettingValueRect(iID: Integer; const rValue: TRect);

   // Процедуры сохранения и загрузки настроек (дочерних узлов RootSetting) из реестра / phoa.ini (если последнее включено)
  procedure SaveAllSettings;
  procedure LoadAllSettings;
   // Процедуры сохранения и загрузки настроек из реестра ini-файла
  procedure IniSaveSettings(const wsIniFileName: WideString);
  procedure IniLoadSettings(const wsIniFileName: WideString);

implementation /////////////////////////////////////////////////////////////////////////////////////////////////////////
uses TypInfo, phUtils, phValSetting;

const
   // Сообщения об ошибках
  SSettingsErrMsg_InvalidSettingID   = 'Invalid setting ID (%d)';
  SSettingsErrMsg_InvalidSettingType = 'Cannot access setting (ID=%d) as type %s';

   // Вызывает EPhoaSettingError
  procedure PhoaSettingError(const wsMessage: WideString; const aParams: Array of const);

     function RetAddr: Pointer;
     asm
       mov eax, [ebp+4]
     end;

  begin
    raise EPhoaSettingError.CreateFmt(wsMessage, aParams) at RetAddr;
  end;

  function SettingValueInt(iID: Integer): Integer;
  var Setting: TPhoaSetting;
  begin
    Setting := RootSetting.Settings[iID];
    if not (Setting is TPhoaIntSetting) then PhoaSettingError(SSettingsErrMsg_InvalidSettingType, ['Integer']);
    Result := TPhoaIntSetting(Setting).Value;
  end;

  function SettingValueBool(iID: Integer): Boolean;
  var Setting: TPhoaSetting;
  begin
    Setting := RootSetting.Settings[iID];
    if not (Setting is TPhoaBoolSetting) then PhoaSettingError(SSettingsErrMsg_InvalidSettingType, ['Bool']);
    Result := TPhoaBoolSetting(Setting).Value;
  end;

  function SettingValueWStr(iID: Integer): WideString;
  var Setting: TPhoaSetting;
  begin
    Setting := RootSetting.Settings[iID];
    if not (Setting is TPhoaWideStrSetting) then PhoaSettingError(SSettingsErrMsg_InvalidSettingType, ['WideString']);
    Result := TPhoaWideStrSetting(Setting).Value;
  end;

  function SettingValueRect(iID: Integer): TRect;
  var Setting: TPhoaSetting;
  begin
    Setting := RootSetting.Settings[iID];
    if not (Setting is TPhoaRectSetting) then PhoaSettingError(SSettingsErrMsg_InvalidSettingType, ['Rect']);
    Result := TPhoaRectSetting(Setting).Value;
  end;

  procedure SetSettingValueInt(iID, iValue: Integer);
  var Setting: TPhoaSetting;
  begin
    Setting := RootSetting.Settings[iID];
    if not (Setting is TPhoaIntSetting) then PhoaSettingError(SSettingsErrMsg_InvalidSettingType, ['Integer']);
    TPhoaIntSetting(Setting).Value := iValue;
  end;

  procedure SetSettingValueBool(iID: Integer; bValue: Boolean);
  var Setting: TPhoaSetting;
  begin
    Setting := RootSetting.Settings[iID];
    if not (Setting is TPhoaBoolSetting) then PhoaSettingError(SSettingsErrMsg_InvalidSettingType, ['Bool']);
    TPhoaBoolSetting(Setting).Value := bValue;
  end;

  procedure SetSettingValueWStr(iID: Integer; const wsValue: WideString);
  var Setting: TPhoaSetting;
  begin
    Setting := RootSetting.Settings[iID];
    if not (Setting is TPhoaWideStrSetting) then PhoaSettingError(SSettingsErrMsg_InvalidSettingType, ['WideString']);
    TPhoaWideStrSetting(Setting).Value := wsValue;
  end;

  procedure SetSettingValueRect(iID: Integer; const rValue: TRect);
  var Setting: TPhoaSetting;
  begin
    Setting := RootSetting.Settings[iID];
    if not (Setting is TPhoaRectSetting) then PhoaSettingError(SSettingsErrMsg_InvalidSettingType, ['Rect']);
    TPhoaRectSetting(Setting).Value := rValue;
  end;

  procedure SaveAllSettings;
  var rif: TPhoaRegIniFile;
  begin
     // Сохраняем настройки в реестре
    rif := TPhoaRegIniFile.Create(SRegRoot);
    try
      RootSetting.RegSave(rif);
    finally
      rif.Free;
    end;
  end;

  procedure LoadAllSettings;
  var
    rif: TPhoaRegIniFile;
    wsAutoLoadIniFile: WideString;
  begin
     // Загружаем настройки из реестра
    rif := TPhoaRegIniFile.Create(SRegRoot);
    try
      RootSetting.RegLoad(rif);
    finally
      rif.Free;
    end;
     // Если нужно и присутствует ini-файл, подгружаем настройки из него
    if SettingValueBool(ISettingID_Gen_LookupPhoaIni) then begin
      wsAutoLoadIniFile := wsApplicationPath+SDefaultIniFileName;
      if WideFileExists(wsAutoLoadIniFile) then IniLoadSettings(wsAutoLoadIniFile);
    end;
  end;

  procedure IniSaveSettings(const wsIniFileName: WideString);
  var fi: TIniFile;
  begin
    fi := TIniFile.Create(wsIniFileName); {!!! Not Unicode-enabled solution }
    try
      RootSetting.IniSave(fi);
    finally
      fi.Free;
    end;
  end;

  procedure IniLoadSettings(const wsIniFileName: WideString);
  var fi: TIniFile;
  begin
    fi := TIniFile.Create(wsIniFileName); {!!! Not Unicode-enabled solution }
    try
      RootSetting.IniLoad(fi);
    finally
      fi.Free;
    end;
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
    ClearChildren;
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

  procedure TPhoaSetting.ClearChildren;
  begin
    if FChildren<>nil then begin
      while FChildren.Count>0 do DeleteSetting(FChildren.Count-1);
      FreeAndNil(FChildren);
    end;
  end;

  constructor TPhoaSetting.Create(AOwner: TPhoaSetting; iID: Integer; const wsName: WideString);
  begin
    CreateNew(AOwner);
    FID   := iID;
    FName := wsName;
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
    ClearChildren;
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

  function TPhoaSetting.GetIndex: Integer;
  begin
    if FOwner=nil then Result := 0 else Result := FOwner.FChildren.IndexOf(Self);
  end;

  function TPhoaSetting.GetModified: Boolean;
  var i: Integer;
  begin
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do
        if GetChildren(i).Modified then begin
          Result := True;
          Exit;
        end;
    Result := False;
  end;

  function TPhoaSetting.GetSettings(iID: Integer): TPhoaSetting;
  begin
    Result := FindID(iID);
    if Result=nil then PhoaSettingError(SSettingsErrMsg_InvalidSettingID, [iID]);
  end;

  procedure TPhoaSetting.IniLoad(IniFile: TIniFile);
  var i: Integer;
  begin
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).IniLoad(IniFile);
  end;

  procedure TPhoaSetting.IniSave(IniFile: TIniFile);
  var i: Integer;
  begin
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).IniSave(IniFile);
  end;

  procedure TPhoaSetting.RegLoad(RegIniFile: TPhoaRegIniFile);
  var i: Integer;
  begin
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).RegLoad(RegIniFile);
  end;

  procedure TPhoaSetting.RegSave(RegIniFile: TPhoaRegIniFile);
  var i: Integer;
  begin
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).RegSave(RegIniFile);
  end;

  procedure TPhoaSetting.RemoveSetting(Item: TPhoaSetting);
  begin
    FChildren.Remove(Item);
  end;

  procedure TPhoaSetting.SetIndex(Value: Integer);
  begin
    if FOwner<>nil then FOwner.FChildren.Move(GetIndex, Value);
  end;

  procedure TPhoaSetting.SetModified(Value: Boolean);
  var i: Integer;
  begin
    if FChildren<>nil then
      for i := 0 to FChildren.Count-1 do GetChildren(i).Modified := Value;
  end;

   //===================================================================================================================
   // TPhoaPageSetting
   //===================================================================================================================

  procedure TPhoaPageSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaPageSetting then begin
      FImageIndex  := TPhoaPageSetting(Source).FImageIndex;
      FHelpContext := TPhoaPageSetting(Source).FHelpContext;
    end;
  end;

  constructor TPhoaPageSetting.Create(AOwner: TPhoaSetting; iID, iImageIndex: Integer; const sName: AnsiString; AHelpContext: THelpContext);
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

  function TPhoaPageSetting.GetVisible: Boolean;
  begin
    Result := True;
  end;

   //===================================================================================================================
   // TPhoaInvisiblePageSetting
   //===================================================================================================================

  constructor TPhoaInvisiblePageSetting.Create(AOwner: TPhoaSetting; iID: Integer);
  begin
    inherited Create(AOwner, iID, -1, '', 0);
  end;

  function TPhoaInvisiblePageSetting.GetEditorClass: TWinControlClass;
  begin
    Result := nil;
  end;

  function TPhoaInvisiblePageSetting.GetVisible: Boolean;
  begin
    Result := False;
  end;

end.
