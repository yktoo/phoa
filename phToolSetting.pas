//**********************************************************************************************************************
//  $Id: phToolSetting.pas,v 1.2 2004-04-25 16:28:31 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phToolSetting;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Registry, IniFiles, VirtualTrees,
  ConsVars, phSettings;

type
   // Вид инструмента
  TPhoaToolKind = (
    ptkSeparator,  // Пункт-разделитель
    ptkDefault,    // Инструмент, выполняющий действие по умолчанию
    ptkOpen,       // Инструмент, выполняющий открытие файла изображения
    ptkEdit,       // Инструмент, выполняющий редактирование файла изображения
    ptkPrint,      // Инструмент, выполняющий печать файла изображения
    ptkCustom);    // Инструмент, задаваемый командной строкой
const
  IPhoaToolKindPrefixLen = 3; // Длина префикса элементов TPhoaToolKind

type
   // Где отображается команда инструмента
  TPhoaToolUsage = (
    ptuToolsMenu,          // В меню "Сервис"
    ptuGroupPopupMenu,     // В popup-меню дерева групп
    ptuThViewerPopupMenu); // В popup-меню окна эскизов
  TPhoaToolUsages = set of TPhoaToolUsage;

   //===================================================================================================================
   // TPhoaToolSetting - настройка, представляющая собой запись инструмента
   //===================================================================================================================

  PPhoaToolSetting = ^TPhoaToolSetting;
  TPhoaToolSetting = class(TPhoaSetting)
  private
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
  protected
    constructor CreateNew(AOwner: TPhoaSetting); override;
  public
    constructor Create(AOwner: TPhoaSetting; const sName, sHint, sRunCommand, sRunFolder, sRunParameters, sMasks: String;
                       AKind: TPhoaToolKind; iRunShowCommand: Integer; AUsages: TPhoaToolUsages);
    procedure Assign(Source: TPhoaSetting); override;
    procedure RegLoad(RegIniFile: TRegIniFile); override;
    procedure RegSave(RegIniFile: TRegIniFile); override;
    procedure IniLoad(IniFile: TIniFile); override;
    procedure IniSave(IniFile: TIniFile); override;
     // Props
     // -- Подсказка
    property Hint: String read FHint write FHint;
     // -- Вид инструмента
    property Kind: TPhoaToolKind read FKind write FKind;
     // -- Наименование, доступное для записи
    property Name: String read FName write FName;
     // -- Команда запуска (для Kind=ptkCustom)
    property RunCommand: String read FRunCommand write FRunCommand;
     // -- Каталог запуска (для Kind=ptkCustom)
    property RunFolder: String read FRunFolder write FRunFolder;
     // -- Параметры запуска (для Kind=ptkCustom)
    property RunParameters: String read FRunParameters write FRunParameters;
     // -- Команда показа при запуске (состояние окна SW_xxx)
    property RunShowCommand: Integer read FRunShowCommand write FRunShowCommand;
     // -- Маски файлов, для которых применим инструмент
    property Masks: String read FMasks write FMasks;
     // -- Где отображается пункт инструмента
    property Usages: TPhoaToolUsages read FUsages write FUsages;
  end;

   //===================================================================================================================
   // Класс пункта-страницы с инструментами
   //===================================================================================================================

  TPhoaToolPageSetting = class(TPhoaPageSetting)
  protected
    function  GetEditorClass: TWinControlClass; override;
  public
    procedure RegLoad(RegIniFile: TRegIniFile); override;
    procedure RegSave(RegIniFile: TRegIniFile); override;
    procedure IniLoad(IniFile: TIniFile); override;
    procedure IniSave(IniFile: TIniFile); override;
  end;

   // Преобразование TPhoaToolKind <-> String
  function PhoaToolKindToStr(Kind: TPhoaToolKind): String;
  function PhoaToolKindFromStr(const sKind: String; Default: TPhoaToolKind): TPhoaToolKind;
   // Преобразование TPhoaToolUsages <-> String
  function PhoaToolUsagesToStr(Usages: TPhoaToolUsages): String;
  function PhoaToolUsagesFromStr(const sUsages: String): TPhoaToolUsages;

implementation
uses TypInfo, phUtils, TB2Item, TBX, Main, Menus, udToolProps;

  function PhoaToolKindToStr(Kind: TPhoaToolKind): String;
  begin
    Result := Copy(GetEnumName(TypeInfo(TPhoaToolKind), Byte(Kind)), IPhoaToolKindPrefixLen+1, MaxInt);
  end;

  function PhoaToolKindFromStr(const sKind: String; Default: TPhoaToolKind): TPhoaToolKind;
  begin
    for Result := Low(Result) to High(Result) do
      if AnsiSameText(sKind, PhoaToolKindToStr(Result)) then Exit;
    Result := Default;
  end;

  function PhoaToolUsagesToStr(Usages: TPhoaToolUsages): String;
  begin
    Result :=
      iif(ptuToolsMenu in Usages,         'M', '')+
      iif(ptuGroupPopupMenu in Usages,    'G', '')+
      iif(ptuThViewerPopupMenu in Usages, 'V', '');
  end;

  function PhoaToolUsagesFromStr(const sUsages: String): TPhoaToolUsages;
  begin
    Result := [];
    if AnsiStrScan(PChar(sUsages), 'M')<>nil then Include(Result, ptuToolsMenu);
    if AnsiStrScan(PChar(sUsages), 'G')<>nil then Include(Result, ptuGroupPopupMenu);
    if AnsiStrScan(PChar(sUsages), 'V')<>nil then Include(Result, ptuThViewerPopupMenu);
  end;

type
   //===================================================================================================================
   // TPhoaToolSettingEditor - редактор настроек класса TPhoaToolSetting
   //===================================================================================================================

  TPhoaToolSettingEditor = class(TVirtualStringTree, IPhoaSettingEditor)
  private
     // Смещение к данным узла (объекту настройки)
    FDataOffset: Cardinal;
     // Пункты меню
    FItemDelete: TTBXItem;
    FItemEdit: TTBXItem;
    FItemMoveUp: TTBXItem;
    FItemMoveDown: TTBXItem;
     // Prop storage
    FOnSettingChange: TNotifyEvent;
    FOnDecodeText: TPhoaSettingDecodeTextEvent;
    FRootSetting: TPhoaToolPageSetting;
     // Загружает список настроек, относящихся к детям узла FRootSetting
    procedure LoadTree;
     // Вызывает OnSettingChange
    procedure DoSettingChange;
     // Возвращает настройку TPhoaToolSetting, связанную с узлом
    function  GetSetting(Node: PVirtualNode): TPhoaToolSetting;
     // Настраивает Header
    procedure SetupHeader;
     // Создаёт PopupMenu
    procedure CreatePopupMenu;
     // Настраивает доступность пунктов PopupMenu 
    procedure EnablePopupMenuItems;
     // События меню
    procedure DeleteToolClick(Sender: TObject);
    procedure EditToolClick(Sender: TObject);
    procedure MoveToolUpClick(Sender: TObject);
    procedure MoveToolDownClick(Sender: TObject);
     // IPhoaSettingEditor
    procedure InitAndEmbed(ParentCtl: TWinControl; AOnSettingChange: TNotifyEvent; AOnDecodeText: TPhoaSettingDecodeTextEvent);
    function  GetRootSetting: TPhoaPageSetting;
    procedure SetRootSetting(Value: TPhoaPageSetting);
  protected
    procedure DblClick; override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates); override;
    procedure DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

   //===================================================================================================================
   // TPhoaToolSetting
   //===================================================================================================================

  procedure TPhoaToolSetting.Assign(Source: TPhoaSetting);
  begin
    inherited Assign(Source);
    if Source is TPhoaToolSetting then begin
      FHint           := TPhoaToolSetting(Source).FHint;
      FKind           := TPhoaToolSetting(Source).FKind;
      FMasks          := TPhoaToolSetting(Source).FMasks;
      FRunCommand     := TPhoaToolSetting(Source).FRunCommand;
      FRunFolder      := TPhoaToolSetting(Source).FRunFolder;
      FRunParameters  := TPhoaToolSetting(Source).FRunParameters;
      FRunShowCommand := TPhoaToolSetting(Source).FRunShowCommand;
      FUsages         := TPhoaToolSetting(Source).FUsages;
    end;
  end;

  constructor TPhoaToolSetting.Create(AOwner: TPhoaSetting; const sName, sHint, sRunCommand, sRunFolder, sRunParameters, sMasks: String; AKind: TPhoaToolKind; iRunShowCommand: Integer; AUsages: TPhoaToolUsages);
  begin
    inherited Create(AOwner, 0, sName);
    FHint           := sHint;
    FRunCommand     := sRunCommand;
    FRunFolder      := sRunFolder;
    FRunParameters  := sRunParameters;
    FMasks          := sMasks;
    FKind           := AKind;
    FRunShowCommand := iRunShowCommand;
    FUsages         := AUsages;
  end;

  constructor TPhoaToolSetting.CreateNew(AOwner: TPhoaSetting);
  begin
    inherited CreateNew(AOwner);
    FKind           := ptkDefault;
    FRunShowCommand := SW_SHOWNORMAL;
    FUsages         := [ptuToolsMenu];
  end;

  function TPhoaToolSetting.GetStoreSection: String;
  begin
    Result := Format('%s\Item%.3d', [SRegPrefTools, Index]);
  end;

  procedure TPhoaToolSetting.IniLoad(IniFile: TIniFile);
  begin
    { Инструменты не поддерживают хранение в Ini-файлах }
  end;

  procedure TPhoaToolSetting.IniSave(IniFile: TIniFile);
  begin
    { Инструменты не поддерживают хранение в Ini-файлах }
  end;

  procedure TPhoaToolSetting.RegLoad(RegIniFile: TRegIniFile);
  var sSection: String;
  begin
    sSection := GetStoreSection;
    FName           := RegIniFile.ReadString (sSection, 'Name',       '');
    FHint           := RegIniFile.ReadString (sSection, 'Hint',       '');
    FKind           := PhoaToolKindFromStr(
                       RegIniFile.ReadString (sSection, 'Kind',       ''),
                       FKind);
    FMasks          := RegIniFile.ReadString (sSection, 'Masks',      '');
    FRunCommand     := RegIniFile.ReadString (sSection, 'RunCmd',     '');
    FRunFolder      := RegIniFile.ReadString (sSection, 'RunFolder',  '');
    FRunParameters  := RegIniFile.ReadString (sSection, 'RunParams',  '');
    FRunShowCommand := RegIniFile.ReadInteger(sSection, 'RunShowCmd', SW_SHOWNORMAL);
    FUsages         := PhoaToolUsagesFromStr(
                       RegIniFile.ReadString (sSection, 'Usages',     'M'));
    inherited RegLoad(RegIniFile);
  end;

  procedure TPhoaToolSetting.RegSave(RegIniFile: TRegIniFile);
  var sSection: String;
  begin
    sSection := GetStoreSection;
    RegIniFile.WriteString (sSection, 'Name',       FName);
    RegIniFile.WriteString (sSection, 'Hint',       FHint);
    RegIniFile.WriteString (sSection, 'Kind',       PhoaToolKindToStr(FKind));
    RegIniFile.WriteString (sSection, 'Masks',      FMasks);
    RegIniFile.WriteString (sSection, 'RunCmd',     FRunCommand);
    RegIniFile.WriteString (sSection, 'RunFolder',  FRunFolder);
    RegIniFile.WriteString (sSection, 'RunParams',  FRunParameters);
    RegIniFile.WriteInteger(sSection, 'RunShowCmd', FRunShowCommand);
    RegIniFile.WriteString (sSection, 'Usages',     PhoaToolUsagesToStr(FUsages));
    inherited RegSave(RegIniFile);
  end;

   //===================================================================================================================
   // TPhoaToolPageSetting
   //===================================================================================================================

  function TPhoaToolPageSetting.GetEditorClass: TWinControlClass;
  begin
    Result := TPhoaToolSettingEditor;
  end;

  procedure TPhoaToolPageSetting.IniLoad(IniFile: TIniFile);
  begin
    { Инструменты не поддерживают хранение в Ini-файлах }
  end;

  procedure TPhoaToolPageSetting.IniSave(IniFile: TIniFile);
  begin
    { Инструменты не поддерживают хранение в Ini-файлах }
  end;

  procedure TPhoaToolPageSetting.RegLoad(RegIniFile: TRegIniFile);
  var i, iCount: Integer;
  begin
     // Получаем количество детей
    iCount := RegIniFile.ReadInteger(SRegPrefTools, 'Count', -1);
     // Если сохранение не производилось, оставляем всё как есть. Иначе загружаем инструменты
    if iCount>=0 then begin
      ClearChildren;
      for i := 0 to iCount-1 do TPhoaToolSetting.CreateNew(Self).RegLoad(RegIniFile);
    end;
  end;

  procedure TPhoaToolPageSetting.RegSave(RegIniFile: TRegIniFile);
  begin
     // Стираем секцию инструментов
    RegIniFile.EraseSection(SRegPrefTools);
     // Пишем количество инструментов
    RegIniFile.WriteInteger(SRegPrefTools, 'Count', ChildCount);
     // Сохраняем всех детей
    inherited RegSave(RegIniFile);
  end;

   //===================================================================================================================
   // TPhoaToolSettingEditor
   //===================================================================================================================

  constructor TPhoaToolSettingEditor.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
     // Каждый узел хранит TPhoaToolSetting
    FDataOffset := AllocateInternalDataArea(SizeOf(Pointer));
    Align := alClient;
    Images := fMain.ilActionsSmall;
     // Настраиваем Header
    SetupHeader;
    with TreeOptions do begin
      AutoOptions      := [toAutoDropExpand, toAutoScroll, toAutoTristateTracking, toAutoDeleteMovedNodes];
      MiscOptions      := [toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning];
      PaintOptions     := [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [toFullRowSelect, toRightClickSelect];
    end;
    HintMode := hmTooltip;
     // Применяем опции
    ApplyTreeSettings(Self);
     // Создаём popup-menu
    CreatePopupMenu;
  end;

  procedure TPhoaToolSettingEditor.CreatePopupMenu;
  var pm: TTBXPopupMenu;

    function NewItem(iImgIdx: Integer; const sCaption, sShortcut: String; const ClickEvent: TNotifyEvent; bDefault: Boolean): TTBXItem;
    begin
      Result := TTBXItem.Create(Self);
      Result.ImageIndex := iImgIdx;
      Result.Caption    := sCaption;
      if bDefault then Result.Options := Result.Options+[tboDefault];
      Result.OnClick    := ClickEvent;
      Result.ShortCut   := TextToShortCut(sShortcut);
      pm.Items.Add(Result);
    end;

    procedure NewSeparator;
    begin
      pm.Items.Add(TTBXSeparatorItem.Create(Self));
    end;

  begin
     // Создаём меню
    pm := TTBXPopupMenu.Create(Self);
     // Наполняем пунктами
    FItemDelete   := NewItem(iiDelete, ConstVal('SDelete'),   'Del',       DeleteToolClick,   False);
    NewSeparator;
    FItemEdit     := NewItem(iiEdit,   ConstVal('SEdit'),     'Alt+Enter', EditToolClick,     True);
    NewSeparator;
    FItemMoveUp   := NewItem(iiUp,     ConstVal('SMoveUp'),   'Ctrl+Up',   MoveToolUpClick,   False);
    FItemMoveDown := NewItem(iiDown,   ConstVal('SMoveDown'), 'Ctrl+Down', MoveToolDownClick, False);
     // Привязываем ImageList
    pm.Images := fMain.ilActionsSmall;
     // Привязываем к дереву
    PopupMenu := pm;
  end;

  procedure TPhoaToolSettingEditor.DblClick;
  begin
    inherited DblClick;
    EditToolClick(nil);
  end;

  procedure TPhoaToolSettingEditor.DeleteToolClick(Sender: TObject);
  var n: PVirtualNode;
  begin
    n := FocusedNode;
    if n<>nil then begin
      GetSetting(n).Free;
      DeleteNode(n);
    end;
  end;

  procedure TPhoaToolSettingEditor.DoChecked(Node: PVirtualNode);
  var p: TPoint;
  begin
    ActivateVTVNode(Self, Node);
    with GetDisplayRect(Node, -1, False) do p := ClientToScreen(Point(Left, Bottom));
    PopupMenu.Popup(p.x, p.y);
  end;

  procedure TPhoaToolSettingEditor.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
  begin
    EnablePopupMenuItems;
  end;

  procedure TPhoaToolSettingEditor.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer);
  begin
    if (Kind in [ikNormal, ikSelected]) and (Column=0) and (Integer(Node.Index)<FRootSetting.ChildCount) then Index := iiTool;
  end;

  procedure TPhoaToolSettingEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    s: String;
    Setting: TPhoaToolSetting;
  begin
    s := '';
    Setting := GetSetting(Node);
    if Setting<>nil then
      case Column of
         // Текст
        0: s := Setting.Name;
         // Вид
        1: s := GetEnumName(TypeInfo(TPhoaToolKind), Byte(Setting.Kind));
         // Маски
        2: s := Setting.Masks;
         // Приложение
        3: if Setting.Kind=ptkCustom then s := Setting.RunCommand;
      end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TPhoaToolSettingEditor.DoInitNode(ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
     // Если это не последний "пустой" пункт, сохраняем пункт TPhoaToolSetting в Node.Data
    if Integer(Node.Index)<FRootSetting.ChildCount then PPhoaSetting(PChar(Node)+FDataOffset)^ := FRootSetting[Node.Index];
     // Настраиваем CheckType и CheckState - кнопку
    Node.CheckType := ctButton;
  end;

  procedure TPhoaToolSettingEditor.DoSettingChange;
  begin
    if Assigned(FOnSettingChange) then FOnSettingChange(Self);
  end;

  procedure TPhoaToolSettingEditor.EditToolClick(Sender: TObject);
  var n: PVirtualNode;
  begin
    n := FocusedNode;
    if (n<>nil) and EditTool(GetSetting(n), FRootSetting) then begin
      DoSettingChange;
      LoadTree;
    end;
  end;

  procedure TPhoaToolSettingEditor.EnablePopupMenuItems;
  var
    n: PVirtualNode;
    idx, idxMaxTool: Integer;
  begin
    n := FocusedNode;
    if n=nil then idx := -1 else idx := n.Index;
    idxMaxTool := RootNodeCount-2;
    FItemDelete.Enabled   := (idx>=0) and (idx<=idxMaxTool);
    FItemEdit.Enabled     := (idx>=0) and (idx<=idxMaxTool+1);
    FItemMoveUp.Enabled   := (idx>0) and (idx<=idxMaxTool);
    FItemMoveDown.Enabled := (idx>=0) and (idx<idxMaxTool);
  end;

  function TPhoaToolSettingEditor.GetRootSetting: TPhoaPageSetting;
  begin
    Result := FRootSetting;
  end;

  function TPhoaToolSettingEditor.GetSetting(Node: PVirtualNode): TPhoaToolSetting;
  begin
    if Node=nil then Result := nil else Result := PPhoaToolSetting(PChar(Node)+FDataOffset)^;
  end;

  procedure TPhoaToolSettingEditor.InitAndEmbed(ParentCtl: TWinControl; AOnSettingChange: TNotifyEvent; AOnDecodeText: TPhoaSettingDecodeTextEvent);
  begin
    Parent           := ParentCtl;
    FOnSettingChange := AOnSettingChange;
    FOnDecodeText    := AOnDecodeText;
  end;

  procedure TPhoaToolSettingEditor.LoadTree;
  begin
    BeginUpdate;
    try
       // Удаляем все узлы
      Clear;
       // Устанавливаем количество записей в корневом каталоге (+1 - на пустую строку для добавления)
      RootNodeCount := FRootSetting.ChildCount+1;
       // Инициализируем все узлы
      ReinitChildren(nil, True);
       // Выделяем первый узел
      ActivateVTVNode(Self, GetFirst);
    finally
      EndUpdate;
    end;
  end;

  procedure TPhoaToolSettingEditor.MoveToolDownClick(Sender: TObject);
  begin
    //!!!
  end;

  procedure TPhoaToolSettingEditor.MoveToolUpClick(Sender: TObject);
  begin
    //!!!
  end;

  procedure TPhoaToolSettingEditor.SetRootSetting(Value: TPhoaPageSetting);
  begin
    if FRootSetting<>Value then begin
      FRootSetting := Value as TPhoaToolPageSetting;
      LoadTree;
    end;
  end;

  procedure TPhoaToolSettingEditor.SetupHeader;
  begin
    with Header do begin
      with Columns.Add do begin
        Width := 150;
        Text  := 'Name'{!!!};
      end;
      with Columns.Add do begin
        Width := 70;
        Text  := 'Kind'{!!!};
      end;
      with Columns.Add do begin
        Width := 80;
        Text  := 'Masks'{!!!};
      end;
      Columns.Add.Text := 'Application'{!!!};
      AutoSizeIndex := 3;
      Options       := Options+[hoAutoResize, hoVisible];
    end;
  end;

end.
