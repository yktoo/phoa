//**********************************************************************************************************************
//  $Id: Main.pas,v 1.25 2004-06-09 14:50:58 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit Main;

interface

uses
   // GR32 must follow GraphicEx because of naming conflict between stretch filter constants
  Windows, Messages, SysUtils, Variants, Classes, Graphics, GraphicEx, GR32, Controls, Forms, Dialogs, phObj, ConsVars,
  ActiveX, XPMan,
  VirtualTrees, TBXDkPanels, ImgList, TB2Item, Placemnt, DTLangTools,
  TB2MRU, TBXExtItems, Menus, TBX, ActnList, TBXStatusBars, TBXLists,
  TB2Dock, TB2Toolbar;

type
  TfMain = class(TForm, IPhoaViews)
    alMain: TActionList;
    aNew: TAction;
    aOpen: TAction;
    aSave: TAction;
    aSaveAs: TAction;
    aExit: TAction;
    aAbout: TAction;
    aNewGroup: TAction;
    aNewPic: TAction;
    aDelete: TAction;
    aEdit: TAction;
    aSettings: TAction;
    aView: TAction;
    dkTop: TTBXDock;
    dkBottom: TTBXDock;
    dkLeft: TTBXDock;
    dkRight: TTBXDock;
    tbMain: TTBXToolbar;
    tbMenu: TTBXToolbar;
    smFile: TTBXSubmenuItem;
    iNew: TTBXItem;
    iOpen: TTBXItem;
    iSave: TTBXItem;
    iSaveAs: TTBXItem;
    iFileSep1: TTBXSeparatorItem;
    iExit: TTBXItem;
    smEdit: TTBXSubmenuItem;
    iNewGroup: TTBXItem;
    iNewPic: TTBXItem;
    iDelete: TTBXItem;
    iEditSep2: TTBXSeparatorItem;
    iEdit: TTBXItem;
    iEditSep3: TTBXSeparatorItem;
    iView: TTBXItem;
    smTools: TTBXSubmenuItem;
    iSettings: TTBXItem;
    smHelp: TTBXSubmenuItem;
    iAbout: TTBXItem;
    bNewGroup: TTBXItem;
    bSaveAs: TTBXItem;
    bSave: TTBXItem;
    bNew: TTBXItem;
    tbSep1: TTBXSeparatorItem;
    bDelete: TTBXItem;
    bNewPic: TTBXItem;
    tbSep2: TTBXSeparatorItem;
    bAbout: TTBXItem;
    bSettings: TTBXItem;
    bExit: TTBXItem;
    bView: TTBXItem;
    tbSep3: TTBXSeparatorItem;
    pmGroups: TTBXPopupMenu;
    ipmGroupsDelete: TTBXItem;
    ipmGroupsEdit: TTBXItem;
    ipmGroupsSep1: TTBXSeparatorItem;
    ipmGroupsNewGroup: TTBXItem;
    pmPics: TTBXPopupMenu;
    ipmPicsView: TTBXItem;
    ipmPicsSep1: TTBXSeparatorItem;
    ipmPicsEdit: TTBXItem;
    ipmPicsDelete: TTBXItem;
    ipmPicsSep2: TTBXSeparatorItem;
    ipmPicsNewPic: TTBXItem;
    sbarMain: TTBXStatusBar;
    mruOpen: TTBXMRUList;
    bOpen: TTBXSubmenuItem;
    bOpenMRU: TTBXMRUListItem;
    iFileSep2: TTBXSeparatorItem;
    smFileMRU: TTBXMRUListItem;
    smView: TTBXSubmenuItem;
    iToggleStatusbar: TTBXVisibilityToggleItem;
    iToggleToolbar: TTBXVisibilityToggleItem;
    aHelpContents: TAction;
    iHelpContents: TTBXItem;
    bHelpContents: TTBXItem;
    aStats: TAction;
    bStats: TTBXItem;
    pmView: TTBXPopupMenu;
    aFind: TAction;
    iFind: TTBXItem;
    iToolsSep1: TTBXSeparatorItem;
    aSelectAll: TAction;
    aSelectNone: TAction;
    iSelectNone: TTBXItem;
    iSelectAll: TTBXItem;
    ipmPicsSelectAll: TTBXItem;
    ipmPicsSelectNone: TTBXItem;
    aPicOps: TAction;
    iPicOps: TTBXItem;
    aSortPics: TAction;
    bEdit: TTBXItem;
    iSortPics: TTBXItem;
    dtlsMain: TDTLanguageSwitcher;
    tbViewSep1: TTBXSeparatorItem;
    fpMain: TFormPlacement;
    aCut: TAction;
    aCopy: TAction;
    aPaste: TAction;
    iPaste: TTBXItem;
    iCopy: TTBXItem;
    iCut: TTBXItem;
    iEditSep1: TTBXSeparatorItem;
    ipmPicsPaste: TTBXItem;
    ipmPicsCopy: TTBXItem;
    ipmPicsCut: TTBXItem;
    ipmPicsSep3: TTBXSeparatorItem;
    tbSep4: TTBXSeparatorItem;
    bPaste: TTBXItem;
    bCopy: TTBXItem;
    bCut: TTBXItem;
    aUndo: TAction;
    iUndo: TTBXItem;
    iEditSep4: TTBXSeparatorItem;
    bUndo: TTBXSubmenuItem;
    smUndoHistory: TTBXSubmenuItem;
    ilActionsSmall: TTBImageList;
    ilActionsMiddle: TTBImageList;
    ilActionsLarge: TTBImageList;
    aPhoaView_New: TAction;
    aPhoaView_Edit: TAction;
    aPhoaView_Delete: TAction;
    aPhoaView_MakeGroup: TAction;
    pmPhoaView: TTBXPopupMenu;
    iPhoaView_SetDefault: TTBXItem;
    iPhoaViewSep1: TTBXSeparatorItem;
    iPhoaViewSep2: TTBXSeparatorItem;
    iPhoaView_New: TTBXItem;
    iPhoaView_Delete: TTBXItem;
    iPhoaView_Edit: TTBXItem;
    iPhoaViewSep3: TTBXSeparatorItem;
    iPhoaView_MakeGroup: TTBXItem;
    gipmPhoaView: TTBGroupItem;
    ulToolbarUndo: TTBXUndoList;
    tbxlToolbarUndo: TTBXLabelItem;
    gismViewViews: TTBGroupItem;
    tbSepHelpWebsite: TTBXSeparatorItem;
    aHelpWebsite: TAction;
    iHelpWebsite: TTBXItem;
    bFind: TTBXItem;
    gipmPhoaViewViews: TTBGroupItem;
    aFileOperations: TAction;
    ipmGroupsSep2: TTBXSeparatorItem;
    ipmGroupsSortPics: TTBXItem;
    ipmGroupsStats: TTBXItem;
    ipmGroupsPicOps: TTBXItem;
    iFileOperations: TTBXItem;
    ipmPicsFileOperations: TTBXItem;
    ipmGroupsFileOperations: TTBXItem;
    aIniSaveSettings: TAction;
    aIniLoadSettings: TAction;
    iFileSep3: TTBXSeparatorItem;
    iIniLoadSettings: TTBXItem;
    iIniSaveSettings: TTBXItem;
    iToolsSep2: TTBXSeparatorItem;
    giTools_ToolsMenu: TTBGroupItem;
    ipmGroupsSep3: TTBXSeparatorItem;
    giTools_GroupsMenu: TTBGroupItem;
    ipmGroupsNewPic: TTBXItem;
    ipmPicsSep4: TTBXSeparatorItem;
    giTools_PicsMenu: TTBGroupItem;
    aHelpFAQ: TAction;
    iHelpFAQ: TTBXItem;
    dpGroups: TTBXDockablePanel;
    tvGroups: TVirtualStringTree;
    aRemoveSearchResults: TAction;
    iRemoveSearchResults: TTBXItem;
    procedure aaNew(Sender: TObject);
    procedure aaOpen(Sender: TObject);
    procedure aaSave(Sender: TObject);
    procedure aaSaveAs(Sender: TObject);
    procedure aaExit(Sender: TObject);
    procedure aaAbout(Sender: TObject);
    procedure aaNewGroup(Sender: TObject);
    procedure aaNewPic(Sender: TObject);
    procedure aaDelete(Sender: TObject);
    procedure aaSettings(Sender: TObject);
    procedure aaEdit(Sender: TObject);
    procedure aaView(Sender: TObject);
    procedure aaHelpContents(Sender: TObject);
    procedure aaStats(Sender: TObject);
    procedure aaFind(Sender: TObject);
    procedure aaSelectAll(Sender: TObject);
    procedure aaSelectNone(Sender: TObject);
    procedure aaPicOps(Sender: TObject);
    procedure aaSortPics(Sender: TObject);
    procedure aaCut(Sender: TObject);
    procedure aaCopy(Sender: TObject);
    procedure aaPaste(Sender: TObject);
    procedure aaUndo(Sender: TObject);
    procedure aaPhoaView_New(Sender: TObject);
    procedure aaPhoaView_Delete(Sender: TObject);
    procedure aaPhoaView_Edit(Sender: TObject);
    procedure aaPhoaView_MakeGroup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure fpMainSavePlacement(Sender: TObject);
    procedure fpMainRestorePlacement(Sender: TObject);
    procedure mruOpenClick(Sender: TObject; const Filename: String);
    procedure dtlsMainLanguageChanged(Sender: TObject);
    procedure SetPhoaViewClick(Sender: TObject);
    procedure SetGroupExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvGroupsCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure tvGroupsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure tvGroupsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvGroupsEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure tvGroupsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure tvGroupsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure tvGroupsDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure tvGroupsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure tvGroupsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure tvGroupsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure bUndoPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure ulToolbarUndoChange(Sender: TObject);
    procedure ulToolbarUndoClick(Sender: TObject);
    procedure aaHelpWebsite(Sender: TObject);
    procedure aaFileOperations(Sender: TObject);
    procedure aaIniSaveSettings(Sender: TObject);
    procedure aaIniLoadSettings(Sender: TObject);
    procedure pmGroupsPopup(Sender: TObject);
    procedure pmPicsPopup(Sender: TObject);
    procedure aaHelpFAQ(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure aaRemoveSearchResults(Sender: TObject);
  private
     // Рабочий альбом
    FPhoA: TPhotoAlbum;
     // Узел результатов поиска
    FSearchNode: PVirtualNode;
     // Список изображений - результаты поиска
    FSearchResults: TPhoaGroup;
     // Handle окна - следующего просмотрщика изменения содержимого clipboard
    FHNextClipbrdViewer: HWND;
     // Стек операций для отмены
    FOperations: TPhoaOperations;
     // Набор свойств, которые требуется отображать в подсказках дерева групп
    FGroupTreeHintProps: TGroupProperties;
     // Флаги, указывающие на то, что инструменты popup-меню групп и вьюера прошли проверку на доступность текущим
     //   выделенным изображениям
    FGroupsPopupToolsValidated: Boolean;
    FPicsPopupToolsValidated: Boolean;
     // Флаг того, что инициализация формы окончена
    FInitialized: Boolean;
     // Prop storage
    FViewer: TThumbnailViewer;
    FViewIndex: Integer;
     // Добавляет в настройки пункты выбора языка интерфейса
    procedure LoadLanguageSettings;
     // Применяет параметры настройки языка
    procedure ApplyLanguage;
     // Применяет параметры настройки инструментов
    procedure ApplyTools;
     // Проверяет необходимость сохранения файла фотоальбома. Возвращает True, если можно продолжать
    function  CheckSave: Boolean;
     // Загружает иерархию групп из фотоальбома в tvGroups
    procedure LoadGroupTree;
     // Загружает фотоальбом из файла
    procedure DoLoad(const sFileName: String);
     // Обрабатывает параметры командной строки
    procedure ProcessCommandLine;
     // Настраивает разрешённость Actions и настраивает Caption формы
    procedure EnableActions;
     // Настраивает доступность инструментов
    procedure EnableTools;
     // Настраивает доступность инструментов для заданного контейнерного пункта
    procedure DoEnableTools(Item: TTBCustomItem);
     // Создаёт список изображений:
     //   если активно дерево групп - то всех изображений группы
     //   если активен вьюер - то выделенных во вьюере изображений
     //   иначе возвращает пустой список
    function  GetSelectedPicLinks: TPhoaPicLinks;
     // Отображает результаты поиска. Если bForceRemove=True, удаляет узел результатов, иначе, при bDoSelectNode=True,
     //   выделяет узел
    procedure DisplaySearchResults(bForceRemove, bDoSelectNode: Boolean);
     // Возвращает вид узла
    function  GetNodeKind(Tree: TBaseVirtualTree; Node: PVirtualNode): TGroupNodeKind;
     // Viewer events
    procedure ViewerSelectionChange(Sender: TObject);
    procedure ViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
     // Application events
    procedure AppHint(Sender: TObject);
    procedure AppException(Sender: TObject; E: Exception);
    procedure AppIdle(Sender: TObject; var Done: Boolean);
     // Clipboard messages
    procedure WMChangeCBChain(var Msg: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Msg: TWMDrawClipboard); message WM_DRAWCLIPBOARD;
     // Откатывает все операции с последней до Index и обновляет визуальные объекты согласно флагам операций
    procedure UndoOperations(Index: Integer);
     // Откатывает индивидуальную операцию (for internal use only)
    procedure UndoOperation(Op: TPhoaOperation);
     // События списка операций
    procedure OperationsStatusChange(Sender: TObject);
    procedure OperationsListChange(Sender: TObject);
     // Событие клика на пункте инструмента
    procedure ToolItemClick(Sender: TObject);
     // Отображает прогресс загрузки
    procedure ShowProgressInfo(const sConstName: String; const aParams: Array of const);
     // Вводит в режим просмотра, начиная с текущего изображения
    procedure StartViewMode(InitFlags: TImgViewInitFlags);
     // Отменяет все "неустойчивые" режимы и возвращается в основной режим просмотра
    procedure ResetMode;
     // Находит и возвращает узел в tvGroups по ID группы; nil, если нет такого
    function  FindGroupNodeByID(iGroupID: Integer): PVirtualNode;
     // IPhoaViews
    function  GetViewIndex: Integer;
    procedure SetViewIndex(Value: Integer);
    function  GetViews: TPhoaViews;
    procedure LoadViewList(idxSelect: Integer);
     // Message handlers
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
    procedure WMStartViewMode(var Msg: TWMStartViewMode); message WM_STARTVIEWMODE;
     // Property handlers
    procedure SetFileName(const Value: String);
    function  GetFileName: String;
    function  GetDisplayFileName: String;
    function  GetCurGroup: TPhoaGroup;
    procedure SetCurGroup(Value: TPhoaGroup);
    function  GetCurRootGroup: TPhoaGroup;
  public
     // Выполняет операцию и обновляет визуальные объекты согласно флагам операции
    procedure PerformOperation(Op: TPhoaOperation);
     // Применяет параметры настройки
    procedure ApplySettings;
     // Переписывает во Viewer текущую выбранную группу
    procedure RefreshViewer;
     // Props
     // -- Имя текущего файла фотоальбома (пустая строка, если новый фотоальбом)
    property FileName: String read GetFileName write SetFileName;
     // -- Текущая выбранная группа в дереве
    property CurGroup: TPhoaGroup read GetCurGroup write SetCurGroup;
     // -- Текущая корневая группа в дереве (фотоальбома или представления)
    property CurRootGroup: TPhoaGroup read GetCurRootGroup;
     // -- Имя файла фотоальбома для отображения (не бывает пустым, в таком случае 'untitled.phoa')
    property DisplayFileName: String read GetDisplayFileName;
     // -- Просмотрщик эскизов
    property Viewer: TThumbnailViewer read FViewer;
     // -- Индекс текущего представления (-1 для дерева групп фотоальбома)
    property ViewIndex: Integer read GetViewIndex write SetViewIndex;
  end;

var
  fMain: TfMain;

implementation
{$R *.dfm}
uses
  GraphicStrings, Clipbrd, Math, Registry, jpeg, TypInfo, ChmHlp, // GraphicStrings => GraphicEx constants
  phUtils, phPhoa,
  udPicProps, udSettings, ufImgView, udSearch, udPhoAProps, udAbout, udPicOps, udSortPics, udViewProps, udSelPhoaGroup,
  ufAddFilesWizard, udStats, udFileOpsWizard, phSettings, phValSetting,
  phToolSetting, udMsgBox, udGroupProps;

   //===================================================================================================================
   //  TfMain
   //===================================================================================================================

  procedure TfMain.aaAbout(Sender: TObject);
  begin
    ResetMode;
    ShowAbout(SettingValueBool(ISettingID_Dlgs_SplashAboutFade));
  end;

  procedure TfMain.aaCopy(Sender: TObject);
  begin
    ResetMode;
    TPhoaBaseOp_PicCopy.Create(Viewer.GetSelectedPicArray);
  end;

  procedure TfMain.aaCut(Sender: TObject);
  begin
    ResetMode;
    TPhoaBaseOp_PicCopy.Create(Viewer.GetSelectedPicArray);
    PerformOperation(TPhoaMultiOp_PicDelete.Create(FOperations, FPhoA, CurGroup, PicArrayToIDArray(Viewer.GetSelectedPicArray)));
  end;

  procedure TfMain.aaDelete(Sender: TObject);
  begin
    ResetMode;
    if CurGroup<>nil then
       // Удаление группы
      if ActiveControl=tvGroups then begin
        if PhoaConfirm(False, 'SConfirm_DelGroup', ISettingID_Dlgs_ConfmDelGroup) then
          PerformOperation(TPhoaOp_GroupDelete.Create(FOperations, FPhoA, CurGroup, True));
       // Удаление изображения
      end else if (ActiveControl=Viewer) and (Viewer.SelCount>0) and PhoaConfirm(False, 'SConfirm_DelPics', ISettingID_Dlgs_ConfmDelPics) then
        PerformOperation(TPhoaMultiOp_PicDelete.Create(FOperations, FPhoA, CurGroup, PicArrayToIDArray(Viewer.GetSelectedPicArray)));
  end;

  procedure TfMain.aaEdit(Sender: TObject);
  begin
    ResetMode;
     // Редактирование групп
    if ActiveControl=tvGroups then begin
      case GetNodeKind(tvGroups, tvGroups.FocusedNode) of
        gnkPhoA:      EditPhoA(FPhoA, FOperations);
        gnkView:      EditView(FPhoA.Views[ViewIndex], FPhoA, FOperations);
        gnkPhoaGroup: EditPicGroup(FPhoA, CurGroup, FOperations);
      end;
     // Редактирование изображения
    end else if (ActiveControl=Viewer) and (Viewer.SelCount>0) and EditPic(Viewer.GetSelectedPicArray, FPhoA, FOperations) then
      RefreshViewer;
  end;

  procedure TfMain.aaExit(Sender: TObject);
  begin
    ResetMode;
    Close;
  end;

  procedure TfMain.aaFileOperations(Sender: TObject);
  var
    View: TPhoaView;
    bPhoaChanged: Boolean;
  begin
    ResetMode;
    if ViewIndex>=0 then View := FPhoA.Views[ViewIndex] else View := nil;
    if DoFileOperations(FPhoA, CurGroup, View, PicArrayToIDArray(Viewer.GetSelectedPicArray), ActiveControl=Viewer, bPhoaChanged) then
       // Если изменилось содержимое фотоальбома
      if bPhoaChanged then begin
         // Помечаем текущее состояние как изменённое без возможности отката
        FOperations.SetNonUndoable;
         // Перестраиваем представления
        FPhoA.Views.UnprocessAllViews;
         // Перегружаем дерево папок
        LoadGroupTree;
       // Иначе просто были внесены необратимые изменения (в файловую систему, но не в фотоальбом) - запрещаем откат
      end else
        FOperations.Clear;
  end;

  procedure TfMain.aaFind(Sender: TObject);
  begin
    ResetMode;
    if DoSearch(FPhoA, CurGroup, FSearchResults) then begin
      DisplaySearchResults(False, True);
      RefreshViewer;
    end;
  end;

  procedure TfMain.aaHelpContents(Sender: TObject);
  begin
    ResetMode;
    HtmlHelpShowContents;
  end;

  procedure TfMain.aaHelpFAQ(Sender: TObject);
  begin
    ResetMode;
    HtmlHelpContext(IDH_faq);
  end;

  procedure TfMain.aaHelpWebsite(Sender: TObject);
  begin
    ResetMode;
    OpenWebsite;
  end;

  procedure TfMain.aaIniLoadSettings(Sender: TObject);

    procedure DoIniLoad(const sFileName: String);
    begin
       // Загружаем настройки
      IniLoadSettings(sFileName);
       // Применяем настройки
      ApplySettings;
      ApplyLanguage;
    end;

  begin
    ResetMode;
    with TOpenDialog.Create(Self) do
      try
        DefaultExt := SDefaultIniFileExt;
        FileName   := SDefaultIniFileName;
        Filter     := ConstVal('SFileFilter_Ini');
        Options    := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
        Title      := ConstVal('SDlgTitle_OpenIni');
        if Execute then DoIniLoad(FileName);
      finally
        Free;
      end;
  end;

  procedure TfMain.aaIniSaveSettings(Sender: TObject);
  begin
    ResetMode;
    with TSaveDialog.Create(Self) do
      try
        DefaultExt := SDefaultIniFileExt;
        FileName   := SDefaultIniFileName;
        Filter     := ConstVal('SFileFilter_Ini');
        Options    := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
        Title      := ConstVal('SDlgTitle_SaveIni');
        if Execute then IniSaveSettings(FileName);
      finally
        Free;
      end;
  end;

  procedure TfMain.aaNew(Sender: TObject);
  begin
    ResetMode;
    if not CheckSave then Exit;
    tvGroups.BeginUpdate;
    try
      FOperations.BeginUpdate;
      try
         // Стираем результаты поиска
        DisplaySearchResults(True, False);
         // Инициализируем фотоальбом и буфер отката
        FPhoA.New(FOperations);
         // Загружаем группы изображений
        LoadViewList(-1);
      finally
        FOperations.EndUpdate;
      end;
    finally
      tvGroups.EndUpdate;
    end;
  end;

  procedure TfMain.aaNewGroup(Sender: TObject);
  begin
    ResetMode;
    PerformOperation(TPhoaOp_GroupNew.Create(FOperations, FPhoA, CurGroup));
  end;

  procedure TfMain.aaNewPic(Sender: TObject);
  begin
    ResetMode;
    if SelectFiles(FPhoA, PPhoaGroup(tvGroups.GetNodeData(tvGroups.FocusedNode))^, FOperations) then RefreshViewer;
  end;

  procedure TfMain.aaOpen(Sender: TObject);
  begin
    ResetMode;
    with TOpenDialog.Create(Self) do
      try
        DefaultExt := SDefaultExt;
        Filter     := ConstVal('SFileFilter_OpenPhoa');
        Options    := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
        Title      := ConstVal('SDlgTitle_OpenPhoa');
        if Execute and CheckSave then DoLoad(FileName);
      finally
        Free;
      end;
  end;

  procedure TfMain.aaPaste(Sender: TObject);
  var iCntBefore: Integer;
  begin
    ResetMode;
    iCntBefore := CurGroup.PicIDs.Count;
    PerformOperation(TPhoaMultiOp_PicPaste.Create(FOperations, FPhoA, CurGroup));
    PhoaInfo(False, 'SNotify_Paste', [CurGroup.PicIDs.Count-iCntBefore], ISettingID_Dlgs_NotifyPaste);
  end;

  procedure TfMain.aaPhoaView_Delete(Sender: TObject);
  begin
    ResetMode;
    if PhoaConfirm(False, 'SConfirm_DelView', ISettingID_Dlgs_ConfmDelView) then
      TPhoaOp_ViewDelete.Create(FOperations, Self);
  end;

  procedure TfMain.aaPhoaView_Edit(Sender: TObject);
  begin
    ResetMode;
    EditView(FPhoA.Views[ViewIndex], FPhoA, FOperations);
  end;

  procedure TfMain.aaPhoaView_MakeGroup(Sender: TObject);
  begin
    ResetMode;
    MakeGroupFromView(FPhoA, FOperations, Self);
  end;

  procedure TfMain.aaPhoaView_New(Sender: TObject);
  begin
    ResetMode;
    EditView(nil, FPhoA, FOperations);
  end;

  procedure TfMain.aaPicOps(Sender: TObject);
  begin
    ResetMode;
    DoPicOps(FPhoA, FOperations, CurGroup, PicArrayToIDArray(Viewer.GetSelectedPicArray));
  end;

  procedure TfMain.aaRemoveSearchResults(Sender: TObject);
  begin
    ResetMode;
    DisplaySearchResults(True, False);
  end;

  procedure TfMain.aaSave(Sender: TObject);
  begin
    ResetMode;
     // Если имя файла не задано, выполняем SaveAs
    if FileName='' then
      aaSaveAs(Sender)
     // Иначе сохраняем файл фотоальбома и помечаем текущее состояние буфера отката как "сохранённое"
    else begin
      FOperations.BeginUpdate;
      StartWait;
      try
        FPhoA.FileSave(FOperations);
      finally
        StopWait;
        FOperations.EndUpdate;
      end;
      EnableActions;
    end;
  end;

  procedure TfMain.aaSaveAs(Sender: TObject);
  begin
    ResetMode;
    with TSaveDialog.Create(Self) do
      try
        DefaultExt  := SDefaultExt;
        Filter      := GetPhoaSaveFilter;
        FilterIndex := ValidRevisionIndex(GetIndexOfRevision(FPhoA.FileRevision))+1;
        Options     := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
        Title       := ConstVal('SDlgTitle_SavePhoa');
        FileName    := DisplayFileName;
        if Execute then begin
           // Сохраняем файл с заданными параметрами, помечаем текущее состояние буфера отката как "сохранённое"
          FOperations.BeginUpdate;
          StartWait;
          try
            FPhoA.FileSaveTo(FileName, aPhFileRevisions[ValidRevisionIndex(FilterIndex-1)].iNumber, FOperations);
          finally
            StopWait;
            FOperations.EndUpdate;
          end;
           // Регистрируем имя файла в списке MRU
          mruOpen.Add(FileName);
          EnableActions;
        end;
      finally
        Free;
      end;
  end;

  procedure TfMain.aaSelectAll(Sender: TObject);
  begin
    ResetMode;
    Viewer.SelectAll;
  end;

  procedure TfMain.aaSelectNone(Sender: TObject);
  begin
    ResetMode;
    Viewer.SelectNone;
  end;

  procedure TfMain.aaSettings(Sender: TObject);
  begin
    ResetMode;
     // В диалоге настроек по умолчанию выбираем кнопку "Режим обзора"
    if EditSettings(ISettingID_Browse) then begin
      ApplySettings;
       // Применяем разрёшённость Drag'n'Drop у Viewer
      EnableActions;
    end;
  end;

  procedure TfMain.aaSortPics(Sender: TObject);
  begin
    ResetMode;
    DoSortPics(FPhoA, CurGroup, FOperations, CurGroup=FSearchResults);
  end;

  procedure TfMain.aaStats(Sender: TObject);
  begin
    ResetMode;
    ShowPhoaStats(FPhoA, CurGroup, Viewer.GetSelectedPicArray);
  end;

  procedure TfMain.aaUndo(Sender: TObject);
  begin
    ResetMode;
    UndoOperations(FOperations.Count-1);
  end;

  procedure TfMain.aaView(Sender: TObject);
  begin
    ResetMode;
    StartViewMode([]);
  end;

  procedure TfMain.AppException(Sender: TObject; E: Exception);
  var s: String;
  begin
     // Добавляем точку, если в конце не знак препинания (ripped from Application.ShowException)
    s := E.Message;
    if (s<>'') and (AnsiLastChar(s)>'.') then s := s+'.';
     // Кажем сообщение об ошибке
    PhoaMsgBox(mbkError, s, False, False, [mbbOK]);
  end;

  procedure TfMain.AppHint(Sender: TObject);
  begin
    sbarMain.Panels[0].Caption := Application.Hint;
  end;

  procedure TfMain.AppIdle(Sender: TObject; var Done: Boolean);
  begin
     // Уничтожаем окно прогресса, если оно есть
    if ProgressWnd<>nil then begin
      ProgressWnd.DisplayStage('');
      ProgressWnd.AnimateFadeout := SettingValueBool(ISettingID_Dlgs_SplashStartFade);
      ProgressWnd.HideWindow;
    end;
  end;

  procedure TfMain.ApplyLanguage;
  begin
     // Настраиваем прочие свойства
    if FInitialized then begin
       // Перестраиваем представления, т.к. они содержат локализуемые названия узлов
      FPhoA.Views.UnprocessAllViews;
       // Перерисовываем дерево
      tvGroups.ReinitChildren(nil, True);
      tvGroups.Invalidate;
       // Обновляем заголовок окна
      EnableActions;
    end;
     // Настраиваем Help-файл
    Application.HelpFile := ExtractFilePath(ParamStr(0))+ConstVal('SHelpFileName');
  end;

  procedure TfMain.ApplySettings;

    procedure SetupViewerCorner(Corner: TThumbCorner; iSettingID: Integer);
    var
      i: Integer;
      tcd: TThumbCornerDetail;
    begin
      i := SettingValueInt(iSettingID);
      tcd.bDisplay := (i>=Byte(Low(TPicProperty))) and (i<=Byte(High(TPicProperty)));
      if tcd.bDisplay then tcd.Prop := TPicProperty(i);
      Viewer.ThumbCornerDetails[Corner] := tcd;
    end;

  begin
     // Настраиваем язык интерфейса
    dtlsMain.Language := SettingValueInt(ISettingID_Gen_Language);
     // Настраиваем основной шрифт программы
    FontFromStr(Font, SettingValueStr(ISettingID_Gen_MainFont));
    ToolbarFont.Assign(Font);
     // Настраиваем текущую кодовую страницу
    cMainCodePage := CharsetToCP(Font.Charset);
     // Настраиваем список последних открывавшихся файлов
    mruOpen.MaxItems := SettingValueInt(ISettingID_Gen_OpenMRUCount);
     // Настраиваем подсказки
    Application.HintHidePause := SettingValueInt(ISettingID_Gen_TooltipDisplTime);
     // Настраиваем доки/панели инструментов
     // -- Перетаскиваемость
    ApplyToolbarSettings(dkTop);
    ApplyToolbarSettings(dkLeft);
    ApplyToolbarSettings(dkRight);
    ApplyToolbarSettings(dkBottom);
     // -- Размер кнопок основной панели
    case SettingValueInt(ISettingID_Gen_ToolbarBtnSize) of
      0: tbMain.Images := ilActionsSmall;
      1: tbMain.Images := ilActionsMiddle;
      2: tbMain.Images := ilActionsLarge;
    end;
     // Настраиваем дерево групп
    ApplyTreeSettings(tvGroups);
    tvGroups.HintMode := GTreeHintModeToVTHintMode(TGroupTreeHintMode(SettingValueInt(ISettingID_Browse_GT_Hints)));
    FGroupTreeHintProps := IntToGroupProps(SettingValueInt(ISettingID_Browse_GT_HintProps));
     // Настраиваем Viewer
    with Viewer do begin
      BeginUpdate;
      try
        ThickThumbBorder  := SettingValueBool(ISettingID_Browse_ViewerThBorder);
        CacheThumbnails   := SettingValueBool(ISettingID_Browse_ViewerCacheThs);
        ThumbCacheSize    := SettingValueInt (ISettingID_Browse_ViewerCacheSze);
        Color             := SettingValueInt (ISettingID_Browse_ViewerBkColor);
        ThumbBackColor    := SettingValueInt (ISettingID_Browse_ViewerThBColor);
        ThumbFontColor    := SettingValueInt (ISettingID_Browse_ViewerThFColor);
        ShowThumbTooltips := SettingValueBool(ISettingID_Browse_ViewerTooltips);
        ThumbTooltipProps := IntToPicProps(SettingValueInt(ISettingID_Browse_ViewerTipProps));
        SetupViewerCorner(tcLeftTop,     ISettingID_Browse_ViewerThLTProp);
        SetupViewerCorner(tcRightTop,    ISettingID_Browse_ViewerThRTProp);
        SetupViewerCorner(tcLeftBottom,  ISettingID_Browse_ViewerThLBProp);
        SetupViewerCorner(tcRightBottom, ISettingID_Browse_ViewerThRBProp);
      finally
        EndUpdate;
      end;
    end;
     // Применяем инструменты
    if RootSetting.Settings[ISettingID_Tools].Modified then ApplyTools;
     // Помечаем все настройки как неизменённые
    RootSetting.Modified := False;
  end;

  procedure TfMain.ApplyTools;
  var
    i: Integer;
    Tool: TPhoaToolSetting;
  begin
     // Всё стираем
    giTools_ToolsMenu.Clear;
    giTools_GroupsMenu.Clear;
    giTools_PicsMenu.Clear;
     // Добавляем инструменты
    for i := 0 to RootSetting.Settings[ISettingID_Tools].ChildCount-1 do begin
      Tool := RootSetting.Settings[ISettingID_Tools].Children[i] as TPhoaToolSetting;
      if ptuToolsMenu         in Tool.Usages then AddToolItem(Tool, giTools_ToolsMenu,  ToolItemClick);
      if ptuGroupPopupMenu    in Tool.Usages then AddToolItem(Tool, giTools_GroupsMenu, ToolItemClick);
      if ptuThViewerPopupMenu in Tool.Usages then AddToolItem(Tool, giTools_PicsMenu,   ToolItemClick);
    end;
  end;

  procedure TfMain.bUndoPopup(Sender: TTBCustomItem; FromLink: Boolean);
  var i: Integer;
  begin
    ulToolbarUndo.Strings.Clear;
    for i := FOperations.Count-1 downto 0 do ulToolbarUndo.Strings.Add(FOperations[i].Name);
     // Initialize tbxlToolbarUndo.Caption
    ulToolbarUndoChange(nil);
  end;

  function TfMain.CheckSave: Boolean;
  var mbr: TMessageBoxResults;
  begin
    Result := FOperations.IsUnmodified;
    if not Result then begin
      mbr := PhoaMsgBox(mbkConfirm, 'SConfirm_FileNotSaved', [DisplayFileName], True, False, [mbbYes, mbbNo, mbbCancel]);
      if mbrYes in mbr then begin
        aSave.Execute;
        Result := FOperations.IsUnmodified;
      end else if mbrNo in mbr then
        Result := True;
    end;
  end;

  procedure TfMain.CMFocusChanged(var Msg: TCMFocusChanged);
  begin
    EnableActions;
  end;

  procedure TfMain.DisplaySearchResults(bForceRemove, bDoSelectNode: Boolean);
  begin
    if bForceRemove then FSearchResults.PicIDs.Clear;
     // Если есть результаты, следим, чтобы узел поиска существовал
    if FSearchResults.PicIDs.Count>0 then begin
      if FSearchNode=nil then FSearchNode := tvGroups.AddChild(nil);
       // Если надо выделить узел
      if bDoSelectNode then ActivateVTNode(tvGroups, FSearchNode);
     // Если нет результатов - следим, чтобы его не было
    end else if FSearchNode<>nil then begin
      tvGroups.DeleteNode(FSearchNode);
      FSearchNode := nil;
    end;
  end;

  procedure TfMain.DoEnableTools(Item: TTBCustomItem);
  var PicLinks: TPhoaPicLinks;
  begin
     // Если пункт содержит подпункты-инструменты
    if Item.Count>0 then begin
       // Создаём список ссылок на изображения
      PicLinks := GetSelectedPicLinks;
      try
         // Настраиваем доступность инструментов
        AdjustToolAvailability(RootSetting.Settings[ISettingID_Tools] as TPhoaToolPageSetting, Item, PicLinks);
      finally
        PicLinks.Free;
      end;
    end;
  end;

  procedure TfMain.DoLoad(const sFileName: String);
  begin
    tvGroups.BeginSynch;
    try
      FOperations.BeginUpdate;
      tvGroups.BeginUpdate;
      StartWait;
      try
        try
           // Уничтожаем результаты поиска
          DisplaySearchResults(True, False);
           // Загружаем файл и очищаем буфер отката
          FPhoA.FileLoad(ExpandUNCFileName(sFileName), FOperations);
           // Регистрируем файл в списке MRU
          mruOpen.Add(FileName);
        finally
           // Загружаем список представлений и по умолчанию выбираем "Группы изображений"
          LoadViewList(-1);
        end;
      finally
        StopWait;
        tvGroups.EndUpdate;
        FOperations.EndUpdate;
      end;
    finally
      tvGroups.EndSynch;
    end;
  end;

  procedure TfMain.dtlsMainLanguageChanged(Sender: TObject);
  begin
    ResetMode;
    ApplyLanguage;
  end;

  procedure TfMain.EnableActions;
  const asUnmod: Array[Boolean] of String[1] = ('*', '');
  var
    bGr, bPic, bPics, bPicSel, bView: Boolean;
    gnk: TGroupNodeKind;
  begin
    if not FInitialized or (csDestroying in ComponentState) then Exit;
    gnk := GetNodeKind(tvGroups, tvGroups.FocusedNode);
    bGr     := ActiveControl=tvGroups;
    bPic    := ActiveControl=Viewer;
    bPics   := FPhoA.Pics.Count>0;
    bPicSel := Viewer.SelCount>0;
    bView   := ViewIndex>=0;
    aUndo.Caption := ConstVal(iif(FOperations.CanUndo, 'SUndoActionTitle', 'SCannotUndo'), [FOperations.LastOpName]);
    aUndo.Enabled                := FOperations.CanUndo;
    smUndoHistory.Enabled        := FOperations.CanUndo;
    aNewGroup.Enabled            := gnk in [gnkPhoA, gnkPhoaGroup];
    aNewPic.Enabled              := gnk in [gnkPhoA, gnkPhoaGroup];
    aDelete.Enabled              := (bGr and (gnk=gnkPhoaGroup)) or (bPic and (gnk in [gnkPhoA, gnkPhoaGroup]) and bPicSel);
    aEdit.Enabled                := (bGr and (gnk in [gnkPhoA, gnkPhoaGroup, gnkView])) or (bPic and (gnk in [gnkPhoA, gnkPhoaGroup]) and bPicSel);
    aCut.Enabled                 := (gnk in [gnkPhoA, gnkPhoaGroup]) and bPicSel and (wClipbrdPicFormatID<>0);
    aCopy.Enabled                := bPicSel and (wClipbrdPicFormatID<>0);
    aPaste.Enabled               := (gnk in [gnkPhoA, gnkPhoaGroup]) and Clipboard.HasFormat(wClipbrdPicFormatID);
    aSortPics.Enabled            := (gnk in [gnkPhoA, gnkPhoaGroup, gnkSearch]) and bPics;
    aSelectAll.Enabled           := (gnk<>gnkNone) and (Viewer.SelCount<CurGroup.PicIDs.Count);
    aSelectNone.Enabled          := bPicSel;
    aView.Enabled                := Viewer.ItemIndex>=0;
    aRemoveSearchResults.Enabled := FSearchNode<>nil;
    aPicOps.Enabled              := (gnk in [gnkPhoA, gnkPhoaGroup]) and bPicSel;
    aFileOperations.Enabled      := bPics;
    aFind.Enabled                := bPics;
     // Views
    aPhoaView_Delete.Enabled    := bView;
    aPhoaView_Edit.Enabled      := bView;
    aPhoaView_MakeGroup.Enabled := bView;
     // Drag-and-drop
    Viewer.DragEnabled       := (gnk in [gnkPhoA, gnkPhoaGroup, gnkSearch]) and SettingValueBool(ISettingID_Browse_ViewerDragDrop);
    Viewer.DragInsideEnabled := gnk in [gnkPhoA, gnkPhoaGroup];
     // Инструменты
    EnableTools;
     // Настраиваем Captions
    Caption := Format('[%s%s] - %s', [ExtractFileName(DisplayFileName), asUnmod[FOperations.IsUnmodified], ConstVal('SAppCaption')]);
    Application.Title := Caption;
    sbarMain.Panels[1].Caption := ConstVal('SPicCount', [FPhoA.Pics.Count]);
  end;

  procedure TfMain.EnableTools;
  begin
     // Настраиваем инструменты меню "Сервис"
    DoEnableTools(giTools_ToolsMenu);
     // Сбрасываем флаги инструментов popup-меню
    FGroupsPopupToolsValidated := False;
    FPicsPopupToolsValidated   := False;
  end;

  function TfMain.FindGroupNodeByID(iGroupID: Integer): PVirtualNode;
  begin
    Result := tvGroups.GetFirst;
    while Result<>nil do begin
      if PPhoaGroup(tvGroups.GetNodeData(Result))^.ID=iGroupID then Exit;
      Result := tvGroups.GetNext(Result);
    end;
  end;

  procedure TfMain.FormActivate(Sender: TObject);
  begin
     // Если есть окошко прогресса, держим главное окно позади него  
    if ProgressWnd<>nil then SetWindowPos(Handle, ProgressWnd.Handle, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
  end;

  procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    ResetMode;
     // Если нет несохранённых данных - проверяем необходимость спрашивания подтверждения
    if FOperations.IsUnmodified then
      CanClose := PhoaConfirm(False, 'SConfirm_AppExit', ISettingID_Dlgs_ConfmAppExit)
     // Иначе спрашиваем, сохранять ли данные 
    else
      CanClose := CheckSave;
  end;

  procedure TfMain.FormCreate(Sender: TObject);
  begin
    try
      ShowProgressInfo('SMsg_Initializing', []);
      ShortTimeFormat := 'hh:nn';
      LongTimeFormat  := 'hh:nn:ss';
       // Настраиваем fpMain
      fpMain.IniFileName := SRegRoot;
      fpMain.IniSection  := SRegMainWindow_Root;     
       // Создаём фотоальбом
      FPhoA := TPhotoAlbum.Create;
      FViewIndex := -1;
       // Настраиваем Application
      Application.OnHint      := AppHint;
      Application.OnException := AppException;
      Application.OnIdle      := AppIdle;
       // Создаём группу - список результатов поиска
      FSearchResults := TPhoaGroup.Create(nil, IGroupID_SearchResults);
       // Create undoable operations list
      FOperations := TPhoaOperations.Create;
      FOperations.OnStatusChange := OperationsStatusChange;
      FOperations.OnOpDone       := OperationsListChange;
      FOperations.OnOpUndone     := OperationsListChange;
       // Create viewer
      FViewer := TThumbnailViewer.Create(Self);
      with FViewer do begin
        Parent            := Self;
        Align             := alClient;
        DragCursor        := crDragMove;
        PhoA              := FPhoA;
        PopupMenu         := pmPics;
        OnDblClick        := aaView;
        OnDragDrop        := ViewerDragDrop;
        OnSelectionChange := ViewerSelectionChange;
      end;
       // Load language list
      LoadLanguageSettings;
       // Add self to the clipboard viewer chain
      FHNextClipbrdViewer := SetClipboardViewer(Handle);
       // Применяем настройки
      ShowProgressInfo('SMsg_ApplyingSettings', []);
      RootSetting.Modified := True;
      ApplySettings;
      ApplyLanguage;
       // Настраиваем дерево папок
      tvGroups.BeginSynch;
      try
        tvGroups.NodeDataSize  := SizeOf(Pointer);
        tvGroups.RootNodeCount := 1;
         // Обрабатываем параметры командной строки
        ProcessCommandLine;
      finally
        tvGroups.EndSynch;
      end;
    finally
      FInitialized := True;
    end;
  end;

  procedure TfMain.FormDestroy(Sender: TObject);
  begin
     // Remove self from the clipboard viewer chain
    ChangeClipboardChain(Handle, FHNextClipbrdViewer);
    FPhoA.Free;
    FSearchResults.Free;
    FOperations.Free;
  end;

  procedure TfMain.fpMainRestorePlacement(Sender: TObject);
  begin
     // Load history
    mruOpen.LoadFromRegIni(fpMain.RegIniFile, SRegOpen_FilesMRU);
     // Load toolbars
    TBRegLoadPositions(Self, HKEY_CURRENT_USER, SRegRoot+'\'+SRegMainWindow_Toolbars);
  end;

  procedure TfMain.fpMainSavePlacement(Sender: TObject);
  begin
     // Save toolbars
    TBRegSavePositions(Self, HKEY_CURRENT_USER, SRegRoot+'\'+SRegMainWindow_Toolbars);
     // Save history
    mruOpen.SaveToRegIni(fpMain.RegIniFile, SRegOpen_FilesMRU);
  end;

  function TfMain.GetCurGroup: TPhoaGroup;
  var p: PPhoaGroup;
  begin
    p := tvGroups.GetNodeData(tvGroups.FocusedNode);
    if p=nil then Result := nil else Result := p^;
  end;

  function TfMain.GetCurRootGroup: TPhoaGroup;
  begin
    if ViewIndex<0 then Result := FPhoA.RootGroup else Result := FPhoA.Views[ViewIndex].RootGroup;
  end;

  function TfMain.GetDisplayFileName: String;
  begin
    Result := FileName;
    if Result='' then Result := SDefaultFName;
  end;

  function TfMain.GetFileName: String;
  begin
    Result := FPhoA.FileName;
  end;

  function TfMain.GetNodeKind(Tree: TBaseVirtualTree; Node: PVirtualNode): TGroupNodeKind;
  var g: TPhoaGroup;
  begin
    Result := gnkNone;
    if Node<>nil then begin
      g := PPhoaGroup(Tree.GetNodeData(Node))^;
      if g.Owner=nil then begin
        if g.ID=IGroupID_SearchResults then Result := gnkSearch
        else if g=FPhoA.RootGroup      then Result := gnkPhoA
        else if FViewIndex>=0          then Result := gnkView;
      end else
        if FViewIndex>=0 then Result := gnkViewGroup else Result := gnkPhoaGroup;
    end;
  end;

  function TfMain.GetSelectedPicLinks: TPhoaPicLinks;
  begin
    Result := TPhoaPicLinks.Create(True);
    try
       // Если активны группы - составляем список ссылок на изображения текущей группы
      if tvGroups.Focused then begin
        if CurGroup<>nil then Result.AddFromGroup(FPhoA, CurGroup, False);
       // Если активен вьюер - составляем список ссылок на выделенные изображения вьюера
      end else if Viewer.Focused then
        Result.AddFromPicIDs(FPhoA, PicArrayToIDArray(Viewer.GetSelectedPicArray), False);
    except
      Result.Free;
      raise;
    end;
  end;

  function TfMain.GetViewIndex: Integer;
  begin
    Result := FViewIndex;
  end;

  function TfMain.GetViews: TPhoaViews;
  begin
    Result := FPhoA.Views;
  end;

  procedure TfMain.LoadGroupTree;
  begin
    ResetMode;
    tvGroups.BeginUpdate;
    try
      tvGroups.ReinitChildren(nil, True);
      ActivateVTNode(tvGroups, tvGroups.GetFirst);
    finally
      tvGroups.EndUpdate;
    end;
    RefreshViewer;
  end;

  procedure TfMain.LoadLanguageSettings;
  var
    Langs: TLanguages;
    LangSetting: TPhoaSetting;
    i: Integer;
  begin
     // Находим пункт списка "Язык интерфейса"
    LangSetting := RootSetting.Settings[ISettingID_Gen_Language];
     // Составляем список доступных языков
    Langs := TLanguages.Create;
    try
      dtlsMain.RootComp.BuildLangList(Langs, True, False);
       // Создаём пункты выбора
      for i := 0 to Langs.Count-1 do TPhoaMutexIntSetting.Create(LangSetting, 0, Langs.Names[i], Langs[i]);
    finally
      Langs.Free;
    end;
  end;

  procedure TfMain.LoadViewList(idxSelect: Integer);
  var
    i: Integer;
    tbi: TTBXItem;
  begin
     // Стираем все пункты пользовательских представлений
    gipmPhoaViewViews.Clear;
     // Добавляем пункты представлений фотоальбома
    for i := 0 to FPhoA.Views.Count-1 do begin
      tbi := TTBXItem.Create(Self);
      with tbi do begin
        Caption    := FPhoA.Views[i].Name;
        ImageIndex := iiView;
        Tag        := i+1; // Tag=0 у пункта iPhoaView_SetDefault ("Группы изображений")
        OnClick    := SetPhoaViewClick;
      end;
      gipmPhoaViewViews.Add(tbi);
    end;
     // Выставляем текущее представление
    SetViewIndex(idxSelect);
  end;

  procedure TfMain.mruOpenClick(Sender: TObject; const Filename: String);
  begin
    ResetMode;
    if CheckSave then DoLoad(FileName);
  end;

  procedure TfMain.OperationsListChange(Sender: TObject);
  var i: Integer;
  begin
     // Invalidate all views except current
    for i := 0 to FPhoA.Views.Count-1 do
      if i<>ViewIndex then FPhoA.Views[i].UnprocessGroups;
  end;

  procedure TfMain.OperationsStatusChange(Sender: TObject);
  var iMaxCnt: Integer;
  begin
    if tsUpdating in tvGroups.TreeStates then Exit;
     // Ограничиваем количество операций в буфере
    iMaxCnt := SettingValueInt(ISettingID_Browse_MaxUndoCount);
    with FOperations do
      while Count>iMaxCnt do Delete(0);
    EnableActions;
  end;

  procedure TfMain.PerformOperation(Op: TPhoaOperation);
  var
    IFlags: TUndoInvalidationFlags;
    GetOpGroupNode_Cache, GetOpParentGroupNode_Cache: PVirtualNode;

     // Получает узел в tvGroups, соответствующий группе GroupAbsIdx операции, кэшируя результат
    function GetOpGroupNode: PVirtualNode;
    begin
      if GetOpGroupNode_Cache=nil then GetOpGroupNode_Cache := FindGroupNodeByID(Op.OpGroupID);
      Result := GetOpGroupNode_Cache;
      Assert(Result<>nil, 'Failed to locate Operation Group Node in TfMainPerformOperation()');
    end;

     // Получает узел в tvGroups, соответствующий группе ParentGroupAbsIdx операции, кэшируя результат
    function GetOpParentGroupNode: PVirtualNode;
    begin
      if GetOpParentGroupNode_Cache=nil then GetOpParentGroupNode_Cache := FindGroupNodeByID(Op.OpParentGroupID);
      Result := GetOpParentGroupNode_Cache;
      Assert(Result<>nil, 'Failed to locate Operation Parent Group Node in TfMainPerformOperation()');
    end;

  begin
    ResetMode;
    tvGroups.BeginUpdate;
    try
      IFlags := Op.InvalidationFlags;
       // Initialize cache
      GetOpGroupNode_Cache       := nil;
      GetOpParentGroupNode_Cache := nil;
       // Проверяем флаги требуемых обновлений
       // -- Переинициализация родителя
      if uifXReinitParent in IFlags then tvGroups.ReinitNode(GetOpParentGroupNode, uifXReinitRecursive in IFlags);
       // -- Переинициализация братьев
      if uifXReinitSiblings in IFlags then tvGroups.ReinitChildren(GetOpParentGroupNode, uifXReinitRecursive in IFlags);
       // -- Invalidate узла
      if uifXInvalidateNode in IFlags then tvGroups.InvalidateNode(GetOpGroupNode);
       // -- Invalidate дерева
      if uifXInvalidateTree in IFlags then tvGroups.Invalidate;
       // Уничтожаем результаты поиска
      DisplaySearchResults(True, False);
    finally
      tvGroups.EndUpdate;
    end;
     // Редактирование текста узла, соответствующего группе операции: выполняем после снятия блокировки перерисовки
    if uifXEditGroup in IFlags then begin
      tvGroups.Selected[GetOpGroupNode] := True;
      tvGroups.FocusedNode := GetOpGroupNode;
      tvGroups.EditNode(GetOpGroupNode, -1);
    end;
     // Обновляем Viewer (после возможной смены выделения из-за редактирования текста узла)
    RefreshViewer;
  end;

  procedure TfMain.pmGroupsPopup(Sender: TObject);
  begin
    if not FGroupsPopupToolsValidated then begin
      DoEnableTools(giTools_GroupsMenu);
      FGroupsPopupToolsValidated := True;
    end;
  end;

  procedure TfMain.pmPicsPopup(Sender: TObject);
  begin
    if not FPicsPopupToolsValidated then begin
      DoEnableTools(giTools_PicsMenu);
      FPicsPopupToolsValidated := True;
    end;
  end;

  procedure TfMain.ProcessCommandLine;
  var
    sPhoaFile: String;
    CmdLine: TPhoaCommandLine;
    ImgViewInitFlags: TImgViewInitFlags;

     // Выбирает в качестве текущего заданное представление, если sViewName<>''
    procedure SelectViewByName(const sViewName: String);
    var idx: Integer;
    begin
      if sViewName<>'' then begin
        idx := FPhoA.Views.IndexOfName(sViewName);
        if idx>=0 then ViewIndex := idx;
      end;
    end;

     // Выбирает в качестве текущей заданную группу в дереве, если sGroupPath<>''
    procedure SelectGroupByPath(const sGroupPath: String);
    begin
      if sGroupPath<>'' then CurGroup := CurRootGroup.GroupByPath[sGroupPath];
    end;

     // Выбирает изображение с заданным ID
    procedure SelectPicByID(iID: Integer);
    begin
      if (iID>0) and (CurGroup<>nil) then begin
        Viewer.ItemIndex := CurGroup.PicIDs.IndexOf(iID);
        Viewer.ScrollIntoView;
      end;
    end;

  begin
     // Разбираем параметры командной строки
    CmdLine := TPhoaCommandLine.Create;
    try
       // Если указан файл - загружаем его
      if clkOpenPhoa in CmdLine.Keys then begin
        sPhoaFile := CmdLine.KeyValues[clkOpenPhoa];
        ShowProgressInfo('SMsg_LoadingPhoa', [ExtractFileName(sPhoaFile)]);
        DoLoad(sPhoaFile);
         // -- Если указано представление - выбираем его
        if clkSelectView  in CmdLine.Keys then SelectViewByName(CmdLine.KeyValues[clkSelectView]);
         // -- Если указана группа - ищем и выделяем её
        if clkSelectGroup in CmdLine.Keys then SelectGroupByPath(CmdLine.KeyValues[clkSelectGroup]);
         // -- Если указан ID изображения - ищем и выделяем его
        if clkSelectPicID in CmdLine.Keys then SelectPicByID(StrToIntDef(CmdLine.KeyValues[clkSelectPicID], 0));
         // -- Если указан режим просмотра, готовим инициализационные флаги
        if clkViewMode in CmdLine.Keys then begin
          ImgViewInitFlags := [];
           // ---- Просмотр слайдов
          if clkSlideShow   in CmdLine.Keys then Include(ImgViewInitFlags, ivifSlideShow);
           // ---- Полноэкранный режим
          if clkFullScreen  in CmdLine.Keys then
            if CmdLine.KeyValues[clkFullScreen]='0' then
              Include(ImgViewInitFlags, ivifForceWindow)
            else
              Include(ImgViewInitFlags, ivifForceFullscreen);
           // ---- Отправляем отложенное сообщение о необходимости входа в режим просмотра
          PostMessage(Handle, WM_STARTVIEWMODE, Byte(ImgViewInitFlags), 0);
        end;
       // Иначе загружаем список представлений и выбираем "Группы изображений"
      end else
        LoadViewList(-1);
    finally
      CmdLine.Free;
    end;
    EnableActions;
  end;

  procedure TfMain.RefreshViewer;
  begin
    Viewer.SetCurrentGroup(CurGroup);
  end;

  procedure TfMain.ResetMode;
  begin
     // Завершаем inplace-редактирование текста узла в дереве групп
    tvGroups.EndEditNode;
  end;

  procedure TfMain.SetCurGroup(Value: TPhoaGroup);
  var n: PVirtualNode;
  begin
     // Перебираем узлы, пока не найдём
    if Value<>nil then begin
      n := tvGroups.GetFirst;
      while (n<>nil) and (PPhoaGroup(tvGroups.GetNodeData(n))^<>Value) do n := tvGroups.GetNext(n);
    end else
      n := nil;
     // Активизируем
    ActivateVTNode(tvGroups, n);
  end;

  procedure TfMain.SetFileName(const Value: String);
  begin
    FPhoA.FileName := Value;
  end;

  procedure TfMain.SetGroupExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var p: PPhoaGroup;
  begin
    if tsUpdating in tvGroups.TreeStates then Exit;
    p := Sender.GetNodeData(Node);
    if (p<>nil) then p^.Expanded := Sender.Expanded[Node];
  end;

  procedure TfMain.SetPhoaViewClick(Sender: TObject);
  begin
    ViewIndex := TComponent(Sender).Tag-1;
  end;

  procedure TfMain.SetViewIndex(Value: Integer);
  var i: Integer;
  begin
    FViewIndex := Value;
     // Настраиваем птицу в меню представлений
    iPhoaView_SetDefault.Checked := Value<0;
    for i := 0 to gipmPhoaViewViews.Count-1 do gipmPhoaViewViews[i].Checked := i=Value;
     // Перегружаем дерево папок
    LoadGroupTree;
  end;

  procedure TfMain.ShowProgressInfo(const sConstName: String; const aParams: array of const);
  begin
    if ProgressWnd<>nil then ProgressWnd.DisplayStage(ConstVal(sConstName, aParams));
  end;

  procedure TfMain.StartViewMode(InitFlags: TImgViewInitFlags);
  begin
    if (CurGroup<>nil) and (Viewer.ItemIndex>=0) then
      ViewImage(InitFlags, CurGroup, FPhoA, Viewer.ItemIndex, FOperations, ViewIndex<0);
  end;

  procedure TfMain.ToolItemClick(Sender: TObject);
  var PicLinks: TPhoaPicLinks;
  begin
     // Создаём массив ссылок на изображения
    PicLinks := GetSelectedPicLinks;
    try
       // Выполняем инструмент
      (RootSetting.Settings[ISettingID_Tools][TComponent(Sender).Tag] as TPhoaToolSetting).Execute(PicLinks);
    finally
      PicLinks.Free;
    end;
  end;

  procedure TfMain.tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
  begin
    if GetNodeKind(tvGroups, Node) in [gnkPhoA, gnkView] then begin
      ItemColor := clBtnFace;
      EraseAction := eaColor;
    end;
  end;

  procedure TfMain.tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    RefreshViewer;
  end;

  procedure TfMain.tvGroupsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var p: TPoint;
  begin
    ResetMode;
    if GetNodeKind(tvGroups, Node) in [gnkPhoA, gnkView] then begin
      with Sender.GetDisplayRect(Node, -1, False) do p := Sender.ClientToScreen(Point(Left, Bottom));
      pmPhoaView.Popup(p.x, p.y);
    end;
  end;

  procedure TfMain.tvGroupsCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  begin
     // Нельзя свёртывать узлы фотоальбома и результатов поиска
    Allowed := Sender.NodeParent[Node]<>nil;
  end;

  procedure TfMain.tvGroupsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
  begin
    Sender.NodeHeight[Node] := 20;
    EditLink := TStringEditLink.Create;
  end;

  procedure TfMain.tvGroupsDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
  begin
     // Перетаскивать можно только при отображении групп и только сами группы
    Allowed := (ViewIndex<0) and (Sender.NodeParent[Node]<>nil);
  end;

  procedure TfMain.tvGroupsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
  var
    nSrc, nTgt: PVirtualNode;
    gTgt: TPhoaGroup;
    AM: TVTNodeAttachMode;
    iNewIndex, iCnt, iCntBefore: Integer;
    bCopy: Boolean;
  begin
    nSrc := Sender.FocusedNode;
    nTgt := Sender.DropTargetNode;
     // Перетаскивание группы
    if Sender=Source then begin
       // Вычисляем и помещаем в nTgt нового родителя, в iNewIndex - новый индекс в родителе, в AM - режим перемещения
      case Mode of
        dmAbove: begin
          iNewIndex := nTgt.Index;
          nTgt := nTgt.Parent;
          AM := amInsertBefore;
        end;
        dmBelow: begin
          iNewIndex := nTgt.Index+1;
          nTgt := nTgt.Parent;
          AM := amInsertAfter;
        end;
        else {dmOnNode} begin
          iNewIndex := -1;
          AM := amAddChildLast;
        end;
      end;
       // Если перемещаем ближе к концу среди детей того же родителя, уменьшаем индекс на 1
      if (Mode in [dmAbove, dmBelow]) and (nTgt=nSrc.Parent) and (iNewIndex>Integer(nSrc.Index)) then Dec(iNewIndex);
       // Перемещаем
      PerformOperation(
        TPhoaOp_GroupDragAndDrop.Create(
          FOperations,
          FPhoA,
          PPhoaGroup(Sender.GetNodeData(nSrc))^, // Group being dragged
          PPhoaGroup(Sender.GetNodeData(nTgt))^, // New parent group
          iNewIndex));
      Sender.MoveTo(nSrc, Sender.DropTargetNode, AM, False);
      Sender.FullyVisible[nSrc] := True;
      Effect := DROPEFFECT_NONE;
     // Перетаскивание изображений
    end else if Source=Viewer then begin
      bCopy := (GetKeyState(VK_CONTROL) and $80<>0) or (GetNodeKind(tvGroups, nSrc)=gnkSearch);
      gTgt := PPhoaGroup(Sender.GetNodeData(nTgt))^;
      iCnt := Viewer.SelCount;
      iCntBefore := gTgt.PicIDs.Count;
      PerformOperation(TPhoaMultiOp_PicDragAndDropToGroup.Create(FOperations, FPhoA, CurGroup, gTgt, PicArrayToIDArray(Viewer.GetSelectedPicArray), bCopy));
      PhoaInfo(
        False,
        iif(bCopy, 'SNotify_DragCopy', 'SNotify_DragMove'),
        [iCnt, gTgt.PicIDs.Count-iCntBefore, iCnt-(gTgt.PicIDs.Count-iCntBefore)],
        iif(bCopy, ISettingID_Dlgs_NotifyDragCopy, ISettingID_Dlgs_NotifyDragMove));
    end;
  end;

  procedure TfMain.tvGroupsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
  const aPicCur: Array[Boolean] of TCursor = (crDragMove, crDragCopy);
  var
    nSrc, nTgt: PVirtualNode;
    gnkSrc, gnkTgt: TGroupNodeKind;
  begin
    Accept := False;
    nSrc := Sender.FocusedNode;
    nTgt := Sender.DropTargetNode;
    gnkSrc := GetNodeKind(tvGroups, nSrc);
    gnkTgt := GetNodeKind(tvGroups, nTgt);
     // Перетаскивание группы
    if Sender=Source then begin
      Effect := DROPEFFECT_MOVE;
      if (gnkTgt<>gnkSearch) and (Mode in [dmAbove, dmOnNode, dmBelow]) then begin
        case Mode of
           // НАД узлом - нельзя вставлять над фотоальбомом и над следующим за nSrc узлом
          dmAbove:  Accept := (gnkTgt<>gnkPhoA) and ((nSrc.Parent<>nTgt.Parent) or (nSrc.Index<>nTgt.Index-1));
           // НА узле - нельзя таскать в родителя исходного узла
          dmOnNode: Accept := nSrc.Parent<>nTgt;
           // ПОД узлом - нельзя вставлять под фотоальбомом и под предыдущим перед nSrc узлом
          dmBelow:  Accept := (gnkTgt<>gnkPhoA) and ((nSrc.Parent<>nTgt.Parent) or (nSrc.Index<>nTgt.Index+1));
        end;
         // nTgt не может быть ребёнком nSrc
        while Accept and (nTgt<>nil) do begin
          Accept := nSrc<>nTgt;
          nTgt := nTgt.Parent;
        end;
      end;
     // Перетаскивание изображений
    end else if Source=Viewer then begin
      Accept :=
        (Mode=dmOnNode) and
        (Viewer.SelCount>0) and
        (nTgt<>nil) and
        (nTgt<>nSrc) and
        (gnkTgt<>gnkSearch);
      if Accept then Viewer.DragCursor := aPicCur[(gnkSrc=gnkSearch) or (ssCtrl in Shift)];
    end;
  end;

  procedure TfMain.tvGroupsEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
  begin
    Sender.NodeHeight[Sender.FocusedNode] := 16;
  end;

  procedure TfMain.tvGroupsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  begin
    Sender.NodeHeight[Node] := 16;
  end;

  procedure TfMain.tvGroupsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
  begin
    Allowed := GetNodeKind(Sender, Node) in [gnkView, gnkPhoaGroup];
  end;

  procedure TfMain.tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var s: String;
  begin
    case GetNodeKind(Sender, Node) of
      gnkPhoA:      s := FPhoA.Description;
      gnkPhoaGroup: s := PPhoaGroup(Sender.GetNodeData(Node))^.GetPropStrs(FGroupTreeHintProps, ': ', S_CRLF);
      else          s := '';
    end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TfMain.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  const
    aiImgIdx: Array[TGroupNodeKind] of Integer = (
      -1,             // gnkNone
      iiPhoA,         // gnkPhoA
      iiView,         // gnkView
      iiFolderSearch, // gnkSearch
      iiFolder,       // gnkPhoaGroup
      iiFolder);      // gnkViewGroup
  begin
    if Kind in [ikNormal, ikSelected] then ImageIndex := aiImgIdx[GetNodeKind(Sender, Node)];
  end;

  procedure TfMain.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    p: PPhoaGroup;
    s: String;
  begin
    p := Sender.GetNodeData(Node);
    s := '';
     // Static text
    case TextType of
      ttNormal:
        case GetNodeKind(Sender, Node) of
          gnkPhoA:        s := ConstVal('SPhotoAlbumNode');
          gnkView:        s := FPhoA.Views[ViewIndex].Name;
          gnkSearch:      s := ConstVal('SSearchResultsNode');
          gnkPhoaGroup,
            gnkViewGroup: s := p^.Text;
        end;
      ttStatic: if p^.PicIDs.Count>0 then s := Format('(%d)', [p^.PicIDs.Count]);
    end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TfMain.tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var p, pp: PPhoaGroup;
  begin
    p := Sender.GetNodeData(Node);
     // Узел обычной группы
    if ParentNode<>nil then begin
      pp := Sender.GetNodeData(ParentNode);
      p^ := pp^.Groups[Node.Index];
     // Узел фотоальбома/представления
    end else if Node.Index=0 then begin
      p^ := CurRootGroup;
      Node.CheckType := ctButton;
     // Узел результатов поиска
    end else
      p^ := FSearchResults;
    Sender.ChildCount[Node] := p^.Groups.Count;
     // Разворачиваем корневой узел или если группа развёрнута
    if (ParentNode=nil) or p^.Expanded then Include(InitialStates, ivsExpanded) else Sender.Expanded[Node] := False;
  end;

  procedure TfMain.tvGroupsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
  begin
    case GetNodeKind(Sender, Node) of
      gnkView:      PerformOperation(TPhoaOp_ViewEdit.Create(FOperations, FPhoA.Views[ViewIndex], Self, UnicodetoAnsiCP(NewText, cMainCodePage), nil, nil));
      gnkPhoaGroup: PerformOperation(TPhoaOp_GroupRename.Create(FOperations, FPhoA, CurGroup, UnicodetoAnsiCP(NewText, cMainCodePage)));
    end;
  end;

  procedure TfMain.tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    if TextType=ttStatic then TargetCanvas.Font.Color := clGrayText
    else if Sender.NodeParent[Node]=nil then TargetCanvas.Font.Style := [fsBold];
  end;

  procedure TfMain.ulToolbarUndoChange(Sender: TObject);
  begin
    tbxlToolbarUndo.UpdateCaption(ConstVal('SUndoOperationCount', [ulToolbarUndo.ItemIndex+1]));
  end;

  procedure TfMain.ulToolbarUndoClick(Sender: TObject);
  begin
    UndoOperations(FOperations.Count-ulToolbarUndo.ItemIndex-1);
  end;

  procedure TfMain.UndoOperation(Op: TPhoaOperation);
  var
    IFlags: TUndoInvalidationFlags;
    OpGroupNode, OpParentGroupNode: PVirtualNode;
  begin
     // Получаем данные до уничтожения объекта операции
    IFlags := Op.InvalidationFlags;
    OpGroupNode       := nil;
    OpParentGroupNode := nil;
    if uifUReinitParent   in IFlags then OpParentGroupNode := FindGroupNodeByID(Op.OpParentGroupID);
    if uifUInvalidateNode in IFlags then OpGroupNode       := FindGroupNodeByID(Op.OpGroupID);
     // Откатываем (и уничтожаем) операцию
    Op.Undo;
     // Проверяем флаги требуемых обновлений
     // -- Переинициализация всего дерева
    if uifUReinitAll      in IFlags then tvGroups.ReinitChildren(nil, True);
     // -- Переинициализация родителя
    if uifUReinitParent   in IFlags then tvGroups.ReinitNode(OpParentGroupNode, uifUReinitRecursive in IFlags);
     // -- Invalidate узла
    if uifUInvalidateNode in IFlags then tvGroups.InvalidateNode(OpGroupNode);
     // -- Invalidate дерева
    if uifUInvalidateTree in IFlags then tvGroups.Invalidate;
  end;

  procedure TfMain.UndoOperations(Index: Integer);
  var i: Integer;
  begin
    tvGroups.CancelEditNode;
    tvGroups.BeginUpdate;
    try
       // Крутим цикл с конца до указанного индекса
      for i := FOperations.Count-1 downto Index do UndoOperation(FOperations[i]);
       // Уничтожаем результаты поиска
      DisplaySearchResults(True, False);
    finally
      tvGroups.EndUpdate;
    end;
    RefreshViewer;
  end;

  procedure TfMain.ViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
  begin
    PerformOperation(
      TPhoaOp_PicDragAndDropInsideGroup.Create(
        FOperations,
        FPhoA,
        CurGroup,
        PicArrayToIDArray(Viewer.GetSelectedPicArray),
        Viewer.DropTargetIndex));
  end;

  procedure TfMain.ViewerSelectionChange(Sender: TObject);
  begin
    EnableActions;
  end;

  procedure TfMain.WMChangeCBChain(var Msg: TWMChangeCBChain);
  begin
     // Реализуем стандартное поведение, согласно Platform SDK
    with Msg do
      if Remove=FHNextClipbrdViewer then begin
        Result := 0;
        FHNextClipbrdViewer := Next;
      end else
        Result := SendMessage(FHNextClipbrdViewer, WM_CHANGECBCHAIN, Remove, Next);
  end;

  procedure TfMain.WMDrawClipboard(var Msg: TWMDrawClipboard);
  begin
    EnableActions;
     // Invoke the next viewer in chain
    if FHNextClipbrdViewer<>0 then SendMessage(FHNextClipbrdViewer, WM_DRAWCLIPBOARD, 0, 0);
  end;

  procedure TfMain.WMHelp(var Msg: TWMHelp);
  begin
    ResetMode;
    HtmlHelpShowContents;
  end;

  procedure TfMain.WMStartViewMode(var Msg: TWMStartViewMode);
  begin
    StartViewMode(Msg.InitFlags);
  end;

end.
