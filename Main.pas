//**********************************************************************************************************************
//  $Id: Main.pas,v 1.50 2004-10-12 12:38:09 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit Main;

interface

uses
   // GR32 must follow GraphicEx because of naming conflict between stretch filter constants
  Windows, Messages, SysUtils, Variants, Classes, Graphics, GraphicEx, GR32, Controls, Forms, Dialogs,
  ActiveX, XPMan,
  phIntf, phMutableIntf, phNativeIntf, phObj, phGUIObj, phOps,
  ConsVars,
  DKLang, ImgList, TB2Item, Placemnt, TB2MRU, TBXExtItems, Menus,
  TBX, ActnList, TBXStatusBars, VirtualTrees, TBXDkPanels, TBXLists,
  TB2Dock, TB2Toolbar;

type
  TfMain = class(TForm, IPhoaViews)
    aAbout: TAction;
    aCopy: TAction;
    aCut: TAction;
    aDelete: TAction;
    aEdit: TAction;
    aExit: TAction;
    aFileOperations: TAction;
    aFind: TAction;
    aFlatMode: TAction;
    aHelpCheckUpdates: TAction;
    aHelpContents: TAction;
    aHelpFAQ: TAction;
    aHelpProductWebsite: TAction;
    aHelpSupport: TAction;
    aHelpVendorWebsite: TAction;
    aIniLoadSettings: TAction;
    aIniSaveSettings: TAction;
    alMain: TActionList;
    aNew: TAction;
    aNewGroup: TAction;
    aNewPic: TAction;
    aOpen: TAction;
    aPaste: TAction;
    aPhoaView_Delete: TAction;
    aPhoaView_Edit: TAction;
    aPhoaView_MakeGroup: TAction;
    aPhoaView_New: TAction;
    aPicOps: TAction;
    aRemoveSearchResults: TAction;
    aSave: TAction;
    aSaveAs: TAction;
    aSelectAll: TAction;
    aSelectNone: TAction;
    aSettings: TAction;
    aSortPics: TAction;
    aStats: TAction;
    aUndo: TAction;
    aView: TAction;
    bAbout: TTBXItem;
    bCopy: TTBXItem;
    bCut: TTBXItem;
    bDelete: TTBXItem;
    bEdit: TTBXItem;
    bExit: TTBXItem;
    bFind: TTBXItem;
    bHelpContents: TTBXItem;
    bNew: TTBXItem;
    bNewGroup: TTBXItem;
    bNewPic: TTBXItem;
    bOpen: TTBXSubmenuItem;
    bOpenMRU: TTBXMRUListItem;
    bPaste: TTBXItem;
    bSave: TTBXItem;
    bSaveAs: TTBXItem;
    bSettings: TTBXItem;
    bStats: TTBXItem;
    bUndo: TTBXSubmenuItem;
    bView: TTBXItem;
    dkBottom: TTBXDock;
    dklcMain: TDKLanguageController;
    dkLeft: TTBXDock;
    dkRight: TTBXDock;
    dkTop: TTBXDock;
    dpGroups: TTBXDockablePanel;
    fpMain: TFormPlacement;
    gipmPhoaView: TTBGroupItem;
    gipmPhoaViewViews: TTBGroupItem;
    gismViewViews: TTBGroupItem;
    giTools_GroupsMenu: TTBGroupItem;
    giTools_PicsMenu: TTBGroupItem;
    giTools_ToolsMenu: TTBGroupItem;
    iAbout: TTBXItem;
    iCopy: TTBXItem;
    iCut: TTBXItem;
    iDelete: TTBXItem;
    iEdit: TTBXItem;
    iEditSep1: TTBXSeparatorItem;
    iEditSep2: TTBXSeparatorItem;
    iEditSep3: TTBXSeparatorItem;
    iEditSep4: TTBXSeparatorItem;
    iExit: TTBXItem;
    iFileOperations: TTBXItem;
    iFileSep1: TTBXSeparatorItem;
    iFileSep2: TTBXSeparatorItem;
    iFileSep3: TTBXSeparatorItem;
    iFind: TTBXItem;
    iFlatMode: TTBXItem;
    iHelpCheckUpdates: TTBXItem;
    iHelpContents: TTBXItem;
    iHelpFAQ: TTBXItem;
    iHelpProductWebsite: TTBXItem;
    iHelpSupport: TTBXItem;
    iHelpVendorWebsite: TTBXItem;
    iIniLoadSettings: TTBXItem;
    iIniSaveSettings: TTBXItem;
    ilActionsLarge: TImageList;
    ilActionsMiddle: TImageList;
    ilActionsSmall: TTBImageList;
    iNew: TTBXItem;
    iNewGroup: TTBXItem;
    iNewPic: TTBXItem;
    iOpen: TTBXItem;
    iPaste: TTBXItem;
    iPhoaView_Delete: TTBXItem;
    iPhoaView_Edit: TTBXItem;
    iPhoaView_MakeGroup: TTBXItem;
    iPhoaView_New: TTBXItem;
    iPhoaView_SetDefault: TTBXItem;
    iPhoaViewSep1: TTBXSeparatorItem;
    iPhoaViewSep2: TTBXSeparatorItem;
    iPhoaViewSep3: TTBXSeparatorItem;
    iPicOps: TTBXItem;
    ipmGroupsDelete: TTBXItem;
    ipmGroupsEdit: TTBXItem;
    ipmGroupsFileOperations: TTBXItem;
    ipmGroupsNewGroup: TTBXItem;
    ipmGroupsNewPic: TTBXItem;
    ipmGroupsPicOps: TTBXItem;
    ipmGroupsSep1: TTBXSeparatorItem;
    ipmGroupsSep2: TTBXSeparatorItem;
    ipmGroupsSep3: TTBXSeparatorItem;
    ipmGroupsSortPics: TTBXItem;
    ipmGroupsStats: TTBXItem;
    ipmPicsCopy: TTBXItem;
    ipmPicsCut: TTBXItem;
    ipmPicsDelete: TTBXItem;
    ipmPicsEdit: TTBXItem;
    ipmPicsFileOperations: TTBXItem;
    ipmPicsNewPic: TTBXItem;
    ipmPicsPaste: TTBXItem;
    ipmPicsSelectAll: TTBXItem;
    ipmPicsSelectNone: TTBXItem;
    ipmPicsSep1: TTBXSeparatorItem;
    ipmPicsSep2: TTBXSeparatorItem;
    ipmPicsSep3: TTBXSeparatorItem;
    ipmPicsSep4: TTBXSeparatorItem;
    ipmPicsView: TTBXItem;
    iRemoveSearchResults: TTBXItem;
    iSave: TTBXItem;
    iSaveAs: TTBXItem;
    iSelectAll: TTBXItem;
    iSelectNone: TTBXItem;
    iSettings: TTBXItem;
    iSortPics: TTBXItem;
    iToggleStatusbar: TTBXVisibilityToggleItem;
    iToggleToolbar: TTBXVisibilityToggleItem;
    iToolsSep1: TTBXSeparatorItem;
    iToolsSep2: TTBXSeparatorItem;
    iUndo: TTBXItem;
    iView: TTBXItem;
    mruOpen: TTBXMRUList;
    pmGroups: TTBXPopupMenu;
    pmPhoaView: TTBXPopupMenu;
    pmPics: TTBXPopupMenu;
    pmView: TTBXPopupMenu;
    sbarMain: TTBXStatusBar;
    smEdit: TTBXSubmenuItem;
    smFile: TTBXSubmenuItem;
    smFileMRU: TTBXMRUListItem;
    smHelp: TTBXSubmenuItem;
    smHelpInternet: TTBXSubmenuItem;
    smTools: TTBXSubmenuItem;
    smUndoHistory: TTBXSubmenuItem;
    smView: TTBXSubmenuItem;
    tbMain: TTBXToolbar;
    tbMenu: TTBXToolbar;
    tbSep1: TTBXSeparatorItem;
    tbSep2: TTBXSeparatorItem;
    tbSep3: TTBXSeparatorItem;
    tbSep4: TTBXSeparatorItem;
    tbSepHelpWebsite: TTBXSeparatorItem;
    tbViewSep1: TTBXSeparatorItem;
    tbxlToolbarUndo: TTBXLabelItem;
    tvGroups: TVirtualStringTree;
    ulToolbarUndo: TTBXUndoList;
    procedure aaAbout(Sender: TObject);
    procedure aaCopy(Sender: TObject);
    procedure aaCut(Sender: TObject);
    procedure aaDelete(Sender: TObject);
    procedure aaEdit(Sender: TObject);
    procedure aaExit(Sender: TObject);
    procedure aaFileOperations(Sender: TObject);
    procedure aaFind(Sender: TObject);
    procedure aaFlatMode(Sender: TObject);
    procedure aaHelpCheckUpdates(Sender: TObject);
    procedure aaHelpContents(Sender: TObject);
    procedure aaHelpFAQ(Sender: TObject);
    procedure aaHelpProductWebsite(Sender: TObject);
    procedure aaHelpSupport(Sender: TObject);
    procedure aaHelpVendorWebsite(Sender: TObject);
    procedure aaIniLoadSettings(Sender: TObject);
    procedure aaIniSaveSettings(Sender: TObject);
    procedure aaNew(Sender: TObject);
    procedure aaNewGroup(Sender: TObject);
    procedure aaNewPic(Sender: TObject);
    procedure aaOpen(Sender: TObject);
    procedure aaPaste(Sender: TObject);
    procedure aaPhoaView_Delete(Sender: TObject);
    procedure aaPhoaView_Edit(Sender: TObject);
    procedure aaPhoaView_MakeGroup(Sender: TObject);
    procedure aaPhoaView_New(Sender: TObject);
    procedure aaPicOps(Sender: TObject);
    procedure aaRemoveSearchResults(Sender: TObject);
    procedure aaSave(Sender: TObject);
    procedure aaSaveAs(Sender: TObject);
    procedure aaSelectAll(Sender: TObject);
    procedure aaSelectNone(Sender: TObject);
    procedure aaSettings(Sender: TObject);
    procedure aaSortPics(Sender: TObject);
    procedure aaStats(Sender: TObject);
    procedure aaUndo(Sender: TObject);
    procedure aaView(Sender: TObject);
    procedure bUndoPopup(Sender: TTBCustomItem; FromLink: Boolean);
    procedure dklcMainLanguageChanged(Sender: TObject);
    procedure dklcMainLanguageChanging(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure fpMainRestorePlacement(Sender: TObject);
    procedure fpMainSavePlacement(Sender: TObject);
    procedure mruOpenClick(Sender: TObject; const Filename: String);
    procedure pmGroupsPopup(Sender: TObject);
    procedure pmPicsPopup(Sender: TObject);
    procedure SetGroupExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SetPhoaViewClick(Sender: TObject);
    procedure tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure tvGroupsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure tvGroupsDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure tvGroupsDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure tvGroupsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure tvGroupsEditCancelled(Sender: TBaseVirtualTree; Column: TColumnIndex);
    procedure tvGroupsEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure tvGroupsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvGroupsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure ulToolbarUndoChange(Sender: TObject);
    procedure ulToolbarUndoClick(Sender: TObject);
  private
     // Активный проект
    FProject: IPhotoAlbumProject;
     // Просматриваемые в настоящий момент изображения (с учётом режима Flat)
    FViewedPics: IPhotoAlbumPicList;
     // Узел результатов поиска
    FSearchNode: PVirtualNode;
     // Список изображений - результаты поиска
    FSearchResults: IPhotoAlbumPicGroup;
     // Handle окна - следующего просмотрщика изменения содержимого clipboard
    FHNextClipbrdViewer: HWND;
     // Стек операций для отмены
    FUndo: TPhoaUndo;
     // Набор свойств, которые требуется отображать в подсказках дерева групп
    FGroupTreeHintProps: TGroupProperties;
     // Флаги, указывающие на то, что инструменты popup-меню групп и вьюера прошли проверку на доступность текущим
     //   выделенным изображениям
    FGroupsPopupToolsValidated: Boolean;
    FPicsPopupToolsValidated: Boolean;
     // Флаг того, что инициализация формы окончена
    FInitialized: Boolean;
     // Счётчик вызовов BeginOperation/EndOperation
    FOpLockCounter: Integer;
     // Сохранённый на время выполнения операции ID последней выделенной группы
    FSavedGroupID: Integer;
     // Сохранённые на время выполнения операции параметры отображения Viewer
    FViewerDisplayData: IThumbnailViewerDisplayData;
     // Prop storage
    FViewer: TThumbnailViewer;
    FViewIndex: Integer;
     // Применяет параметры настройки языка
    procedure ApplyLanguage;
     // Применяет параметры настройки инструментов
    procedure ApplyTools;
     // Проверяет необходимость сохранения файла фотоальбома. Возвращает True, если можно продолжать
    function  CheckSave: Boolean;
     // Загружает иерархию групп из фотоальбома в tvGroups
    procedure LoadGroupTree;
     // Загрузка/сохранение фотоальбома в файле
    procedure DoLoad(const sFileName: String);
    procedure DoSave(const sFileName: String; iRevisionNumber: Integer);
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
     //   иначе возвращает nil
    function  GetSelectedPics: IPhoaPicList;
     // Отображает результаты поиска. Если bForceRemove=True, удаляет узел результатов, иначе, при bDoSelectNode=True,
     //   выделяет узел
    procedure DisplaySearchResults(bForceRemove, bDoSelectNode: Boolean);
     // Обновляет состояние aFlatMode
    procedure UpdateFlatModeAction;
     // Возвращает вид узла
    function  GetNodeKind(Tree: TBaseVirtualTree; Node: PVirtualNode): TGroupNodeKind;
     // Viewer events
    procedure ViewerSelectionChange(Sender: TObject);
    procedure ViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
     // Application events
    procedure AppHint(Sender: TObject);
    procedure AppException(Sender: TObject; E: Exception);
    procedure AppIdle(Sender: TObject; var Done: Boolean);
     // Откатывает все операции с последней до Index и обновляет визуальные объекты согласно флагам операций
    procedure UndoOperations(Index: Integer);
     // Откатывает индивидуальную операцию (for internal use only)
    procedure UndoOperation(Op: TPhoaOperation);
     // События списка операций
    procedure OperationsStatusChange(Sender: TObject);
    procedure OperationsListChange(Sender: TObject);
     // Событие клика на пункте инструмента
    procedure ToolItemClick(Sender: TObject);
     // Вводит в режим просмотра, начиная с текущего изображения. InitFlags задаёт опции инициализации
    procedure StartViewMode(InitFlags: TImgViewInitFlags);
     // Отменяет все "неустойчивые" режимы и возвращается в основной режим просмотра
    procedure ResetMode;
     // Находит и возвращает узел в tvGroups по ID группы; nil, если нет такого
    function  FindGroupNodeByID(iGroupID: Integer): PVirtualNode;
//!!!     // IPhoaViews
    function  GetViewIndex: Integer;
    procedure SetViewIndex(Value: Integer);
//    function  GetViews: TPhoaViews;
    procedure LoadViewList(idxSelect: Integer);
     // Message handlers
    procedure WMChangeCBChain(var Msg: TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard(var Msg: TWMDrawClipboard); message WM_DRAWCLIPBOARD;
    procedure CMFocusChanged(var Msg: TCMFocusChanged);   message CM_FOCUSCHANGED;
    procedure WMHelp(var Msg: TWMHelp);                   message WM_HELP;
    procedure WMStartViewMode(var Msg: TWMStartViewMode); message WM_STARTVIEWMODE;
     // Property handlers
    procedure SetFileName(const Value: String);
    function  GetFileName: String;
    function  GetDisplayFileName: String;
    function  GetCurGroup: IPhotoAlbumPicGroup;
    procedure SetCurGroup(Value: IPhotoAlbumPicGroup);
    function  GetCurRootGroup: IPhotoAlbumPicGroup;
  public
    function  IsShortCut(var Message: TWMKey): Boolean; override;
     // Должна вызываться перед началом выполнения любой операции
    procedure BeginOperation;
     // Должна вызываться после выполнения операции. После последнего вызова обновляет отображаемые данные
    procedure EndOperation(Operation: TPhoaOperation);
     // Применяет параметры настройки
    procedure ApplySettings;
     // Обновляет Viewer
    procedure RefreshViewer;
     // Props
     // -- Имя текущего файла фотоальбома (пустая строка, если новый фотоальбом)
    property FileName: String read GetFileName write SetFileName;
     // -- Текущая выбранная группа в дереве
    property CurGroup: IPhotoAlbumPicGroup read GetCurGroup write SetCurGroup;
     // -- Текущая корневая группа в дереве (фотоальбома или представления)
    property CurRootGroup: IPhotoAlbumPicGroup read GetCurRootGroup;
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

   // Загружает ImageList из PNG-ресурса, если он ещё не загружен
  procedure MakeImagesLoaded(const sResourceName: String; Images: TCustomImageList);
  var
    PNG: TPNGGraphic;
    Bmp: TBitmap;
  begin
    if Images.Count=0 then begin
       // Загружаем картинки в PNG
      PNG := TPNGGraphic.Create;
      try
        PNG.LoadFromResourceName(HInstance, sResourceName);
         // Копируем в битмэп
        Bmp := TBitmap.Create;
        try
          Bmp.Assign(PNG);
           // Загружаем в ImageList
          Images.AddMasked(Bmp, clFuchsia);
        finally
          Bmp.Free;
        end;
      finally
        PNG.Free;
      end;
    end;
  end;

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
    TPhoaBaseOp_PicCopy.Create(Viewer.SelectedPics, TPicClipboardFormats(Byte(SettingValueInt(ISettingID_Gen_ClipFormats))));
  end;

  procedure TfMain.aaCut(Sender: TObject);
  var Operation: TPhoaOperation;
  begin
    TPhoaBaseOp_PicCopy.Create(Viewer.SelectedPics, TPicClipboardFormats(Byte(SettingValueInt(ISettingID_Gen_ClipFormats))));
    Operation := nil;
    BeginOperation;
    try
      Operation := TPhoaMultiOp_PicDelete.Create(FUndo, FProject, CurGroup, Viewer.SelectedPics);
    finally
      EndOperation(Operation);
    end;
  end;

  procedure TfMain.aaDelete(Sender: TObject);
  var Operation: TPhoaOperation;
  begin
    ResetMode;
    Operation := nil;
    if CurGroup<>nil then
       // Удаление группы
      if ActiveControl=tvGroups then begin
        if PhoaConfirm(False, 'SConfirm_DelGroup', ISettingID_Dlgs_ConfmDelGroup) then begin
          BeginOperation;
          try
            Operation := TPhoaOp_GroupDelete.Create(FUndo, FProject, CurGroup, True);
          finally
            EndOperation(Operation);
          end;
        end;
       // Удаление изображения
      end else if (ActiveControl=Viewer) and (Viewer.SelectedPics.Count>0) and PhoaConfirm(False, 'SConfirm_DelPics', ISettingID_Dlgs_ConfmDelPics) then begin
        BeginOperation;
        try
          Operation := TPhoaMultiOp_PicDelete.Create(FUndo, FProject, CurGroup, Viewer.SelectedPics);
        finally
          EndOperation(Operation);
        end;
      end;
  end;

  procedure TfMain.aaEdit(Sender: TObject);
  begin
    ResetMode;
     // Редактирование групп
    if ActiveControl=tvGroups then begin
      case GetNodeKind(tvGroups, tvGroups.FocusedNode) of
        gnkPhoA:      EditPhoA(FProject, FUndo);
        gnkView:      EditView(FProject.ViewsX[ViewIndex], FProject, FUndo);
        gnkPhoaGroup: EditPicGroup(FProject, CurGroup, FUndo);
      end;
     // Редактирование изображения
    end else if (ActiveControl=Viewer) and (Viewer.SelectedPics.Count>0) then
      EditPics(Viewer.SelectedPics, FProject, FUndo);
  end;

  procedure TfMain.aaExit(Sender: TObject);
  begin
    ResetMode;
    Close;
  end;

  procedure TfMain.aaFileOperations(Sender: TObject);
  var
    View: IPhotoAlbumView;
    bPhoaChanged: Boolean;
  begin
    ResetMode;
    if ViewIndex>=0 then View := FProject.ViewsX[ViewIndex] else View := nil;
    if DoFileOperations(FProject, CurGroup, View, Viewer.SelectedPics, ActiveControl=Viewer, bPhoaChanged) then
       // Если изменилось содержимое фотоальбома
      if bPhoaChanged then begin
         // Помечаем текущее состояние как изменённое без возможности отката
        FUndo.SetNonUndoable;
         // Перестраиваем представления
        FProject.Views.Invalidate;
         // Перегружаем дерево папок
        LoadGroupTree;
       // Иначе просто были внесены необратимые изменения (в файловую систему, но не в фотоальбом) - запрещаем откат
      end else
        FUndo.Clear;
  end;

  procedure TfMain.aaFind(Sender: TObject);
  begin
    ResetMode;
    if DoSearch(FProject, CurGroup, FSearchResults) then DisplaySearchResults(False, True);
  end;

  procedure TfMain.aaFlatMode(Sender: TObject);
  begin
    SetSettingValueBool(ISettingID_Browse_FlatMode, not SettingValueBool(ISettingID_Browse_FlatMode));
    UpdateFlatModeAction;
    RefreshViewer;
  end;

  procedure TfMain.aaHelpCheckUpdates(Sender: TObject);
  begin
    ResetMode;
    DKWeb.Open_VerCheck;
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

  procedure TfMain.aaHelpProductWebsite(Sender: TObject);
  begin
    ResetMode;
    DKWeb.Open_ViewInfo;
  end;

  procedure TfMain.aaHelpSupport(Sender: TObject);
  begin
    ResetMode;
    DKWeb.Open_Support;
  end;

  procedure TfMain.aaHelpVendorWebsite(Sender: TObject);
  begin
    ResetMode;
    DKWeb.Open_Index;
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
      FUndo.BeginUpdate;
      try
         // Стираем результаты поиска
        DisplaySearchResults(True, False);
         // Инициализируем фотоальбом и буфер отката
        FProject.New{(!!!FUndo)};
         // Загружаем группы изображений
        LoadViewList(-1);
      finally
        FUndo.EndUpdate;
      end;
    finally
      tvGroups.EndUpdate;
    end;
  end;

  procedure TfMain.aaNewGroup(Sender: TObject);
  var Operation: TPhoaOperation;
  begin
    Operation := nil;
    BeginOperation;
    try
      Operation := TPhoaOp_GroupNew.Create(FUndo, FProject, CurGroup);
    finally
      EndOperation(Operation);
    end;
  end;

  procedure TfMain.aaNewPic(Sender: TObject);
  begin
    ResetMode;
    AddFiles(FProject, PPhotoAlbumPicGroup(tvGroups.GetNodeData(tvGroups.FocusedNode))^, FUndo);
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
  var
    iCntBefore: Integer;
    Operation: TPhoaOperation;
  begin
    iCntBefore := CurGroup.Pics.Count;
    Operation := nil;
    BeginOperation;
    try
      Operation := TPhoaMultiOp_PicPaste.Create(FUndo, FProject, CurGroup);
    finally
      EndOperation(Operation);
    end;
    PhoaInfo(False, 'SNotify_Paste', [CurGroup.Pics.Count-iCntBefore], ISettingID_Dlgs_NotifyPaste);
  end;

  procedure TfMain.aaPhoaView_Delete(Sender: TObject);
  var Operation: TPhoaOperation;
  begin
    ResetMode;
    Operation := nil;
    if PhoaConfirm(False, 'SConfirm_DelView', ISettingID_Dlgs_ConfmDelView) then begin
      BeginOperation;
      try
        Operation := TPhoaOp_ViewDelete.Create(FUndo, Self);
      finally
        EndOperation(Operation);
      end;
    end;
  end;

  procedure TfMain.aaPhoaView_Edit(Sender: TObject);
  begin
    ResetMode;
    EditView(FProject.ViewsX[ViewIndex], FProject, FUndo);
  end;

  procedure TfMain.aaPhoaView_MakeGroup(Sender: TObject);
  begin
    ResetMode;
    MakeGroupFromView(FProject, FUndo, Self);
  end;

  procedure TfMain.aaPhoaView_New(Sender: TObject);
  begin
    ResetMode;
    EditView(nil, FProject, FUndo);
  end;

  procedure TfMain.aaPicOps(Sender: TObject);
  begin
    ResetMode;
    DoPicOps(FProject, FUndo, CurGroup, Viewer.SelectedPics);
  end;

  procedure TfMain.aaRemoveSearchResults(Sender: TObject);
  begin
    ResetMode;
    DisplaySearchResults(True, False);
  end;

  procedure TfMain.aaSave(Sender: TObject);
  begin
    ResetMode;
     // Если имя файла не задано, выполняем SaveAs. Иначе сохраняем файл фотоальбома
    if FileName='' then aSaveAs.Execute else DoSave(FProject.FileName, FProject.FileRevision);
  end;

  procedure TfMain.aaSaveAs(Sender: TObject);
  begin
    ResetMode;
    with TSaveDialog.Create(Self) do
      try
        DefaultExt  := SDefaultExt;
        Filter      := GetPhoaSaveFilter;
        FilterIndex := ValidRevisionIndex(GetIndexOfRevision(FProject.FileRevision))+1;
        Options     := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
        Title       := ConstVal('SDlgTitle_SavePhoa');
        FileName    := DisplayFileName;
        if Execute then DoSave(FileName, aPhFileRevisions[ValidRevisionIndex(FilterIndex-1)].iNumber);
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
    Viewer.ClearSelection;
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
    DoSortPics(FProject, CurGroup, FUndo, CurGroup=FSearchResults);
  end;

  procedure TfMain.aaStats(Sender: TObject);
  begin
    ResetMode;
    ShowProjectStats(FProject, CurGroup);
  end;

  procedure TfMain.aaUndo(Sender: TObject);
  begin
    ResetMode;
    UndoOperations(FUndo.Count-1);
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
    HideProgressWnd;
  end;

  procedure TfMain.ApplyLanguage;
  begin
     // Настраиваем прочие свойства
    if FInitialized then begin
       // Перестраиваем представления, т.к. они содержат локализуемые названия узлов
      FProject.Views.Invalidate;
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
    LangManager.LanguageID := SettingValueInt(ISettingID_Gen_Language);
     // Настраиваем тему
    with (RootSetting.Settings[ISettingID_Gen_Theme] as TPhoaListSetting) do TBXSetTheme(VariantText); 
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
      1: begin
        MakeImagesLoaded('PNG_MIDDLE_IMAGES', ilActionsMiddle);
        tbMain.Images := ilActionsMiddle;
      end;
      2: begin
        MakeImagesLoaded('PNG_LARGE_IMAGES', ilActionsLarge);
        tbMain.Images := ilActionsLarge;
      end;
    end;
     // Настраиваем дерево групп
    ApplyTreeSettings(tvGroups);
    tvGroups.HintMode := GTreeHintModeToVTHintMode(TGroupTreeHintMode(SettingValueInt(ISettingID_Browse_GT_Hints)));
    FGroupTreeHintProps := IntToGroupProps(SettingValueInt(ISettingID_Browse_GT_HintProps));
     // Обновляем режим отображения
    UpdateFlatModeAction; 
     // Настраиваем Viewer
    with Viewer do begin
      BeginUpdate;
      try
        ThumbBackBorderStyle  := TThumbBackBorderStyle(SettingValueInt(ISettingID_Browse_ViewerThBordSt));
        ThumbBackBorderColor  := SettingValueInt (ISettingID_Browse_ViewerThBordCl);
        Color                 := SettingValueInt (ISettingID_Browse_ViewerBkColor);
        ThumbBackColor        := SettingValueInt (ISettingID_Browse_ViewerThBColor);
        ThumbFontColor        := SettingValueInt (ISettingID_Browse_ViewerThFColor);
        ThumbShadowVisible    := SettingValueBool(ISettingID_Browse_ViewerThShadow);
        ThumbShadowBlurRadius := SettingValueInt(ISettingID_Browse_ViewerThShRadius);
        ThumbShadowOffset     := Point(SettingValueInt(ISettingID_Browse_ViewerThShOffsX), SettingValueInt(ISettingID_Browse_ViewerThShOffsY));
        ThumbShadowColor      := SettingValueInt(ISettingID_Browse_ViewerThShColor);
        ThumbShadowOpacity    := SettingValueInt(ISettingID_Browse_ViewerThShOpact);
        ShowThumbTooltips     := SettingValueBool(ISettingID_Browse_ViewerTooltips);
        ThumbTooltipProps     := IntToPicProps(SettingValueInt(ISettingID_Browse_ViewerTipProps));
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

  procedure TfMain.BeginOperation;
  var Group: IPhotoAlbumPicGroup;
  begin
    if FOpLockCounter=0 then begin
      ResetMode;
      tvGroups.BeginUpdate;
       // Сохраняем ID текущей группы
      Group := CurGroup;
      if Group=nil then FSavedGroupID := 0 else FSavedGroupID := Group.ID;
       // Сохраняем параметры отображения Viewer
      FViewerDisplayData := FViewer.SaveDisplay;
    end;
    Inc(FOpLockCounter);
  end;

  procedure TfMain.bUndoPopup(Sender: TTBCustomItem; FromLink: Boolean);
  var i: Integer;
  begin
    ulToolbarUndo.Strings.Clear;
    for i := FUndo.Count-1 downto 0 do ulToolbarUndo.Strings.Add(FUndo[i].Name);
     // Initialize tbxlToolbarUndo.Caption
    ulToolbarUndoChange(nil);
  end;

  function TfMain.CheckSave: Boolean;
  var mbr: TMessageBoxResults;
  begin
    Result := FUndo.IsUnmodified;
    if not Result then begin
      mbr := PhoaMsgBox(mbkConfirm, 'SConfirm_FileNotSaved', [DisplayFileName], True, False, [mbbYes, mbbNo, mbbCancel]);
      if mbrYes in mbr then begin
        aSave.Execute;
        Result := FUndo.IsUnmodified;
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
    if bForceRemove then FSearchResults.PicsX.Clear;
     // Если есть результаты, следим, чтобы узел поиска существовал
    if FSearchResults.Pics.Count>0 then begin
      if FSearchNode=nil then FSearchNode := tvGroups.AddChild(nil);
       // Если надо выделить узел
      if bDoSelectNode then ActivateVTNode(tvGroups, FSearchNode);
     // Если нет результатов - следим, чтобы его не было
    end else if FSearchNode<>nil then begin
      tvGroups.DeleteNode(FSearchNode);
      FSearchNode := nil;
    end;
  end;

  procedure TfMain.dklcMainLanguageChanged(Sender: TObject);
  begin
    dkTop.EndUpdate;
    dkLeft.EndUpdate;
    dkRight.EndUpdate;
    dkBottom.EndUpdate;
    ApplyLanguage;
  end;

  procedure TfMain.dklcMainLanguageChanging(Sender: TObject);
  begin
    ResetMode;
    dkTop.BeginUpdate;
    dkLeft.BeginUpdate;
    dkRight.BeginUpdate;
    dkBottom.BeginUpdate;
  end;

  procedure TfMain.DoEnableTools(Item: TTBCustomItem);
  begin
     // Если пункт содержит подпункты-инструменты
    if Item.Count>0 then
       // Создаём список ссылок на изображения и настраиваем доступность инструментов
      AdjustToolAvailability(RootSetting.Settings[ISettingID_Tools] as TPhoaToolPageSetting, Item, GetSelectedPics);
  end;

  procedure TfMain.DoLoad(const sFileName: String);
  begin
    tvGroups.BeginSynch;
    try
      FUndo.BeginUpdate;
      tvGroups.BeginUpdate;
      StartWait;
      try
        try
           // Уничтожаем результаты поиска
          DisplaySearchResults(True, False);
           // Загружаем файл и очищаем буфер отката
          FProject.LoadFromFile(ExpandUNCFileName(sFileName){!!!, FUndo});
           // Регистрируем файл в списке MRU
          mruOpen.Add(sFileName);
        finally
           // Загружаем список представлений и по умолчанию выбираем "Группы изображений"
          LoadViewList(-1);
        end;
      finally
        StopWait;
        tvGroups.EndUpdate;
        FUndo.EndUpdate;
      end;
    finally
      tvGroups.EndSynch;
    end;
  end;

  procedure TfMain.DoSave(const sFileName: String; iRevisionNumber: Integer);
  begin
     // Предупреждаем пользователя, если он сохраняет в более старую ревизию
    if (iRevisionNumber<IPhFileRevisionNumber) and not PhoaConfirm(True, 'SConfirm_SavingOldFormatFile', ISettingID_Dlgs_ConfmOldFile) then Exit;
    FUndo.BeginUpdate;
    StartWait;
    try
      FProject.SaveToFile(sFileName, SProject_Generator, SProject_Remark, iRevisionNumber{!!!, FUndo});
    finally
      StopWait;
      FUndo.EndUpdate;
    end;
     // Регистрируем имя файла в списке MRU
    mruOpen.Add(sFileName);
    EnableActions;
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
    bPics   := FProject.Pics.Count>0;
    bPicSel := Viewer.SelectedPics.Count>0;
    bView   := ViewIndex>=0;
    aUndo.Caption := ConstVal(iif(FUndo.CanUndo, 'SUndoActionTitle', 'SCannotUndo'), [FUndo.LastOpName]);
    aUndo.Enabled                := FUndo.CanUndo;
    smUndoHistory.Enabled        := FUndo.CanUndo;
    aNewGroup.Enabled            := gnk in [gnkPhoA, gnkPhoaGroup];
    aNewPic.Enabled              := gnk in [gnkPhoA, gnkPhoaGroup];
    aDelete.Enabled              := (bGr and (gnk=gnkPhoaGroup)) or (bPic and (gnk in [gnkPhoA, gnkPhoaGroup]) and bPicSel);
    aEdit.Enabled                := (bGr and (gnk in [gnkPhoA, gnkPhoaGroup, gnkView])) or (bPic and (gnk in [gnkPhoA, gnkPhoaGroup, gnkSearch]) and bPicSel and not bView);
    aCut.Enabled                 := (gnk in [gnkPhoA, gnkPhoaGroup]) and bPicSel and (wClipbrdPicFormatID<>0);
    aCopy.Enabled                := bPicSel and (wClipbrdPicFormatID<>0);
    aPaste.Enabled               := (gnk in [gnkPhoA, gnkPhoaGroup]) and Clipboard.HasFormat(wClipbrdPicFormatID);
    aSortPics.Enabled            := (gnk in [gnkPhoA, gnkPhoaGroup, gnkSearch]) and bPics;
    aSelectAll.Enabled           := (gnk<>gnkNone) and (Viewer.SelectedPics.Count<CurGroup.Pics.Count);
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
    Caption := Format('[%s%s] - %s', [ExtractFileName(DisplayFileName), asUnmod[FUndo.IsUnmodified], ConstVal('SAppCaption')]);
    Application.Title := Caption;
    sbarMain.Panels[1].Caption := ConstVal('SPicCount', [FProject.Pics.Count]);
  end;

  procedure TfMain.EnableTools;
  begin
     // Настраиваем инструменты меню "Сервис"
    DoEnableTools(giTools_ToolsMenu);
     // Сбрасываем флаги инструментов popup-меню
    FGroupsPopupToolsValidated := False;
    FPicsPopupToolsValidated   := False;
  end;

  procedure TfMain.EndOperation(Operation: TPhoaOperation);
  var
    IFlags: TUndoInvalidationFlags;
    GetOpGroupNode_Cache, GetOpParentGroupNode_Cache: PVirtualNode;
    Group: IPhotoAlbumPicGroup;

     // Получает узел в tvGroups, соответствующий группе GroupAbsIdx операции, кэшируя результат
    function GetOpGroupNode: PVirtualNode;
    begin
      if GetOpGroupNode_Cache=nil then GetOpGroupNode_Cache := FindGroupNodeByID(Operation.OpGroupID);
      Result := GetOpGroupNode_Cache;
      Assert(Result<>nil, 'Failed to locate Operation Group Node in TfMain.EndOperation()');
    end;

     // Получает узел в tvGroups, соответствующий группе ParentGroupAbsIdx операции, кэшируя результат
    function GetOpParentGroupNode: PVirtualNode;
    begin
      if GetOpParentGroupNode_Cache=nil then GetOpParentGroupNode_Cache := FindGroupNodeByID(Operation.OpParentGroupID);
      Result := GetOpParentGroupNode_Cache;
      Assert(Result<>nil, 'Failed to locate Operation Parent Group Node in TfMain.EndOperation()');
    end;

  begin
    Assert(FOpLockCounter>0, 'Excessive TfMain.EndOperation() call');
    Dec(FOpLockCounter);
    try
       // Operation будет nil в случае неудачной попытки выполнения (при вызове EndOperation() в секции finally)
      if Operation<>nil then begin
         // Initialize cache
        GetOpGroupNode_Cache       := nil;
        GetOpParentGroupNode_Cache := nil;
         // Отрабатываем изменения, вносимые операцией
        IFlags := Operation.InvalidationFlags;
         // -- Переинициализация родителя
        if uifXReinitParent in IFlags then tvGroups.ReinitNode(GetOpParentGroupNode, uifXReinitRecursive in IFlags);
         // -- Переинициализация братьев
        if uifXReinitSiblings in IFlags then tvGroups.ReinitChildren(GetOpParentGroupNode, uifXReinitRecursive in IFlags);
      end;
    finally
      if FOpLockCounter=0 then tvGroups.EndUpdate;
    end;
     // Уничтожаем результаты поиска
    DisplaySearchResults(True, False);
     // Редактирование текста узла, соответствующего группе операции: выполняем после снятия блокировки перерисовки
    if uifXEditGroup in IFlags then begin
      tvGroups.Selected[GetOpGroupNode] := True;
      tvGroups.FocusedNode := GetOpGroupNode;
      tvGroups.EditNode(GetOpGroupNode, -1);
    end;
     // Обновляем Viewer (после возможной смены выделения из-за редактирования текста узла)
    if FOpLockCounter=0 then begin
      FViewer.BeginUpdate;
      try
        Group := CurGroup;
        RefreshViewer;
         // Если группа не поменялась, восстанавливаем параметры отображения
        if (Group<>nil) and (Group.ID=FSavedGroupID) then FViewer.RestoreDisplay(FViewerDisplayData);
      finally
        FViewer.EndUpdate;
        FViewerDisplayData := nil;
      end;
    end;
  end;

  function TfMain.FindGroupNodeByID(iGroupID: Integer): PVirtualNode;
  begin
    Result := tvGroups.GetFirst;
    while Result<>nil do begin
      if PPhotoAlbumPicGroup(tvGroups.GetNodeData(Result))^.ID=iGroupID then Exit;
      Result := tvGroups.GetNext(Result);
    end;
  end;

  procedure TfMain.FormActivate(Sender: TObject);
  begin
     // Если есть окошко прогресса, держим главное окно позади него  
    KeepBehindProgressWnd(Handle);
  end;

  procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    ResetMode;
     // Если нет несохранённых данных - проверяем необходимость спрашивания подтверждения
    if FUndo.IsUnmodified then
      CanClose := PhoaConfirm(False, 'SConfirm_AppExit', ISettingID_Dlgs_ConfmAppExit)
     // Иначе спрашиваем, сохранять ли данные 
    else
      CanClose := CheckSave;
  end;

  procedure TfMain.FormCreate(Sender: TObject);
  begin
    try
      ShortTimeFormat := 'hh:nn';
      LongTimeFormat  := 'hh:nn:ss';
       // Настраиваем fpMain
      fpMain.IniFileName := SRegRoot;
      fpMain.IniSection  := SRegMainWindow_Root;
       // Создаём фотоальбом
      FProject := NewPhotoAlbumProject;
      FViewIndex := -1;
       // Настраиваем Application
      Application.OnHint      := AppHint;
      Application.OnException := AppException;
      Application.OnIdle      := AppIdle;
       // Создаём группу - список результатов поиска
      FSearchResults := NewPhotoAlbumPicGroup(nil, IGroupID_SearchResults);
       // Create undoable operations list
      FUndo := TPhoaUndo.Create;
      FUndo.OnStatusChange := OperationsStatusChange;
      FUndo.OnOpDone       := OperationsListChange;
      FUndo.OnOpUndone     := OperationsListChange;
       // Create viewer
      FViewer := TThumbnailViewer.Create(Self);
      with FViewer do begin
        Parent            := Self;
        Align             := alClient;
        //#TODO: Выставлять DisplayMode := tvdmDetail или tvdmTile;
        DragCursor        := crDragMove;
        PopupMenu         := pmPics;
        OnDragDrop        := ViewerDragDrop;
        OnSelectionChange := ViewerSelectionChange;
        OnStartViewMode   := aaView;
      end;
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
    FViewer.Free;
    FViewerDisplayData := nil;
    FViewedPics        := nil;
    FSearchResults     := nil;
    FProject           := nil;
    FUndo.Free;
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

  function TfMain.GetCurGroup: IPhotoAlbumPicGroup;
  var p: PPhotoAlbumPicGroup;
  begin
    p := tvGroups.GetNodeData(tvGroups.FocusedNode);
    if p=nil then Result := nil else Result := p^;
  end;

  function TfMain.GetCurRootGroup: IPhotoAlbumPicGroup;
  begin
    if ViewIndex<0 then Result := FProject.RootGroupX else Result := FProject.ViewsX[ViewIndex].RootGroupX;
  end;

  function TfMain.GetDisplayFileName: String;
  begin
    Result := FileName;
    if Result='' then Result := SDefaultFName;
  end;

  function TfMain.GetFileName: String;
  begin
    Result := FProject.FileName;
  end;

  function TfMain.GetNodeKind(Tree: TBaseVirtualTree; Node: PVirtualNode): TGroupNodeKind;
  var g: IPhotoAlbumPicGroup;
  begin
    Result := gnkNone;
    if Node<>nil then begin
      g := PPhotoAlbumPicGroup(Tree.GetNodeData(Node))^;
      if g.Owner=nil then begin
        if g.ID=IGroupID_SearchResults then Result := gnkSearch
        else if g=FProject.RootGroup      then Result := gnkPhoA
        else if FViewIndex>=0          then Result := gnkView;
      end else
        if FViewIndex>=0 then Result := gnkViewGroup else Result := gnkPhoaGroup;
    end;
  end;

  function TfMain.GetSelectedPics: IPhoaPicList;
  begin
    if Viewer.Focused then        Result := Viewer.SelectedPics
    else if tvGroups.Focused then Result := FViewedPics
    else                          Result := nil;
  end;

  function TfMain.GetViewIndex: Integer;
  begin
    Result := FViewIndex;
  end;

//!!!  function TfMain.GetViews: TPhoaViews;
//  begin
//    Result := FProject.Views;
//  end;

  function TfMain.IsShortCut(var Message: TWMKey): Boolean;
  begin
     // Запрещаем Shortcuts в процессе редактирования текста в дереве групп
    if tvGroups.IsEditing then Result := False else Result := inherited IsShortCut(Message);
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

  procedure TfMain.LoadViewList(idxSelect: Integer);
  var
    i: Integer;
    tbi: TTBXItem;
  begin
     // Стираем все пункты пользовательских представлений
    gipmPhoaViewViews.Clear;
     // Добавляем пункты представлений фотоальбома
    for i := 0 to FProject.Views.Count-1 do begin
      tbi := TTBXItem.Create(Self);
      with tbi do begin
        Caption    := FProject.Views[i].Name;
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
    for i := 0 to FProject.Views.Count-1 do
      if i<>ViewIndex then FProject.Views[i].Invalidate;
  end;

  procedure TfMain.OperationsStatusChange(Sender: TObject);
  var iMaxCnt: Integer;
  begin
    if tsUpdating in tvGroups.TreeStates then Exit;
     // Ограничиваем количество операций в буфере
    iMaxCnt := SettingValueInt(ISettingID_Browse_MaxUndoCount);
    with FUndo do
      while Count>iMaxCnt do Delete(0);
    EnableActions;
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
        idx := FProject.Views.IndexOfName(sViewName);
        if idx>=0 then ViewIndex := idx;
      end;
    end;

     // Выбирает в качестве текущей заданную группу в дереве, если sGroupPath<>''
    procedure SelectGroupByPath(const sGroupPath: String);
    begin
      if sGroupPath<>'' then CurGroup := CurRootGroup.GroupByPathX[sGroupPath];
    end;

     // Выбирает изображение с заданным ID
    procedure SelectPicByID(iID: Integer);
    begin
      if (iID>0) and (CurGroup<>nil) then begin
        Viewer.ItemIndex := CurGroup.Pics.IndexOfID(iID);
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
  var UniquePics, ViewPics: IPhotoAlbumPicList;

    procedure RecursivelyAddPics(Group: IPhotoAlbumPicGroup);
    var
      i: Integer;
      bDoAdd: Boolean;
      Pic: IPhoaPic;
    begin
       // Добавляем ссылки на изображения группы
      for i := 0 to Group.Pics.Count-1 do begin
        Pic := Group.Pics[i];
         // Проверяем на дубликаты
        UniquePics.Add(Pic, True, bDoAdd);
         // Добавляем (на дубликаты уже проверено)
        if bDoAdd then ViewPics.Add(Pic, False);
      end;
       // Повторяем то же для вложенных групп
      for i := 0 to Group.Groups.Count-1 do RecursivelyAddPics(Group.GroupsX[i]);
    end;

  begin
    FViewedPics := nil;
     // Если есть текущая группа
    if CurGroup<>nil then
       // Не рекурсивный режим
      if not SettingValueBool(ISettingID_Browse_FlatMode) then
        FViewedPics := CurGroup.PicsX
       // Рекурсивный режим
      else begin
         // Создаём временный [сортированный] список изображений, чтобы быстро отсеивать уже добавленные изображения
        UniquePics := NewPhotoAlbumPicList(True);
         // Создаём список изображений для просмотра
        ViewPics := NewPhotoAlbumPicList(False);
         // Рекурсивно наполняем список
        RecursivelyAddPics(CurGroup);
         // Сохраняем список 
        FViewedPics := ViewPics;
      end;
     // Обновляем вьюер
    FViewer.ReloadPicList(FViewedPics);
  end;

  procedure TfMain.ResetMode;
  begin
     // Завершаем inplace-редактирование текста узла в дереве групп
    tvGroups.EndEditNode;
  end;

  procedure TfMain.SetCurGroup(Value: IPhotoAlbumPicGroup);
  var n: PVirtualNode;
  begin
     // Перебираем узлы, пока не найдём
    if Value<>nil then begin
      n := tvGroups.GetFirst;
      while (n<>nil) and (PPhotoAlbumPicGroup(tvGroups.GetNodeData(n))^<>Value) do n := tvGroups.GetNext(n);
    end else
      n := nil;
     // Активизируем
    ActivateVTNode(tvGroups, n);
  end;

  procedure TfMain.SetFileName(const Value: String);
  begin
    FProject.FileName := Value;
  end;

  procedure TfMain.SetGroupExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var p: PPhotoAlbumPicGroup;
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

  procedure TfMain.StartViewMode(InitFlags: TImgViewInitFlags);
  var iPicIndex: Integer;
  begin
    iPicIndex := Viewer.ItemIndex;
    if (FViewedPics<>nil) and (iPicIndex>=0) then begin
      ViewImage(InitFlags, FViewedPics, FProject, iPicIndex, FUndo, ViewIndex<0);
      Viewer.ItemIndex := iPicIndex;
    end;
  end;

  procedure TfMain.ToolItemClick(Sender: TObject);
  begin
     // Создаём массив ссылок на изображения и выполняем инструмент
    (RootSetting.Settings[ISettingID_Tools][TComponent(Sender).Tag] as TPhoaToolSetting).Execute(GetSelectedPics);
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
    gTgt: IPhotoAlbumPicGroup;
    AM: TVTNodeAttachMode;
    iNewIndex, iCnt, iCntBefore: Integer;
    bCopy: Boolean;
    Operation: TPhoaOperation;
  begin
    Operation := nil;
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
      BeginOperation;
      try
        Operation := TPhoaOp_GroupDragAndDrop.Create(
          FUndo,
          FProject,
          PPhotoAlbumPicGroup(Sender.GetNodeData(nSrc))^, // Group being dragged
          PPhotoAlbumPicGroup(Sender.GetNodeData(nTgt))^, // New parent group
          iNewIndex);
      finally
        EndOperation(Operation);
      end;
      Sender.MoveTo(nSrc, Sender.DropTargetNode, AM, False);
      Sender.FullyVisible[nSrc] := True;
      Effect := DROPEFFECT_NONE;
     // Перетаскивание изображений
    end else if Source=Viewer then begin
      bCopy := (GetKeyState(VK_CONTROL) and $80<>0) or (GetNodeKind(tvGroups, nSrc)=gnkSearch);
      gTgt := PPhotoAlbumPicGroup(Sender.GetNodeData(nTgt))^;
      iCnt := Viewer.SelectedPics.Count;
      iCntBefore := gTgt.Pics.Count;
      BeginOperation;
      try
        Operation := TPhoaMultiOp_PicDragAndDropToGroup.Create(FUndo, FProject, CurGroup, gTgt, Viewer.SelectedPics, bCopy);
      finally
        EndOperation(Operation);
      end;
      PhoaInfo(
        False,
        iif(bCopy, 'SNotify_DragCopy', 'SNotify_DragMove'),
        [iCnt, gTgt.Pics.Count-iCntBefore, iCnt-(gTgt.Pics.Count-iCntBefore)],
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
        (Viewer.SelectedPics.Count>0) and
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

  procedure TfMain.tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PPhotoAlbumPicGroup(Sender.GetNodeData(Node))^ := nil;
  end;

  procedure TfMain.tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var s: String;
  begin
    case GetNodeKind(Sender, Node) of
      gnkPhoA:      s := FProject.Description;
      gnkPhoaGroup: s := GetPicGroupPropStrs(PPhotoAlbumPicGroup(Sender.GetNodeData(Node))^, FGroupTreeHintProps, ': ', S_CRLF);
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
    p: PPhotoAlbumPicGroup;
    s: String;
  begin
    p := Sender.GetNodeData(Node);
    s := '';
     // Static text
    case TextType of
      ttNormal:
        case GetNodeKind(Sender, Node) of
          gnkPhoA:        s := ConstVal('SPhotoAlbumNode');
          gnkView:        s := FProject.Views[ViewIndex].Name;
          gnkSearch:      s := ConstVal('SSearchResultsNode');
          gnkPhoaGroup,
            gnkViewGroup: s := p^.Text;
        end;
      ttStatic: if p^.Pics.Count>0 then s := Format('(%d)', [p^.Pics.Count]);
    end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TfMain.tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var p, pp: PPhotoAlbumPicGroup;
  begin
    p := Sender.GetNodeData(Node);
     // Узел обычной группы
    if ParentNode<>nil then begin
      pp := Sender.GetNodeData(ParentNode);
      p^ := pp^.GroupsX[Node.Index];
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
  var Operation: TPhoaOperation;
  begin
    Operation := nil;
    BeginOperation;
    try
      case GetNodeKind(Sender, Node) of
        gnkView:      Operation := TPhoaOp_ViewEdit.Create(FUndo, FProject.ViewsX[ViewIndex], Self, UnicodetoAnsiCP(NewText, cMainCodePage), nil, nil);
        gnkPhoaGroup: Operation := TPhoaOp_GroupRename.Create(FUndo, FProject, CurGroup, UnicodetoAnsiCP(NewText, cMainCodePage));
      end;
    finally
      EndOperation(Operation);
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
    UndoOperations(FUndo.Count-ulToolbarUndo.ItemIndex-1);
  end;

  procedure TfMain.UndoOperation(Op: TPhoaOperation);
  var
    IFlags: TUndoInvalidationFlags;
    OpParentGroupNode: PVirtualNode;
  begin
     // Получаем данные до уничтожения объекта операции
    IFlags := Op.InvalidationFlags;
    OpParentGroupNode := nil;
    if uifUReinitParent in IFlags then OpParentGroupNode := FindGroupNodeByID(Op.OpParentGroupID);
     // Откатываем (и уничтожаем) операцию
    Op.Undo;
     // Проверяем флаги требуемых обновлений
     // -- Переинициализация всего дерева
    if uifUReinitAll    in IFlags then tvGroups.ReinitChildren(nil, True);
     // -- Переинициализация родителя
    if uifUReinitParent in IFlags then tvGroups.ReinitNode(OpParentGroupNode, uifUReinitRecursive in IFlags);
  end;

  procedure TfMain.UndoOperations(Index: Integer);
  var i: Integer;
  begin
    ResetMode;
    tvGroups.BeginUpdate;
    try
       // Крутим цикл с конца до указанного индекса
      for i := FUndo.Count-1 downto Index do UndoOperation(FUndo[i]);
       // Уничтожаем результаты поиска
      DisplaySearchResults(True, False);
    finally
      tvGroups.EndUpdate;
    end;
    RefreshViewer;
  end;

  procedure TfMain.UpdateFlatModeAction;
  begin
    aFlatMode.Checked := SettingValueBool(ISettingID_Browse_FlatMode);
  end;

  procedure TfMain.ViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
  var Operation: TPhoaOperation;
  begin
    Operation := nil;
    BeginOperation;
    try
      Operation := TPhoaOp_PicDragAndDropInsideGroup.Create(FUndo, FProject, CurGroup, Viewer.SelectedPics, Viewer.DropTargetIndex);
    finally
      EndOperation(Operation);
    end;
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
