//**********************************************************************************************************************
//  $Id: Main.pas,v 1.11 2004-05-03 16:34:03 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit Main;

interface

uses
   // GR32 must follow GraphicEx because of naming conflict between stretch filter constants
  Windows, Messages, SysUtils, Variants, Classes, Graphics, GraphicEx, GR32, Controls, Forms, Dialogs, phObj, ConsVars,
  ActiveX,
  TB2Item, Placemnt, DTLangTools, XPMan, TB2MRU, IniFiles,
  Menus, TBX, ImgList, ActnList, VirtualTrees, ExtCtrls, TBXStatusBars,
  TB2Dock, TB2Toolbar, TBXExtItems, TBXLists;

type
  TfMain = class(TForm, IPhoaViews)
    sMain: TSplitter;
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
    pGroups: TPanel;
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
    tvGroups: TVirtualStringTree;
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
  private
     // Рабочий альбом
    FPhoA: TPhotoAlbum;
     // Корневой узел - фотоальбом или представление
    FRootNode: PVirtualNode;
     // Узел результатов поиска
    FSearchedNode: PVirtualNode;
     // Список изображений - результаты поиска
    FSearchResults: TPhoaGroup;
     // Handle окна - следующего просмотрщика изменения содержимого clipboard
    FHNextClipbrdViewer: HWND;
     // Стек операций для отмены
    FOperations: TPhoaOperations;
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
     // Настраивает разрешённость Actions и настраивает Caption формы
    procedure EnableActions;
     // Настраивает доступность инструментов 
    procedure EnableTools;
     // Создаёт список изображений:
     //   если активно дерево групп - то всех изображений группы
     //   если активен вьюер - то выделенных во вьюере изображений
     //   иначе возвращает пустой список
    function  GetSelectedPicLinks: TPhoaPicLinks;
     // Отображает результаты поиска. Если bForceRemove=True, удаляет узел результатов, иначе, при bDoSelectNode=True,
     //   выделяет узел
    procedure DisplaySearchResults(bForceRemove, bDoSelectNode: Boolean);
     // Viewer events
    procedure ViewerSelectionChange(Sender: TObject);
    procedure ViewerDragDrop(Sender, Source: TObject; X, Y: Integer);
     // Application events
    procedure AppHint(Sender: TObject);
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
     // IPhoaViews
    function  GetViewIndex: Integer;
    procedure SetViewIndex(Value: Integer);
    function  GetViews: TPhoaViews;
    procedure LoadViewList(idxSelect: Integer);
     // Message handlers
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
     // Property handlers
    procedure SetFileName(const Value: String);
    function  GetFileName: String;
    function  GetDisplayFileName: String;
    function  GetCurGroup: TPhoaGroup;
  public
     // Выполняет операцию и обновляет визуальные объекты согласно флагам операции
    procedure PerformOperation(Op: TPhoaOperation);
     // Применяет параметры настройки
    procedure ApplySettings;
     // Переписывает во Viewer текущую выбранную группу
    procedure RefreshViewer;
     // Процедуры сохранения/восстановления настроек из ini-файла
    procedure IniStoreSettings(const sIniFileName: String);
    procedure IniRestoreSettings(const sIniFileName: String);
     // Props
     // -- Имя текущего файла фотоальбома (пустая строка, если новый фотоальбом)
    property FileName: String read GetFileName write SetFileName;
     // -- Текущая выбранная группа в дереве
    property CurGroup: TPhoaGroup read GetCurGroup;
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
  phToolSetting;

   //===================================================================================================================
   //  TfMain
   //===================================================================================================================

  procedure TfMain.aaAbout(Sender: TObject);
  begin
    ShowAbout;
  end;

  procedure TfMain.aaCopy(Sender: TObject);
  begin
    TPhoaBaseOp_PicCopy.Create(Viewer.GetSelectedPicArray);
  end;

  procedure TfMain.aaCut(Sender: TObject);
  begin
    TPhoaBaseOp_PicCopy.Create(Viewer.GetSelectedPicArray);
    PerformOperation(TPhoaMultiOp_PicDelete.Create(FOperations, FPhoA, CurGroup, PicArrayToIDArray(Viewer.GetSelectedPicArray)));
  end;

  procedure TfMain.aaDelete(Sender: TObject);
  begin
    if CurGroup<>nil then
       // Удаление группы
      if ActiveControl=tvGroups then begin
        if ConfirmIfSettingRequires(ConstVal('SConfirm_DelGroup'), ISettingID_Dlgs_ConfmDelGroup) then
          PerformOperation(TPhoaOp_GroupDelete.Create(FOperations, FPhoA, CurGroup, True));
       // Удаление изображения
      end else if (ActiveControl=Viewer) and (Viewer.SelCount>0) and ConfirmIfSettingRequires(ConstVal('SConfirm_DelPics'), ISettingID_Dlgs_ConfmDelPics) then
        PerformOperation(TPhoaMultiOp_PicDelete.Create(FOperations, FPhoA, CurGroup, PicArrayToIDArray(Viewer.GetSelectedPicArray)));
  end;

  procedure TfMain.aaEdit(Sender: TObject);
  begin
    if ActiveControl=tvGroups then begin
       // Корневой узел
      if tvGroups.FocusedNode=FRootNode then
        if FViewIndex>=0 then aPhoaView_Edit.Execute else EditPhoA(FPhoA, FOperations)
       // Редактирование группы
      else if CurGroup<>nil then
        tvGroups.EditNode(tvGroups.FocusedNode, -1);
     // Редактирование изображения
    end else if (ActiveControl=Viewer) and (Viewer.SelCount>0) and EditPic(Viewer.GetSelectedPicArray, FPhoA, FOperations) then
      RefreshViewer;
  end;

  procedure TfMain.aaExit(Sender: TObject);
  begin
    Close;
  end;

  procedure TfMain.aaFileOperations(Sender: TObject);
  var
    View: TPhoaView;
    bPhoaChanged: Boolean;
  begin
    if FViewIndex>=0 then View := FPhoA.Views[FViewIndex] else View := nil;
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
    if DoSearch(FPhoA, CurGroup, FSearchResults) then begin
      DisplaySearchResults(False, True);
      RefreshViewer;
    end;
  end;

  procedure TfMain.aaHelpContents(Sender: TObject);
  begin
    HtmlHelpShowContents;
  end;

  procedure TfMain.aaHelpWebsite(Sender: TObject);
  begin
    OpenWebsite;
  end;

  procedure TfMain.aaIniLoadSettings(Sender: TObject);
  begin
    with TOpenDialog.Create(Self) do
      try
        DefaultExt := SDefaultIniFileExt;
        FileName   := SDefaultIniFileName;
        Filter     := ConstVal('SFileFilter_Ini');
        Options    := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
        Title      := ConstVal('SDlgTitle_OpenIni');
        if Execute then IniRestoreSettings(FileName);
      finally
        Free;
      end;
  end;

  procedure TfMain.aaIniSaveSettings(Sender: TObject);
  begin
    with TSaveDialog.Create(Self) do
      try
        DefaultExt := SDefaultIniFileExt;
        FileName   := SDefaultIniFileName;
        Filter     := ConstVal('SFileFilter_Ini');
        Options    := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
        Title      := ConstVal('SDlgTitle_SaveIni');
        if Execute then IniStoreSettings(FileName);
      finally
        Free;
      end;
  end;

  procedure TfMain.aaNew(Sender: TObject);
  begin
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
    PerformOperation(TPhoaOp_GroupNew.Create(FOperations, FPhoA, CurGroup));
  end;

  procedure TfMain.aaNewPic(Sender: TObject);
  begin
    if SelectFiles(FPhoA, PPhoaGroup(tvGroups.GetNodeData(tvGroups.FocusedNode))^, FOperations) then RefreshViewer;
  end;

  procedure TfMain.aaOpen(Sender: TObject);
  begin
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
    iCntBefore := CurGroup.PicIDs.Count;
    PerformOperation(TPhoaMultiOp_PicPaste.Create(FOperations, FPhoA, CurGroup));
    InfoIfSettingRequires(ConstVal('SNotify_Paste', [CurGroup.PicIDs.Count-iCntBefore]), ISettingID_Dlgs_NotifyPaste);
  end;

  procedure TfMain.aaPhoaView_Delete(Sender: TObject);
  begin
    if ConfirmIfSettingRequires(ConstVal('SConfirm_DelView'), ISettingID_Dlgs_ConfmDelView) then
      TPhoaOp_ViewDelete.Create(FOperations, Self);
  end;

  procedure TfMain.aaPhoaView_Edit(Sender: TObject);
  begin
    EditView(FPhoA.Views[FViewIndex], FPhoA, FOperations);
  end;

  procedure TfMain.aaPhoaView_MakeGroup(Sender: TObject);
  begin
    MakeGroupFromView(FPhoA, FOperations, Self);
  end;

  procedure TfMain.aaPhoaView_New(Sender: TObject);
  begin
    EditView(nil, FPhoA, FOperations);
  end;

  procedure TfMain.aaPicOps(Sender: TObject);
  begin
    DoPicOps(FPhoA, FOperations, CurGroup, PicArrayToIDArray(Viewer.GetSelectedPicArray));
  end;

  procedure TfMain.aaSave(Sender: TObject);
  begin
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
    Viewer.SelectAll;
  end;

  procedure TfMain.aaSelectNone(Sender: TObject);
  begin
    Viewer.SelectNone;
  end;

  procedure TfMain.aaSettings(Sender: TObject);
  begin
     // В диалоге настроек по умолчанию выбираем кнопку "Режим обзора"
    if EditSettings(ISettingID_Browse) then begin
      ApplySettings;
       // Применяем разрёшённость Drag'n'Drop у Viewer
      EnableActions;
    end;
  end;

  procedure TfMain.aaSortPics(Sender: TObject);
  var g: TPhoaGroup;
  begin
    if CurGroup=FPhoA.RootGroup then g := nil else g := CurGroup;
    DoSortPics(FPhoA, g, FOperations, g=FSearchResults);
  end;

  procedure TfMain.aaStats(Sender: TObject);
  begin
    ShowPhoaStats(FPhoA, CurGroup, Viewer.GetSelectedPicArray);
  end;

  procedure TfMain.aaUndo(Sender: TObject);
  begin
    UndoOperations(FOperations.Count-1);
  end;

  procedure TfMain.aaView(Sender: TObject);
  begin
    if Viewer.ItemIndex>=0 then ViewImage(CurGroup, FPhoA, Viewer.ItemIndex, FOperations, ViewIndex<0);
  end;

  procedure TfMain.AppHint(Sender: TObject);
  begin
    sbarMain.Panels[0].Caption := Application.Hint;
  end;

  procedure TfMain.ApplyLanguage;
  begin
     // Настраиваем прочие свойства
    if Assigned(FRootNode) then begin
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
  begin
    Result := FOperations.IsUnmodified;
    if not Result then
      case MessageBox(Handle, PChar(ConstVal('SConfirm_FileNotSaved', [DisplayFileName])), PChar(ConstVal('SDlgTitle_Confirm')), MB_ICONEXCLAMATION or MB_YESNOCANCEL) of
        IDYES: begin
          aSave.Execute;
          Result := FOperations.IsUnmodified;
        end;
        IDNO: Result := True;
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
      if FSearchedNode=nil then FSearchedNode := tvGroups.AddChild(nil);
       // Если надо выделить узел
      if bDoSelectNode then begin
        tvGroups.Selected[FSearchedNode] := True;
        tvGroups.FocusedNode := FSearchedNode;
      end;
     // Если нет результатов - следим, чтобы его не было
    end else if FSearchedNode<>nil then begin
      tvGroups.DeleteNode(FSearchedNode);
      FSearchedNode := nil;
    end;
  end;

  procedure TfMain.DoLoad(const sFileName: String);
  begin
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
  end;

  procedure TfMain.dtlsMainLanguageChanged(Sender: TObject);
  begin
    ApplyLanguage;
  end;

  procedure TfMain.EnableActions;
  const asUnmod: Array[Boolean] of String[1] = ('*', '');
  var
    bGr, bPic, bPics, bGrSel, bPicSel, bPhoANode, bSrchNode, bPicGroups: Boolean;
    n: PVirtualNode;
  begin
    if csDestroying in ComponentState then Exit;
    n := tvGroups.FocusedNode;
    bGr        := ActiveControl=tvGroups;
    bPic       := ActiveControl=Viewer;
    bPics      := FPhoA.Pics.Count>0;
    bPhoANode  := n=FRootNode;
    bSrchNode  := n=FSearchedNode;
    bGrSel     := (n<>nil) and not bSrchNode;
    bPicSel    := Viewer.SelCount>0;
    bPicGroups := FViewIndex<0;
    aUndo.Caption := ConstVal(iif(FOperations.CanUndo, 'SUndoActionTitle', 'SCannotUndo'), [FOperations.LastOpName]);
    aUndo.Enabled               := FOperations.CanUndo;
    smUndoHistory.Enabled       := FOperations.CanUndo;
    aNewGroup.Enabled           := bPicGroups and bGrSel;
    aNewPic.Enabled             := bPicGroups and bGrSel;
    aDelete.Enabled             := bPicGroups and ((bGr and bGrSel and not bPhoANode) or (bPic and bPicSel and not bSrchNode));
    aEdit.Enabled               := (not bPicGroups and bGr and bPhoANode) or (bPicGroups and bGr and bGrSel) or (bPicGroups and bPic and bPicSel);
    aCut.Enabled                := bPicGroups and bPicSel and (wClipbrdPicFormatID<>0);
    aCopy.Enabled               := bPicSel and (wClipbrdPicFormatID<>0);
    aPaste.Enabled              := bPicGroups and bGrSel and Clipboard.HasFormat(wClipbrdPicFormatID);
    aSortPics.Enabled           := bPicGroups and bPics;
    aSelectAll.Enabled          := (CurGroup<>nil) and (Viewer.SelCount<CurGroup.PicIDs.Count);
    aSelectNone.Enabled         := bPicSel;
    aView.Enabled               := bPicSel;
    aPicOps.Enabled             := bPicGroups and bPicSel;
    aFileOperations.Enabled     := bPics and ((bGr and bGrSel) or (bPic and bPicSel));
    aFind.Enabled               := bPics;
     // Views
    aPhoaView_Delete.Enabled    := not bPicGroups;
    aPhoaView_Edit.Enabled      := not bPicGroups;
    aPhoaView_MakeGroup.Enabled := not bPicGroups;
     // Drag-and-drop
    Viewer.DragEnabled := bPicGroups and SettingValueBool(ISettingID_Browse_ViewerDragDrop);
     // Инструменты
    EnableTools; 
     // Настраиваем Captions
    Caption := Format('[%s%s] - %s', [ExtractFileName(DisplayFileName), asUnmod[FOperations.IsUnmodified], ConstVal('SAppCaption')]);
    Application.Title := Caption;
    sbarMain.Panels[1].Caption := ConstVal('SPicCount', [FPhoA.Pics.Count]);
  end;

  procedure TfMain.EnableTools;
  var PicLinks: TPhoaPicLinks;
  begin
    PicLinks := GetSelectedPicLinks;
    try
       // Если активны группы - настраиваем popup-меню групп
      if tvGroups.Focused then
        AdjustToolAvailability(giTools_GroupsMenu, PicLinks)
       // Если активны группы - настраиваем popup-меню вьюера
      else if Viewer.Focused then
        AdjustToolAvailability(giTools_PicsMenu, PicLinks);
       // Настраиваем инструменты меню "Сервис"
      AdjustToolAvailability(giTools_ToolsMenu, PicLinks);
    finally
      PicLinks.Free;
    end;
  end;

  procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
     // Если нет несохранённых данных - проверяем необходимость спрашивания подтверждения
    if FOperations.IsUnmodified then
      CanClose := ConfirmIfSettingRequires(ConstVal('SConfirm_AppExit'), ISettingID_Dlgs_ConfmAppExit)
     // Иначе спрашиваем, сохранять ли данные 
    else
      CanClose := CheckSave;
  end;

  procedure TfMain.FormCreate(Sender: TObject);
  begin
    ShortTimeFormat := 'hh:nn';
    LongTimeFormat  := 'hh:nn:ss';
     // Настраиваем fpMain
    fpMain.IniFileName := SRegRoot;
    fpMain.IniSection  := SRegMainWindow_Root;     
     // Создаём фотоальбом
    FPhoA := TPhotoAlbum.Create;
    FViewIndex := -1;
     // Настраиваем дерево папок
    tvGroups.RootNodeCount := 1;
    FRootNode := tvGroups.GetFirst;
     // Настраиваем Application
    Application.OnHint := AppHint;
     // Создаём список результатов поиска
    FSearchResults := TPhoaGroup.Create(nil);
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
  var sAutoLoadIniFile: String;
  begin
     // Загружаем основные (определяемые пользователем) установки
    RootSetting.RegLoad(fpMain.RegIniFile);
    with fpMain.RegIniFile do begin
       // Load misc settings
      ViewInfoPos   := Rect(
                       ReadInteger(SRegPrefs_Root, 'ViewInfoPosL', 90),
                       ReadInteger(SRegPrefs_Root, 'ViewInfoPosT', 9400),
                       ReadInteger(SRegPrefs_Root, 'ViewInfoPosR', 9910),
                       ReadInteger(SRegPrefs_Root, 'ViewInfoPosB', 9880));
      pGroups.Width := ReadInteger(SRegPrefs_Root, 'GroupsWidth',  150);
       // Load history
      mruOpen.LoadFromRegIni(fpMain.RegIniFile, SRegOpen_FilesMRU);
    end;
     // Load toolbars
    TBRegLoadPositions(Self, HKEY_CURRENT_USER, SRegRoot+'\'+SRegMainWindow_Toolbars);
     // Если нужно и присутствует ini-файл, подгружаем настройки из него
    if SettingValueBool(ISettingID_Gen_LookupPhoaIni) then begin
      sAutoLoadIniFile := ExtractFilePath(ParamStr(0))+SDefaultIniFileName;
      if FileExists(sAutoLoadIniFile) then IniRestoreSettings(sAutoLoadIniFile);
    end;
     // Применяем настройки
    RootSetting.Modified := True;
    ApplySettings;
    ApplyLanguage;
     // If command line specifies a file, then load it; else load view list and select "Picture groups"
    if ParamCount>0 then DoLoad(ParamStr(1)) else LoadViewList(-1);
    EnableActions;
  end;

  procedure TfMain.fpMainSavePlacement(Sender: TObject);
  begin
     // Сохраняем основные (определяемые пользователем) установки
    RootSetting.RegSave(fpMain.RegIniFile);
    with fpMain.RegIniFile do begin
       // Save misc settings
      WriteInteger(SRegPrefs_Root, 'ViewInfoPosL', ViewInfoPos.Left);
      WriteInteger(SRegPrefs_Root, 'ViewInfoPosT', ViewInfoPos.Top);
      WriteInteger(SRegPrefs_Root, 'ViewInfoPosR', ViewInfoPos.Right);
      WriteInteger(SRegPrefs_Root, 'ViewInfoPosB', ViewInfoPos.Bottom);
      WriteInteger(SRegPrefs_Root, 'GroupsWidth',  pGroups.Width);
       // Save toolbars
      TBRegSavePositions(Self, HKEY_CURRENT_USER, SRegRoot+'\'+SRegMainWindow_Toolbars);
       // Save history
      mruOpen.SaveToRegIni(fpMain.RegIniFile, SRegOpen_FilesMRU);
    end;
  end;

  function TfMain.GetCurGroup: TPhoaGroup;
  var p: PPhoaGroup;
  begin
    p := tvGroups.GetNodeData(tvGroups.FocusedNode);
    if p=nil then Result := nil else Result := p^;
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

  procedure TfMain.IniRestoreSettings(const sIniFileName: String);
  var fi: TIniFile;
  begin
    fi := TIniFile.Create(sIniFileName);
    try
       // Загружаем установки
      RootSetting.IniLoad(fi);
       // Load misc settings
      ViewInfoPos := Rect(
        fi.ReadInteger(SRegPrefs_Root, 'ViewInfoPosL', ViewInfoPos.Left),
        fi.ReadInteger(SRegPrefs_Root, 'ViewInfoPosT', ViewInfoPos.Top),
        fi.ReadInteger(SRegPrefs_Root, 'ViewInfoPosR', ViewInfoPos.Right),
        fi.ReadInteger(SRegPrefs_Root, 'ViewInfoPosB', ViewInfoPos.Bottom));
    finally
      fi.Free;
    end;
     // Применяем настройки
    ApplySettings;
    ApplyLanguage;
  end;

  procedure TfMain.IniStoreSettings(const sIniFileName: String);
  var fi: TIniFile;
  begin
    fi := TIniFile.Create(sIniFileName);
    try
       // Сохраняем основные (определяемые пользователем) установки
      RootSetting.IniSave(fi);
       // Save misc settings
      fi.WriteInteger(SRegPrefs_Root, 'ViewInfoPosL', ViewInfoPos.Left);
      fi.WriteInteger(SRegPrefs_Root, 'ViewInfoPosT', ViewInfoPos.Top);
      fi.WriteInteger(SRegPrefs_Root, 'ViewInfoPosR', ViewInfoPos.Right);
      fi.WriteInteger(SRegPrefs_Root, 'ViewInfoPosB', ViewInfoPos.Bottom);
    finally
      fi.Free;
    end;
  end;

  procedure TfMain.LoadGroupTree;
  begin
    with tvGroups do begin
      BeginUpdate;
      try
        ReinitNode(FRootNode, True);
        tvGroups.Selected[FRootNode] := True;
        tvGroups.FocusedNode := FRootNode;
      finally
        EndUpdate;
      end;
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
      if GetOpGroupNode_Cache=nil then GetOpGroupNode_Cache := VTVNodeByAbsoluteIndex(tvGroups, Op.GroupAbsIdx);
      Result := GetOpGroupNode_Cache;
    end;

     // Получает узел в tvGroups, соответствующий группе ParentGroupAbsIdx операции, кэшируя результат
    function GetOpParentGroupNode: PVirtualNode;
    begin
      if GetOpParentGroupNode_Cache=nil then GetOpParentGroupNode_Cache := VTVNodeByAbsoluteIndex(tvGroups, Op.ParentGroupAbsIdx);
      Result := GetOpParentGroupNode_Cache;
    end;

  begin
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

  procedure TfMain.RefreshViewer;
  begin
    Viewer.ViewGroup(CurGroup);
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

  procedure TfMain.ToolItemClick(Sender: TObject);
  var PicLinks: TPhoaPicLinks;
  begin
     // Создаём массив ссылок на изображения
    PicLinks := GetSelectedPicLinks;
    try
       // Выполняем инструмент
      TPhoaToolSetting(TComponent(Sender).Tag).Execute(PicLinks);
    finally
      PicLinks.Free;
    end;
  end;

  procedure TfMain.tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
  begin
    if Node=FRootNode then begin
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
    if Node=FRootNode then begin
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
    Allowed := (FViewIndex<0) and (Sender.NodeParent[Node]<>nil);
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
      bCopy := (GetKeyState(VK_CONTROL) and $80<>0) or (nSrc=FSearchedNode);
      gTgt := PPhoaGroup(Sender.GetNodeData(nTgt))^;
      iCnt := Viewer.SelCount;
      iCntBefore := gTgt.PicIDs.Count;
      PerformOperation(TPhoaMultiOp_PicDragAndDropToGroup.Create(FOperations, FPhoA, CurGroup, gTgt, PicArrayToIDArray(Viewer.GetSelectedPicArray), bCopy));
      InfoIfSettingRequires(
        ConstVal(iif(bCopy, 'SNotify_DragCopy', 'SNotify_DragMove'), [iCnt, gTgt.PicIDs.Count-iCntBefore, iCnt-(gTgt.PicIDs.Count-iCntBefore)]),
        iif(bCopy, ISettingID_Dlgs_NotifyDragCopy, ISettingID_Dlgs_NotifyDragMove));
    end;
  end;

  procedure TfMain.tvGroupsDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
  const aPicCur: Array[Boolean] of TCursor = (crDragMove, crDragCopy);
  var nSrc, nTgt: PVirtualNode;
  begin
    Accept := False;
    nSrc := Sender.FocusedNode;
    nTgt := Sender.DropTargetNode;
     // Перетаскивание группы
    if Sender=Source then begin
      Effect := DROPEFFECT_MOVE;
      if (nTgt<>FSearchedNode) and (Mode in [dmAbove, dmOnNode, dmBelow]) then begin
        case Mode of
           // НАД узлом - нельзя вставлять над фотоальбомом и над следующим за nSrc узлом
          dmAbove:  Accept := (nTgt<>FRootNode) and ((nSrc.Parent<>nTgt.Parent) or (nSrc.Index<>nTgt.Index-1));
           // НА узле - нельзя таскать в родителя исходного узла
          dmOnNode: Accept := nSrc.Parent<>nTgt;
           // ПОД узлом - нельзя вставлять под фотоальбомом и под предыдущим перед nSrc узлом
          dmBelow:  Accept := (nTgt<>FRootNode) and ((nSrc.Parent<>nTgt.Parent) or (nSrc.Index<>nTgt.Index+1));
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
        (nTgt<>FSearchedNode);
      if Accept then Viewer.DragCursor := aPicCur[(nSrc=FSearchedNode) or (ssCtrl in Shift)];
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
    Allowed :=
      ((FViewIndex>=0) xor (Node<>FRootNode)) and // Когда активно дерево групп, можно редактировать любую папку, кроме
                                                  //   корневой. Иначе, когда активно представление, можно редактировать
                                                  //   только корневой узел (само представление)
      (Node<>FSearchedNode);                      // Узел результатов поиска редактировать нельзя вообще
  end;

  procedure TfMain.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Kind in [ikNormal, ikSelected] then
       // Обычная группа
      if Sender.NodeParent[Node]<>nil then ImageIndex := iiFolder
       // Узел результатов поиска
      else if Node=FSearchedNode      then ImageIndex := iiFolderSearch
       // Узел фотоальбома
      else if ViewIndex<0             then ImageIndex := iiPhoA
       // Узел представления
      else                                 ImageIndex := iiView;
  end;

  procedure TfMain.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    p: PPhoaGroup;
    s: String;
  begin
    p := Sender.GetNodeData(Node);
     // Static text
    if TextType=ttStatic then begin
      if p^.PicIDs.Count>0 then                   s := Format('(%d)', [p^.PicIDs.Count]);
     // Обычная группа
    end else if Sender.NodeParent[Node]<>nil then s := p^.Text
     // Узел результатов поиска
    else if Node=FSearchedNode      then          s := ConstVal('SSearchResultsNode')
     // Узел фотоальбома
    else if ViewIndex<0             then          s := ConstVal('SPhotoAlbumNode')
     // Узел представления
    else                                          s := FPhoA.Views[ViewIndex].Name;
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
      if ViewIndex<0 then p^ := FPhoA.RootGroup else p^ := FPhoA.Views[ViewIndex].RootGroup;
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
     // Редактировали представление?
    if (FViewIndex>=0) and (tvGroups.FocusedNode=FRootNode) then
      PerformOperation(TPhoaOp_ViewEdit.Create(FOperations, FPhoA.Views[FViewIndex], Self, UnicodetoAnsiCP(NewText, cMainCodePage), nil, nil))
     // Иначе редактировали группу
    else
      PerformOperation(TPhoaOp_GroupRename.Create(FOperations, FPhoA, CurGroup, UnicodetoAnsiCP(NewText, cMainCodePage)));
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
    if uifUReinitParent   in IFlags then OpParentGroupNode := VTVNodeByAbsoluteIndex(tvGroups, Op.ParentGroupAbsIdx);
    if uifUInvalidateNode in IFlags then OpGroupNode       := VTVNodeByAbsoluteIndex(tvGroups, Op.GroupAbsIdx);
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
    HtmlHelpShowContents;
  end;

initialization
  with FileFormatList do begin
    RegisterFileFormat('msk', '', 'Paint Shop Pro mask',          [ftRaster],           False, True, TBitmap);
    RegisterFileFormat('pfr', '', 'Paint Shop Pro picture frame', [ftRaster, ftVector], False, True, TPSPGraphic);
    RegisterFileFormat('tub', '', 'Paint Shop Pro picture tube',  [ftRaster, ftVector], False, True, TPSPGraphic);
  end;
end.
