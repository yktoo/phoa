//**********************************************************************************************************************
//  $Id: udPicProps.pas,v 1.17 2004-12-31 13:38:58 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udPicProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Registry,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, phMetadata, GR32_Layers, phWizard,
  phDlg, Menus, TB2Item, TBX, ImgList, DKLang, TB2Dock, TB2Toolbar,
  StdCtrls, ExtCtrls;

type
  TdPicProps = class(TPhoaDialog, IWizardHostForm)
    bPgData: TTBXItem;
    bPgFileProps: TTBXItem;
    bPgGroups: TTBXItem;
    bPgKeywords: TTBXItem;
    bPgMetadata: TTBXItem;
    bPgView: TTBXItem;
    dklcMain: TDKLanguageController;
    dkNav: TTBXDock;
    ilFiles: TImageList;
    pMain: TPanel;
    pmNav: TTBXPopupMenu;
    pPages: TPanel;
    tbNav: TTBXToolbar;
    procedure PageButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
     // Контроллер страниц
    FController: TWizardController;
     // ID последней использованной (видимой) страницы
    FLastUsedPageID: Integer;
     // Список ImageIndeices файлов из системного ImageList'а
    FFileImageIndices: Array of Integer;
     // Список файлов изображений
    FPictureFiles: TStringList;
     // Список операций для отмены редактирования/добавления
    FUndoOperations: TPhoaOperations;
     // Prop storage
    FApp: IPhotoAlbumApp;
    FEditedPics: IPhotoAlbumPicList;
     // Фокусирует первый контрол на текущей странице
    procedure FocusFirstPageControl;
     // IWizardHostForm
    function  WizHost_PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean;
    procedure WizHost_PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer);
    procedure WizHost_StatusChanged;
    function  WizHost_GetHostControl: TWinControl;
    function  WizHost_GetNextPageID: Integer;
    function  WizHost_GetStorageForm: TForm;
    function  IWizardHostForm.PageChanging   = WizHost_PageChanging;
    procedure IWizardHostForm.PageChanged    = WizHost_PageChanged;
    procedure IWizardHostForm.StatusChanged  = WizHost_StatusChanged;
    function  IWizardHostForm.GetHostControl = WizHost_GetHostControl;
    function  IWizardHostForm.GetNextPageID  = WizHost_GetNextPageID;
    function  IWizardHostForm.GetStorageForm = WizHost_GetStorageForm;
     // Prop handlers
    function  GetFileImageIndex(Index: Integer): Integer;
    function  GetPictureFiles(Index: Integer): String;
    procedure SetPictureFiles(Index: Integer; const Value: String);
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
    procedure SettingsStore(rif: TRegIniFile); override;
    procedure SettingsRestore(rif: TRegIniFile); override;
    function  GetFormRegistrySection: String; override;
    function  GetSizeable: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
     // Ищет изображение по имени файла и возвращает его ID, если нашла, иначе возвращает 0. Поиск осуществляется с
     //   учётом новых имён файлов, заданных в диалоге, но среди ВСЕХ изображений проекта
    function  FindPicIDByFileName(const sFileName: String): Integer;
     // Props
     // -- Приложение
    property App: IPhotoAlbumApp read FApp;
     // -- Редактируемые изображения
    property EditedPics: IPhotoAlbumPicList read FEditedPics;
     // -- ImageIndices файлов редактируемых изображений
    property FileImageIndex[Index: Integer]: Integer read GetFileImageIndex;
     // -- Имена файлов редактируемых изображений
    property PictureFiles[Index: Integer]: String read GetPictureFiles write SetPictureFiles;
  end;

  function EditPics(AApp: IPhotoAlbumApp; AEditedPics: IPhotoAlbumPicList; AUndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses
  ShellAPI,
  phUtils, phSettings, ConsVars, Main,
  phPicPropsDlgPage, ufrPicProps_FileProps, ufrPicProps_Metadata, ufrPicProps_View, ufrPicProps_Data,
  ufrPicProps_Keywords, ufrPicProps_Groups;

  function EditPics(AApp: IPhotoAlbumApp; AEditedPics: IPhotoAlbumPicList; AUndoOperations: TPhoaOperations): Boolean;
  begin
    with TdPicProps.Create(Application) do
      try
        FApp            := AApp;
        FEditedPics     := AEditedPics;
        FUndoOperations := AUndoOperations;
        Result := Execute;
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TfPicProps
   //===================================================================================================================

  procedure TdPicProps.ButtonClick_OK;
  var
    i, idxMainParam: Integer;
    sOpParam: String;
    OpParams: IPhoaOperationParams;
    aMainOpParams: Array of Variant;
  begin
     // Проверяем последовательно все страницы
    for i := 0 to FController.Count-1 do
      if not (FController[i] as TPicPropsDialogPage).CanApply then begin
        FController.SetVisiblePageID(FController[i].ID, pcmForced);
        Exit;
      end;
     // Применяем последовательно все страницы, формируя массив параметров главной операции (PicEdit)
    aMainOpParams := nil;
    idxMainParam  := -1;
    for i := 0 to FController.Count-1 do begin
      sOpParam := '';
      OpParams := nil;
      TPicPropsDialogPage(FController[i]).Apply(sOpParam, OpParams);
      if sOpParam<>'' then begin
        SetLength(aMainOpParams, idxMainParam+3);
        Inc(idxMainParam);
        aMainOpParams[idxMainParam] := sOpParam;
        Inc(idxMainParam);
        aMainOpParams[idxMainParam] := OpParams;
      end;
    end;
     // Выполняем операцию
    FApp.PerformOperation('PicEdit', aMainOpParams);
     // Закрываем окошко
    inherited ButtonClick_OK;
  end;

  constructor TdPicProps.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FPictureFiles := TStringList.Create;
     // Создаём контроллер страниц
    FController := TWizardController.Create(Self);
    with FController do begin
      KeepHistory := False;
       // Создаём страницы
      CreatePage(TfrPicProps_FileProps, IDlgPicPropsPageID_FileProps, IDH_intf_pic_props_fprops,   '');
      CreatePage(TfrPicProps_Metadata,  IDlgPicPropsPageID_Metadata,  IDH_intf_pic_props_metadata, '');
      CreatePage(TfrPicProps_View,      IDlgPicPropsPageID_View,      IDH_intf_pic_props_view,     '');
      CreatePage(TfrPicProps_Data,      IDlgPicPropsPageID_Data,      IDH_intf_pic_props_data,     '');
      CreatePage(TfrPicProps_Keywords,  IDlgPicPropsPageID_Keywords,  IDH_intf_pic_props_keywords, '');
      CreatePage(TfrPicProps_Groups,    IDlgPicPropsPageID_Groups,    IDH_intf_pic_props_groups,   '');
    end;
  end;

  destructor TdPicProps.Destroy;
  begin
    FController.Free;
    FPictureFiles.Free;
    inherited Destroy;
  end;

type TWinControlCast = class(TWinControl);

  function TdPicProps.FindPicIDByFileName(const sFileName: String): Integer;
  var
    i, iPicID: Integer;
    ProjPics: IPhotoAlbumPicList;
  begin
     // Сначала ищем среди редактируемых изображений
    for i := 0 to FPictureFiles.Count-1 do
      if ReverseCompare(FPictureFiles[i], sFileName) then begin
        Result := EditedPics[i].ID;
        Exit;
      end;
     // Если не нашли - ищем среди всех изображений фотоальбома
    Result := 0;
    ProjPics := App.Project.PicsX;
    for i := 0 to ProjPics.Count-1 do
       // Нашли такой файл
      if ReverseCompare(ProjPics[i].FileName, sFileName) then begin
         // Если изображение находится в числе редактируемых, значит, файл ему изменили (иначе мы нашли бы его ранее
         //   поиском по редактируемым изображениям) и изображения с таким файлом более не существует
        iPicID := ProjPics[i].ID;
        if EditedPics.IndexOfID(iPicID)<0 then Result := iPicID;
        Break;
      end;
  end;

  procedure TdPicProps.FocusFirstPageControl;
  begin
    if Visible and (FController.VisiblePage<>nil) then TWinControlCast(FController.VisiblePage).SelectFirst;
  end;

  procedure TdPicProps.FormShow(Sender: TObject);
  begin
    FocusFirstPageControl;
  end;

  function TdPicProps.GetFileImageIndex(Index: Integer): Integer;
  var
    FileInfo: TSHFileInfo;
    pImgIdx: PInteger;
  begin
     // Если ImageIndex=-1 - это значит, он ещё не считывался
    pImgIdx := @FFileImageIndices[Index];
    if pImgIdx^=-1 then begin
      SHGetFileInfo(PAnsiChar(FPictureFiles[Index]), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
      pImgIdx^ := FileInfo.iIcon;
    end;
    Result := pImgIdx^;
  end;

  function TdPicProps.GetFormRegistrySection: String;
  begin
    Result := SRegPicProps_Root;
  end;

  function TdPicProps.GetPictureFiles(Index: Integer): String;
  begin
    Result := FPictureFiles[Index];
  end;

  function TdPicProps.GetSizeable: Boolean;
  begin
    Result := True;
  end;

  procedure TdPicProps.InitializeDialog;
  var
    i, iPageID: Integer;
    FileInfo: TSHFileInfo;
  begin
    inherited InitializeDialog;
    pmNav.LinkSubitems := tbNav.Items;
    if FEditedPics.Count>0 then begin
       // Инициализируем список индексов изображений файлов в -1 ("ImageIndex не считан") и копируем список файлов
      SetLength(FFileImageIndices, FEditedPics.Count);
      for i := 0 to FEditedPics.Count-1 do begin
        FFileImageIndices[i] := -1;
        FPictureFiles.Add(FEditedPics[i].FileName);
      end;
       // Получаем Handle системного ImageList-а
      ilFiles.Handle := SHGetFileInfo(PAnsiChar(FPictureFiles[0]), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    end;
     // Определяем требуемую начальную страницу
    case TPicPropsDlgDefaultPage(SettingValueInt(ISettingID_Dlgs_PP_DefaultPage)) of
      ppddpFileProps:      iPageID := IDlgPicPropsPageID_FileProps;
      ppddpMetadata:       iPageID := IDlgPicPropsPageID_Metadata;
      ppddpView:           iPageID := IDlgPicPropsPageID_View;
      ppddpData:           iPageID := IDlgPicPropsPageID_Data;
      ppddpKeywords:       iPageID := IDlgPicPropsPageID_Keywords;
      ppddpGroups:         iPageID := IDlgPicPropsPageID_Groups;
      else {ppddpLastUsed} iPageID := FLastUsedPageID;
    end;
     // Если не удалось определить допустимый ID страницы - пусть это будет страница "Данные"
    if (iPageID=0) or (FController.IndexOfID(iPageID)<0) then iPageID := IDlgPicPropsPageID_Data;
     // Выставляем начальную страницу
    FController.SetVisiblePageID(iPageID, pcmForced);
  end;

  procedure TdPicProps.PageButtonClick(Sender: TObject);
  begin
    FController.SetVisiblePageID(TComponent(Sender).Tag+1, pcmForced);
  end;

  procedure TdPicProps.SetPictureFiles(Index: Integer; const Value: String);
  var i: Integer;
  begin
    if FPictureFiles[Index]<>Value then begin
      FPictureFiles[Index] := Value;
       // Сбрасываем ImageIndex
      FFileImageIndices[Index] := -1;
       // Уведомляем все страницы об изменении файла
      for i := 0 to FController.Count-1 do TPicPropsDialogPage(FController[i]).FileChanged(Index);
      Modified := True;
    end;
  end;

  procedure TdPicProps.SettingsRestore(rif: TRegIniFile);
  begin
    inherited SettingsRestore(rif);
    FLastUsedPageID := rif.ReadInteger('', 'LastUsedPageID', 0);
  end;

  procedure TdPicProps.SettingsStore(rif: TRegIniFile);
  begin
    inherited SettingsStore(rif);
    rif.WriteInteger('', 'LastUsedPageID', FController.VisiblePageID);
  end;

  function TdPicProps.WizHost_GetHostControl: TWinControl;
  begin
    Result := pPages;
  end;

  function TdPicProps.WizHost_GetNextPageID: Integer;
  begin
    Result := 0;
  end;

  function TdPicProps.WizHost_GetStorageForm: TForm;
  begin
    Result := Self;
  end;

  procedure TdPicProps.WizHost_PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer);
  var i, iPageID: Integer;
  begin
     // Настраиваем "нажатость" кнопок выбора страниц
    iPageID := FController.VisiblePageID;
    for i := 0 to tbNav.Items.Count-1 do
      with tbNav.Items[i] do Checked := Tag=iPageID-1;
     // Фокусируем первый контрол
    FocusFirstPageControl;
  end;

  function TdPicProps.WizHost_PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean;
  begin
    Result := True;
  end;

  procedure TdPicProps.WizHost_StatusChanged;
  begin
    { does nothing }
  end;

end.

