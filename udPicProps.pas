//**********************************************************************************************************************
//  $Id: udPicProps.pas,v 1.11 2004-10-15 13:49:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udPicProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, phMetadata,
  GR32_Layers, phWizard,
  phDlg, Menus, TB2Item, TBX, ImgList, Placemnt, TB2Dock, TB2Toolbar,
  ExtCtrls, StdCtrls, DKLang;

type
  TdPicProps = class(TPhoaDialog, IWizardHostForm)
    dklcMain: TDKLanguageController;
    fpMain: TFormPlacement;
    pMain: TPanel;
    dkNav: TTBXDock;
    tbNav: TTBXToolbar;
    bPgFileProps: TTBXItem;
    bPgMetadata: TTBXItem;
    bPgView: TTBXItem;
    bPgData: TTBXItem;
    bPgKeywords: TTBXItem;
    bPgGroups: TTBXItem;
    ilFiles: TImageList;
    pPages: TPanel;
    pmNav: TTBXPopupMenu;
    procedure PageButtonClick(Sender: TObject);
  private
     // Контроллер страниц
    FController: TWizardController;
     // Список ImageIndeices файлов из системного ImageList'а
    FFileImageIndices: Array of Integer;
     // Список операций для отмены редактирования/добавления
    FUndoOperations: TPhoaOperations;
     // Prop storage
    FApp: IPhotoAlbumApp;
    FEditedPics: IPhotoAlbumPicList;
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
  protected
    procedure InitializeDialog; override;
    procedure FinalizeDialog; override;
    procedure ButtonClick_OK; override;
  public
     // Props
     // -- Приложение
    property App: IPhotoAlbumApp read FApp;
     // -- Редактируемые изображения
    property EditedPics: IPhotoAlbumPicList read FEditedPics;
     // -- ImageIndices файлов редактируемых изображений
    property FileImageIndex[Index: Integer]: Integer read GetFileImageIndex;
  end;

  function EditPics(AApp: IPhotoAlbumApp; AEditedPics: IPhotoAlbumPicList; AUndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses
  ShellAPI,
  phUtils, ConsVars, Main,
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
    i: Integer;
    Operation: TPhoaOp_PicEdit;
    Changes: TPhoaOperationChanges;
  begin
    Changes := [];
    fMain.BeginOperation;
    try
       // Проверяем последовательно все страницы
      for i := 0 to FController.Count-1 do
        if not (FController[i] as TPicPropsDialogPage).CanApply then begin
          FController.SetVisiblePageID(FController[i].ID, pcmForced);
          Exit;
        end;
       // Создаём операцию отката
      Operation := TPhoaOp_PicEdit.Create(FUndoOperations, FApp.Project, Changes);
       // Применяем последовательно все страницы
      for i := 0 to FController.Count-1 do TPicPropsDialogPage(FController[i]).Apply(Operation.Operations, Changes);
    finally
      fMain.EndOperation(Changes);
    end;
    inherited ButtonClick_OK;
  end;

  procedure TdPicProps.FinalizeDialog;
  begin
    FController.Free;
    inherited FinalizeDialog;
  end;

  function TdPicProps.GetFileImageIndex(Index: Integer): Integer;
  begin
    Result := FFileImageIndices[Index];
  end;

  procedure TdPicProps.InitializeDialog;
  var
    i: Integer;
    HSysIL: THandle;
    FileInfo: TSHFileInfo;
  begin
    inherited InitializeDialog;
    MakeSizeable;
     // Настраиваем fpMain
    fpMain.IniFileName := SRegRoot;
    fpMain.IniSection  := SRegPicProps_Root;
    pmNav.LinkSubitems := tbNav.Items;
     // Считываем ImageIndices файлов изображений
    SetLength(FFileImageIndices, FEditedPics.Count);
    HSysIL := 0;
    for i := 0 to FEditedPics.Count-1 do begin
      HSysIL := SHGetFileInfo(PAnsiChar(FEditedPics[i].FileName), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
      FFileImageIndices[i] := FileInfo.iIcon;
    end;
    ilFiles.Handle := HSysIL;
     // Создаём контроллер страниц
    FController := TWizardController.Create(Self);
    with FController do begin
      KeepHistory := False;
       // Создаём страницы и отображаем первую страницу
      CreatePage(TfrPicProps_FileProps, IDlgPicPropsPageID_FileProps, IDH_intf_pic_props_fprops,   '');
      CreatePage(TfrPicProps_Metadata,  IDlgPicPropsPageID_Metadata,  IDH_intf_pic_props_metadata, '');
      CreatePage(TfrPicProps_View,      IDlgPicPropsPageID_View,      IDH_intf_pic_props_view,     '');
      CreatePage(TfrPicProps_Data,      IDlgPicPropsPageID_Data,      IDH_intf_pic_props_data,     '');
      CreatePage(TfrPicProps_Keywords,  IDlgPicPropsPageID_Keywords,  IDH_intf_pic_props_keywords, '');
      CreatePage(TfrPicProps_Groups,    IDlgPicPropsPageID_Groups,    IDH_intf_pic_props_groups,   '');
      SetVisiblePageID(IDlgPicPropsPageID_Data, pcmForced);
    end;
  end;

  procedure TdPicProps.PageButtonClick(Sender: TObject);
  begin
    FController.SetVisiblePageID(TComponent(Sender).Tag+1, pcmForced);
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

