//**********************************************************************************************************************
//  $Id: phoa.dpr,v 1.43 2005-08-15 11:25:11 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
program phoa;

uses
  Forms,
  Windows,
  dkWebUtils in '..\dkWebUtils.pas',
  ConsVars in 'ConsVars.pas',
  Main in 'Main.pas' {fMain},
  phAppIntf in 'phAppIntf.pas',
  phChmHlp in 'phChmHlp.pas',
  phDlg in 'phDlg.pas' {PhoaDialog},
  phFrm in 'phFrm.pas' {PhoaForm},
  phGraphics in 'phGraphics.pas',
  phGUIObj in 'phGUIObj.pas',
  phIJLIntf in 'phIJLIntf.pas',
  phIntf in 'phIntf.pas',
  phMetadata in 'phMetadata.pas',
  phMutableIntf in 'phMutableIntf.pas',
  phNativeIntf in 'phNativeIntf.pas',
  phObj in 'phObj.pas',
  phObjConst in 'phObjConst.pas',
  phOps in 'phOps.pas',
  phParsingPicFilter in 'phParsingPicFilter.pas',
  phPhoa in 'phPhoa.pas',
  phPicFilterHighlighter in 'phPicFilterHighlighter.pas',
  phPicPropsDlgPage in 'phPicPropsDlgPage.pas' {PicPropsDialogPage: TFrame},
  phPluginUsage in 'phPluginUsage.pas',
  phSettings in 'phSettings.pas',
  phToolSetting in 'phToolSetting.pas',
  phUtils in 'phUtils.pas',
  phValSetting in 'phValSetting.pas',
  phWizard in 'phWizard.pas' {WizardPage: TFrame},
  phWizForm in 'phWizForm.pas' {PhoaWizardForm},
  udAbout in 'udAbout.pas' {dAbout},
  udFileOpsWizard in 'Wizard_FileOps\udFileOpsWizard.pas' {dFileOpsWizard},
  udGroupProps in 'udGroupProps.pas' {dGroupProps},
  phMsgBox in 'phMsgBox.pas' {dMsgBox},
  udPicOps in 'udPicOps.pas' {dPicOps},
  udPicProps in 'Dialog_PicProps\udPicProps.pas' {dPicProps},
  udProjectProps in 'udProjectProps.pas' {dProjectProps},
  udSearch in 'udSearch.pas' {dSearch},
  udSelKeywords in 'udSelKeywords.pas' {dSelKeywords},
  udSelPhoaGroup in 'udSelPhoaGroup.pas' {dSelPhoaGroup},
  udSettings in 'udSettings.pas' {dSettings},
  udSortPics in 'udSortPics.pas' {dSortPics},
  udStats in 'udStats.pas' {dStats},
  udToolProps in 'udToolProps.pas' {dToolProps},
  udViewProps in 'udViewProps.pas' {dViewProps},
  ufAddFilesWizard in 'Wizard_AddFiles\ufAddFilesWizard.pas' {fAddFilesWizard},
  ufImgView in 'ufImgView.pas' {fImgView},
  ufrExprPicFilter in 'ufrExprPicFilter.pas' {frExprPicFilter: TFrame},
  ufrPicProps_Data in 'Dialog_PicProps\ufrPicProps_Data.pas' {frPicProps_Data: TFrame},
  ufrPicProps_FileProps in 'Dialog_PicProps\ufrPicProps_FileProps.pas' {frPicProps_FileProps: TFrame},
  ufrPicProps_Groups in 'Dialog_PicProps\ufrPicProps_Groups.pas' {frPicProps_Groups: TFrame},
  ufrPicProps_Keywords in 'Dialog_PicProps\ufrPicProps_Keywords.pas' {frPicProps_Keywords: TFrame},
  ufrPicProps_Metadata in 'Dialog_PicProps\ufrPicProps_Metadata.pas' {frPicProps_Metadata: TFrame},
  ufrPicProps_View in 'Dialog_PicProps\ufrPicProps_View.pas' {frPicProps_View: TFrame},
  ufrSorting in 'ufrSorting.pas' {frSorting: TFrame},
  ufrWzPage_Log in 'ufrWzPage_Log.pas' {frWzPage_Log: TFrame},
  ufrWzPage_Processing in 'ufrWzPage_Processing.pas' {frWzPage_Processing: TFrame},
  ufrWzPageAddFiles_CheckFiles in 'Wizard_AddFiles\ufrWzPageAddFiles_CheckFiles.pas' {frWzPageAddFiles_CheckFiles: TFrame},
  ufrWzPageAddFiles_SelFiles in 'Wizard_AddFiles\ufrWzPageAddFiles_SelFiles.pas' {frWzPageAddFiles_SelFiles: TFrame},
  ufrWzPageFileOps_CDOptions in 'Wizard_FileOps\ufrWzPageFileOps_CDOptions.pas' {frWzPageFileOps_CDOptions: TFrame},
  ufrWzPageFileOps_DelOptions in 'Wizard_FileOps\ufrWzPageFileOps_DelOptions.pas' {frWzPageFileOps_DelOptions: TFrame},
  ufrWzPageFileOps_MoveOptions in 'Wizard_FileOps\ufrWzPageFileOps_MoveOptions.pas' {frWzPageFileOps_MoveOptions: TFrame},
  ufrWzPageFileOps_MoveOptions2 in 'Wizard_FileOps\ufrWzPageFileOps_MoveOptions2.pas' {frWzPageFileOps_MoveOptions2: TFrame},
  ufrWzPageFileOps_RepairOptions in 'Wizard_FileOps\ufrWzPageFileOps_RepairOptions.pas' {frWzPageFileOps_RepairOptions: TFrame},
  ufrWzPageFileOps_RepairSelLinks in 'Wizard_FileOps\ufrWzPageFileOps_RepairSelLinks.pas' {frWzPageFileOps_RepairSelLinks: TFrame},
  ufrWzPageFileOps_SelFolder in 'Wizard_FileOps\ufrWzPageFileOps_SelFolder.pas' {frWzPageFileOps_SelFolder: TFrame},
  ufrWzPageFileOps_SelPics in 'Wizard_FileOps\ufrWzPageFileOps_SelPics.pas' {frWzPageFileOps_SelPics: TFrame},
  ufrWzPageFileOps_SelTask in 'Wizard_FileOps\ufrWzPageFileOps_SelTask.pas' {frWzPageFileOps_SelTask: TFrame};

{$R *.res}

var
  hMtx: THandle;

begin
   //-------------------------------------------------------------------------------------------------------------------
   // Инициализация
   //-------------------------------------------------------------------------------------------------------------------
   // Создаём мьютекс, свидетельствующий о том, что программа запущена (используется инсталлером)
  hMtx := CreateMutex(nil, False, 'PHOA_RUNNING_MUTEX');
   // Инициализируем и загружаем настройки
  InitSettings;
  LoadAllSettings;
   // Отображаем окно прогресса
  if SettingValueBool(ISettingID_Dlgs_SplashStartShow) then CreateProgressWnd;
  ShowProgressInfo('SMsg_Initializing', []);
   // Составляем список доступных языков
  InitLanguages;
   // Загружаем плагины
  ShowProgressInfo('SMsg_ScanningPlugins', []);
  PluginsInitialize;
   // Инициализируем приложение
  ShowProgressInfo('SMsg_Starting', []);
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  PluginModules.AppInitialized(fMain);
   // Создаём плагины режима обзора
  PluginModules.CreatePlugins([ppkBrowseMode]); 
   //-------------------------------------------------------------------------------------------------------------------
   // Выполняем приложение
   //-------------------------------------------------------------------------------------------------------------------
  Application.Run;
   //-------------------------------------------------------------------------------------------------------------------
   // Финализация
   //-------------------------------------------------------------------------------------------------------------------
   // Уничтожаем все плагины
  PluginModules.ReleasePlugins(PluginKinds_All);
   // Уведомляем модули
  PluginModules.AppFinalizing;
   // Сохраняем настройки
  SaveAllSettings;
   // Выгружаем плагины
  PluginsFinalize;
   // Уничтожаем мьютекс
  if hMtx<>0 then CloseHandle(hMtx);
end.
