//**********************************************************************************************************************
//  $Id: phoa.dpr,v 1.39 2005-02-26 12:35:51 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
program phoa;

uses
  Forms,
  Windows,
  dkWebUtils in '..\dkWebUtils.pas',
  ChmHlp in 'ChmHlp.pas',
  ConsVars in 'ConsVars.pas',
  Main in 'Main.pas' {fMain},
  phAppIntf in 'phAppIntf.pas',
  phDlg in 'phDlg.pas' {PhoaDialog},
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
  phPlugin in 'phPlugin.pas',
  phPluginUsage in 'phPluginUsage.pas',
  phSettings in 'phSettings.pas',
  phToolSetting in 'phToolSetting.pas',
  phUtils in 'phUtils.pas',
  phValSetting in 'phValSetting.pas',
  phWizard in 'phWizard.pas' {WizardPage: TFrame},
  phWizForm in 'phWizForm.pas' {PhoaWizardForm},
  udAbout in 'udAbout.pas' {dAbout},
  udFileOpsWizard in 'udFileOpsWizard.pas' {dFileOpsWizard},
  udGroupProps in 'udGroupProps.pas' {dGroupProps},
  udMsgBox in 'udMsgBox.pas' {dMsgBox},
  udPicOps in 'udPicOps.pas' {dPicOps},
  udPicProps in 'udPicProps.pas' {dPicProps},
  udProjectProps in 'udProjectProps.pas' {dProjectProps},
  udSearch in 'udSearch.pas' {dSearch},
  udSelKeywords in 'udSelKeywords.pas' {dSelKeywords},
  udSelPhoaGroup in 'udSelPhoaGroup.pas' {dSelPhoaGroup},
  udSettings in 'udSettings.pas' {dSettings},
  udSortPics in 'udSortPics.pas' {dSortPics},
  udStats in 'udStats.pas' {dStats},
  udToolProps in 'udToolProps.pas' {dToolProps},
  udViewProps in 'udViewProps.pas' {dViewProps},
  ufAddFilesWizard in 'ufAddFilesWizard.pas' {fAddFilesWizard},
  ufImgView in 'ufImgView.pas' {fImgView},
  ufrExprPicFilter in 'ufrExprPicFilter.pas' {frExprPicFilter: TFrame},
  ufrPicProps_Data in 'ufrPicProps_Data.pas' {frPicProps_Data: TFrame},
  ufrPicProps_FileProps in 'ufrPicProps_FileProps.pas' {frPicProps_FileProps: TFrame},
  ufrPicProps_Groups in 'ufrPicProps_Groups.pas' {frPicProps_Groups: TFrame},
  ufrPicProps_Keywords in 'ufrPicProps_Keywords.pas' {frPicProps_Keywords: TFrame},
  ufrPicProps_Metadata in 'ufrPicProps_Metadata.pas' {frPicProps_Metadata: TFrame},
  ufrPicProps_View in 'ufrPicProps_View.pas' {frPicProps_View: TFrame},
  ufrSorting in 'ufrSorting.pas' {frSorting: TFrame},
  ufrWzPage_Log in 'ufrWzPage_Log.pas' {frWzPage_Log: TFrame},
  ufrWzPage_Processing in 'ufrWzPage_Processing.pas' {frWzPage_Processing: TFrame},
  ufrWzPageAddFiles_CheckFiles in 'ufrWzPageAddFiles_CheckFiles.pas' {frWzPageAddFiles_CheckFiles: TFrame},
  ufrWzPageAddFiles_SelFiles in 'ufrWzPageAddFiles_SelFiles.pas' {frWzPageAddFiles_SelFiles: TFrame},
  ufrWzPageFileOps_CDOptions in 'ufrWzPageFileOps_CDOptions.pas' {frWzPageFileOps_CDOptions: TFrame},
  ufrWzPageFileOps_DelOptions in 'ufrWzPageFileOps_DelOptions.pas' {frWzPageFileOps_DelOptions: TFrame},
  ufrWzPageFileOps_MoveOptions in 'ufrWzPageFileOps_MoveOptions.pas' {frWzPageFileOps_MoveOptions: TFrame},
  ufrWzPageFileOps_MoveOptions2 in 'ufrWzPageFileOps_MoveOptions2.pas' {frWzPageFileOps_MoveOptions2: TFrame},
  ufrWzPageFileOps_RepairOptions in 'ufrWzPageFileOps_RepairOptions.pas' {frWzPageFileOps_RepairOptions: TFrame},
  ufrWzPageFileOps_RepairSelLinks in 'ufrWzPageFileOps_RepairSelLinks.pas' {frWzPageFileOps_RepairSelLinks: TFrame},
  ufrWzPageFileOps_SelFolder in 'ufrWzPageFileOps_SelFolder.pas' {frWzPageFileOps_SelFolder: TFrame},
  ufrWzPageFileOps_SelPics in 'ufrWzPageFileOps_SelPics.pas' {frWzPageFileOps_SelPics: TFrame},
  ufrWzPageFileOps_SelTask in 'ufrWzPageFileOps_SelTask.pas' {frWzPageFileOps_SelTask: TFrame};

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
   //-------------------------------------------------------------------------------------------------------------------
   // Выполняем приложение
   //-------------------------------------------------------------------------------------------------------------------
  Application.Run;
   //-------------------------------------------------------------------------------------------------------------------
   // Финализация
   //-------------------------------------------------------------------------------------------------------------------
  PluginModules.AppFinalizing;
   // Сохраняем настройки
  SaveAllSettings;
   // Выгружаем плагины
  PluginsFinalize;
   // Уничтожаем мьютекс
  if hMtx<>0 then CloseHandle(hMtx);
end.
