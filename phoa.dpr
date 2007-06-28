//**********************************************************************************************************************
//  $Id: phoa.dpr,v 1.46 2007-06-28 18:41:37 dale Exp $
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
  phFrm in 'phFrm.pas' {PhoaForm: TTntForm},
  phGraphics in 'phGraphics.pas',
  phGUIObj in 'phGUIObj.pas',
  phIJLIntf in 'phIJLIntf.pas',
  phIntf in 'phIntf.pas',
  phMetadata in 'phMetadata.pas',
  phMsgBox in 'phMsgBox.pas' {dMsgBox: TTntForm},
  phMutableIntf in 'phMutableIntf.pas',
  phNativeIntf in 'phNativeIntf.pas',
  phObj in 'phObj.pas',
  phObjConst in 'phObjConst.pas',
  phOps in 'phOps.pas',
  phParsingPicFilter in 'phParsingPicFilter.pas',
  phPhoa in 'phPhoa.pas',
  phPicFilterHighlighter in 'phPicFilterHighlighter.pas',
  phPicPropsDlgPage in 'phPicPropsDlgPage.pas' {PicPropsDialogPage: TTntFrame},
  phPluginUsage in 'phPluginUsage.pas',
  phSettings in 'phSettings.pas',
  phToolSetting in 'phToolSetting.pas',
  phUtils in 'phUtils.pas',
  phValSetting in 'phValSetting.pas',
  phWizard in 'phWizard.pas' {WizardPage: TTntFrame},
  phWizForm in 'phWizForm.pas' {PhoaWizardForm},
  udAbout in 'udAbout.pas' {dAbout},
  udFileOpsWizard in 'Wizard_FileOps\udFileOpsWizard.pas' {dFileOpsWizard},
  udGroupProps in 'udGroupProps.pas' {dGroupProps},
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
  ufrExprPicFilter in 'ufrExprPicFilter.pas' {frExprPicFilter: TTntFrame},
  ufrPicProps_Data in 'Dialog_PicProps\ufrPicProps_Data.pas' {frPicProps_Data: TTntFrame},
  ufrPicProps_FileProps in 'Dialog_PicProps\ufrPicProps_FileProps.pas' {frPicProps_FileProps: TTntFrame},
  ufrPicProps_Groups in 'Dialog_PicProps\ufrPicProps_Groups.pas' {frPicProps_Groups: TTntFrame},
  ufrPicProps_Keywords in 'Dialog_PicProps\ufrPicProps_Keywords.pas' {frPicProps_Keywords: TTntFrame},
  ufrPicProps_Metadata in 'Dialog_PicProps\ufrPicProps_Metadata.pas' {frPicProps_Metadata: TTntFrame},
  ufrPicProps_View in 'Dialog_PicProps\ufrPicProps_View.pas' {frPicProps_View: TTntFrame},
  ufrSorting in 'ufrSorting.pas' {frSorting: TTntFrame},
  ufrWzPage_Log in 'ufrWzPage_Log.pas' {frWzPage_Log: TTntFrame},
  ufrWzPage_Processing in 'ufrWzPage_Processing.pas' {frWzPage_Processing: TTntFrame},
  ufrWzPageAddFiles_CheckFiles in 'Wizard_AddFiles\ufrWzPageAddFiles_CheckFiles.pas' {frWzPageAddFiles_CheckFiles: TTntFrame},
  ufrWzPageAddFiles_SelFiles in 'Wizard_AddFiles\ufrWzPageAddFiles_SelFiles.pas' {frWzPageAddFiles_SelFiles: TTntFrame},
  ufrWzPageFileOps_CDOptions in 'Wizard_FileOps\ufrWzPageFileOps_CDOptions.pas' {frWzPageFileOps_CDOptions: TTntFrame},
  ufrWzPageFileOps_DelOptions in 'Wizard_FileOps\ufrWzPageFileOps_DelOptions.pas' {frWzPageFileOps_DelOptions: TTntFrame},
  ufrWzPageFileOps_MoveOptions in 'Wizard_FileOps\ufrWzPageFileOps_MoveOptions.pas' {frWzPageFileOps_MoveOptions: TTntFrame},
  ufrWzPageFileOps_MoveOptions2 in 'Wizard_FileOps\ufrWzPageFileOps_MoveOptions2.pas' {frWzPageFileOps_MoveOptions2: TTntFrame},
  ufrWzPageFileOps_RepairOptions in 'Wizard_FileOps\ufrWzPageFileOps_RepairOptions.pas' {frWzPageFileOps_RepairOptions: TTntFrame},
  ufrWzPageFileOps_RepairSelLinks in 'Wizard_FileOps\ufrWzPageFileOps_RepairSelLinks.pas' {frWzPageFileOps_RepairSelLinks: TTntFrame},
  ufrWzPageFileOps_SelFolder in 'Wizard_FileOps\ufrWzPageFileOps_SelFolder.pas' {frWzPageFileOps_SelFolder: TTntFrame},
  ufrWzPageFileOps_SelPics in 'Wizard_FileOps\ufrWzPageFileOps_SelPics.pas' {frWzPageFileOps_SelPics: TTntFrame},
  ufrWzPageFileOps_SelTask in 'Wizard_FileOps\ufrWzPageFileOps_SelTask.pas' {frWzPageFileOps_SelTask: TTntFrame};

{$R *.res}
{$R *.dkl_const.res}  

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
  PluginModules.InitializeAll(fMain);
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
  PluginModules.FinalizeAll;
   // Сохраняем настройки
  SaveAllSettings;
   // Выгружаем плагины
  PluginsFinalize;
   // Уничтожаем мьютекс
  if hMtx<>0 then CloseHandle(hMtx);
end.
