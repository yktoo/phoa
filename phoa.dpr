//**********************************************************************************************************************
//  $Id: phoa.dpr,v 1.36 2005-02-13 19:16:38 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
program phoa;

uses
  Forms,
  Windows,
  dkWebUtils in '..\dkWebUtils.pas',
  phObj in 'phObj.pas',
  phPhoa in 'phPhoa.pas',
  phMetadata in 'phMetadata.pas',
  ConsVars in 'ConsVars.pas',
  phUtils in 'phUtils.pas',
  Main in 'Main.pas' {fMain},
  udPicProps in 'udPicProps.pas' {dPicProps},
  udSettings in 'udSettings.pas' {dSettings},
  ufImgView in 'ufImgView.pas' {fImgView},
  udSearch in 'udSearch.pas' {dSearch},
  udProjectProps in 'udProjectProps.pas' {dProjectProps},
  udAbout in 'udAbout.pas' {dAbout},
  ChmHlp in 'ChmHlp.pas',
  udPicOps in 'udPicOps.pas' {dPicOps},
  udSortPics in 'udSortPics.pas' {dSortPics},
  udSelKeywords in 'udSelKeywords.pas' {dSelKeywords},
  udViewProps in 'udViewProps.pas' {dViewProps},
  udSelPhoaGroup in 'udSelPhoaGroup.pas' {dSelPhoaGroup},
  phDlg in 'phDlg.pas' {PhoaDialog},
  ufAddFilesWizard in 'ufAddFilesWizard.pas' {fAddFilesWizard},
  ufrWzPageAddFiles_SelFiles in 'ufrWzPageAddFiles_SelFiles.pas' {frWzPageAddFiles_SelFiles: TFrame},
  phWizard in 'phWizard.pas' {WizardPage: TFrame},
  phWizForm in 'phWizForm.pas' {PhoaWizardForm},
  ufrWzPageAddFiles_CheckFiles in 'ufrWzPageAddFiles_CheckFiles.pas' {frWzPageAddFiles_CheckFiles: TFrame},
  ufrWzPage_Processing in 'ufrWzPage_Processing.pas' {frWzPage_Processing: TFrame},
  ufrPicProps_FileProps in 'ufrPicProps_FileProps.pas' {frPicProps_FileProps: TFrame},
  ufrPicProps_Metadata in 'ufrPicProps_Metadata.pas' {frPicProps_Metadata: TFrame},
  ufrPicProps_View in 'ufrPicProps_View.pas' {frPicProps_View: TFrame},
  ufrPicProps_Data in 'ufrPicProps_Data.pas' {frPicProps_Data: TFrame},
  ufrPicProps_Keywords in 'ufrPicProps_Keywords.pas' {frPicProps_Keywords: TFrame},
  ufrPicProps_Groups in 'ufrPicProps_Groups.pas' {frPicProps_Groups: TFrame},
  phPicPropsDlgPage in 'phPicPropsDlgPage.pas' {PicPropsDialogPage: TFrame},
  ufrWzPage_Log in 'ufrWzPage_Log.pas' {frWzPage_Log: TFrame},
  ufrSorting in 'ufrSorting.pas' {frSorting: TFrame},
  udStats in 'udStats.pas' {dStats},
  udFileOpsWizard in 'udFileOpsWizard.pas' {dFileOpsWizard},
  ufrWzPageFileOps_SelTask in 'ufrWzPageFileOps_SelTask.pas' {frWzPageFileOps_SelTask: TFrame},
  ufrWzPageFileOps_SelPics in 'ufrWzPageFileOps_SelPics.pas' {frWzPageFileOps_SelPics: TFrame},
  ufrWzPageFileOps_SelFolder in 'ufrWzPageFileOps_SelFolder.pas' {frWzPageFileOps_SelFolder: TFrame},
  ufrWzPageFileOps_MoveOptions in 'ufrWzPageFileOps_MoveOptions.pas' {frWzPageFileOps_MoveOptions: TFrame},
  ufrWzPageFileOps_DelOptions in 'ufrWzPageFileOps_DelOptions.pas' {frWzPageFileOps_DelOptions: TFrame},
  ufrWzPageFileOps_RepairOptions in 'ufrWzPageFileOps_RepairOptions.pas' {frWzPageFileOps_RepairOptions: TFrame},
  ufrWzPageFileOps_CDOptions in 'ufrWzPageFileOps_CDOptions.pas' {frWzPageFileOps_CDOptions: TFrame},
  ufrWzPageFileOps_RepairSelLinks in 'ufrWzPageFileOps_RepairSelLinks.pas' {frWzPageFileOps_RepairSelLinks: TFrame},
  ufrWzPageFileOps_MoveOptions2 in 'ufrWzPageFileOps_MoveOptions2.pas' {frWzPageFileOps_MoveOptions2: TFrame},
  phSettings in 'phSettings.pas',
  phValSetting in 'phValSetting.pas',
  phToolSetting in 'phToolSetting.pas',
  udToolProps in 'udToolProps.pas' {dToolProps},
  udMsgBox in 'udMsgBox.pas' {dMsgBox},
  phGraphics in 'phGraphics.pas',
  udGroupProps in 'udGroupProps.pas' {dGroupProps},
  phGUIObj in 'phGUIObj.pas',
  phIntf in 'phIntf.pas',
  phMutableIntf in 'phMutableIntf.pas',
  phOps in 'phOps.pas',
  phNativeIntf in 'phNativeIntf.pas',
  phObjConst in 'phObjConst.pas',
  phParsingPicFilter in 'phParsingPicFilter.pas',
  phPicFilterHighlighter in 'phPicFilterHighlighter.pas',
  phIJLIntf in 'phIJLIntf.pas',
  ufrExprPicFilter in 'ufrExprPicFilter.pas' {frExprPicFilter: TFrame},
  phPlugin in 'phPlugin.pas';

{$R *.res}

var
  hMtx: THandle;

begin
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
   // Выполняем приложение
  ShowProgressInfo('SMsg_Starting', []);
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
   // Сохраняем настройки
  SaveAllSettings; 
   // Уничтожаем мьютекс
  if hMtx<>0 then CloseHandle(hMtx);
end.
