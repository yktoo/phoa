//**********************************************************************************************************************
//  $Id: phWizForm.pas,v 1.4 2004-04-18 16:13:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phWizForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ConsVars, phObj, phWizard, Registry,
  DTLangTools, StdCtrls, ExtCtrls;

type
  TPhoaWizardForm = class(TForm, IWizardHostForm)
    pMain: TPanel;
    pButtons: TPanel;
    bCancel: TButton;
    bNext: TButton;
    bHelp: TButton;
    bBack: TButton;
    dtlsMain: TDTLanguageSwitcher;
    bvBottom: TBevel;
    pHeader: TPanel;
    lHeading: TLabel;
    bvTopPanel: TBevel;
    iIcon: TImage;
    procedure bHelpClick(Sender: TObject);
    procedure bBackClick(Sender: TObject);
    procedure bNextClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  private
     // Контроллер страниц
    FController: TWizardController;
     // True, если сделаны обновления в фотоальбоме
    FHasUpdates: Boolean;
     // Message handlers
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
     // IWizardHostForm
    function  WizHost_GetHostControl: TWinControl;
    function  WizHost_GetStorageForm: TForm;
    function  IWizardHostForm.PageChanging   = PageChanging;
    procedure IWizardHostForm.PageChanged    = PageChanged;
    procedure IWizardHostForm.StatusChanged  = UpdateButtons;
    function  IWizardHostForm.GetHostControl = WizHost_GetHostControl;
    function  IWizardHostForm.GetNextPageID  = GetNextPageID;
    function  IWizardHostForm.GetStorageForm = WizHost_GetStorageForm;
     // Prop handlers
    function  GetCurPage: TWizardPage;
    function  GetCurPageID: Integer;
    procedure SetHasUpdates(const Value: Boolean);
  protected
     // Инициализация/финализация Мастера
    procedure InitializeWizard; virtual;
    procedure FinalizeWizard; virtual;
     // Функции, возвращающие разрешённость соответствующих кнопок
    function  IsBtnBackEnabled:   Boolean; virtual;
    function  IsBtnNextEnabled:   Boolean; virtual;
    function  IsBtnCancelEnabled: Boolean; virtual;
     // Процедуры реакции на нажатия кнопок
    procedure ButtonClick_Back; virtual;
    procedure ButtonClick_Next; virtual;
    procedure ButtonClick_Cancel; virtual;
     // Должна возвращать ID следующей страницы, или 0, если нет такой
    function  GetNextPageID: Integer; virtual; abstract;
     // Должна возвращать имя раздела реестра для сохранения настроек
    function  GetFormRegistrySection: String; virtual; abstract;
     // Процедуры, вызываемые в процессе записи/чтения настроек из реестра. Могут использоваться потомками для
     //   сохранения/восстановления собственных настроек
    procedure SettingsStore(rif: TRegIniFile); virtual;
    procedure SettingsRestore(rif: TRegIniFile); virtual;
     // Вызывается перед сменой страницы. Должна вернуть True, чтобы позволить смену. В базовом классе всегда возвращает
     //   True
    function  PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean; virtual;
     // Вызывается после смены страницы. В базовом классе не делает ничего
    procedure PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer); virtual;
     // Обновляет кнопки
    procedure UpdateButtons;
  public
    constructor Create(AOwner: TComponent); override;
     // Выполняет Мастер. Возвращает True, если Мастер сделал изменения в фотоальбоме
    function Execute: Boolean;
     // Props
     // -- Контроллер страниц
    property Controller: TWizardController read FController;
     // -- Текущая отображаемая страница (nil, если нет)
    property CurPage: TWizardPage read GetCurPage;
     // -- ID текущей отображаемой страницы (0, если нет)
    property CurPageID: Integer read GetCurPageID;
     // -- True, если сделаны обновления в фотоальбоме
    property HasUpdates: Boolean read FHasUpdates write SetHasUpdates;
  end;

implementation
{$R *.dfm}
uses phUtils, ChmHlp, phSettings;

   //===================================================================================================================
   // TdFileOpsWizard
   //===================================================================================================================

  procedure TPhoaWizardForm.bBackClick(Sender: TObject);
  begin
    ButtonClick_Back;
  end;

  procedure TPhoaWizardForm.bCancelClick(Sender: TObject);
  begin
    ButtonClick_Cancel;
  end;

  procedure TPhoaWizardForm.bHelpClick(Sender: TObject);
  begin
    HtmlHelpContext(HelpContext);
  end;

  procedure TPhoaWizardForm.bNextClick(Sender: TObject);
  begin
    ButtonClick_Next;
  end;

  procedure TPhoaWizardForm.ButtonClick_Back;
  begin
    FController.SetPrevPageFromHistory;
  end;

  procedure TPhoaWizardForm.ButtonClick_Cancel;
  begin
    ModalResult := mrCancel;
  end;

  procedure TPhoaWizardForm.ButtonClick_Next;
  begin
    FController.SetNextPage;
  end;

  constructor TPhoaWizardForm.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
     // Создаём size gripper
    TSizeGripper.Create(Self).Parent := pButtons;
  end;

  function TPhoaWizardForm.Execute: Boolean;
  begin
    InitializeWizard;
    try
      ShowModal;
      Result := FHasUpdates;
    finally
      FinalizeWizard;
    end;
  end;

  procedure TPhoaWizardForm.FinalizeWizard;
  var rif: TRegIniFile;
  begin
     // Сохраняем настройки
    rif := TRegIniFile.Create(SRegRoot+'\'+GetFormRegistrySection);
    try
      SettingsStore(rif);
    finally
      rif.Free;
    end;
    FController.Free;
  end;

  function TPhoaWizardForm.GetCurPage: TWizardPage;
  begin
    Result := FController.VisiblePage;
  end;

  function TPhoaWizardForm.GetCurPageID: Integer;
  begin
    Result := FController.VisiblePageID;
  end;

  procedure TPhoaWizardForm.InitializeWizard;
  var rif: TRegIniFile;
  begin
    FontFromStr(Font, RootSetting.ValueStrByID[ISettingID_Gen_MainFont]);
     // Загружаем значок
    iIcon.Picture.Icon.Handle := LoadIcon(HInstance, 'MAINICON');
     // Создаём и настраиваем контроллер
    FController := TWizardController.Create(Self);
     // Загружаем настройки
    rif := TRegIniFile.Create(SRegRoot+'\'+GetFormRegistrySection);
    try
      SettingsRestore(rif);
    finally
      rif.Free;
    end;
  end;

  function TPhoaWizardForm.IsBtnBackEnabled: Boolean;
  begin
    Result := not FController.HistoryEmpty;
  end;

  function TPhoaWizardForm.IsBtnCancelEnabled: Boolean;
  begin
    Result := True;
  end;

  function TPhoaWizardForm.IsBtnNextEnabled: Boolean;
  begin
    Result := (CurPage<>nil) and CurPage.DataValid and (GetNextPageID>0);
  end;

  procedure TPhoaWizardForm.PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer);
  begin
    { does nothing }
  end;

  function TPhoaWizardForm.PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean;
  begin
    Result := True;
  end;

  procedure TPhoaWizardForm.SetHasUpdates(const Value: Boolean);
  begin
    if FHasUpdates<>Value then begin
      FHasUpdates := Value;
      UpdateButtons;
    end;
  end;

  procedure TPhoaWizardForm.SettingsRestore(rif: TRegIniFile);
  var
    s: String;
    r: TRect;
  begin
     // Восстанавливаем размеры формы
    s := rif.ReadString('', 'Position', '');
    r.Left   := StrToIntDef(ExtractFirstWord(s, ','), Left);
    r.Top    := StrToIntDef(ExtractFirstWord(s, ','), Top);
    r.Right  := StrToIntDef(ExtractFirstWord(s, ','), Left+Width);
    r.Bottom := StrToIntDef(ExtractFirstWord(s, ','), Top+Height);
    BoundsRect := r;
  end;

  procedure TPhoaWizardForm.SettingsStore(rif: TRegIniFile);
  begin
     // Сохраняем размеры формы
    rif.WriteString('', 'Position', Format('%d,%d,%d,%d', [Left, Top, Left+Width, Top+Height]));
  end;

  procedure TPhoaWizardForm.UpdateButtons;
  begin
     // Настраиваем кнопки
    bBack.Enabled   := IsBtnBackEnabled;
    bNext.Enabled   := IsBtnNextEnabled;
    bNext.Default   := bNext.Enabled;
    bCancel.Enabled := IsBtnCancelEnabled;
    bCancel.Caption := ConstVal(iif(FHasUpdates, 'SBtn_Close', 'SBtn_Cancel'));
    bCancel.Default := not bNext.Default and bCancel.Enabled;
     // Устанавливаем heading
    if CurPage<>nil then lHeading.Caption := CurPage.PageTitle;
  end;

  function TPhoaWizardForm.WizHost_GetHostControl: TWinControl;
  begin
    Result := pMain;
  end;

  function TPhoaWizardForm.WizHost_GetStorageForm: TForm;
  begin
    Result := Self;
  end;

  procedure TPhoaWizardForm.WMHelp(var Msg: TWMHelp);
  begin
    HtmlHelpContext(HelpContext);
  end;

end.
