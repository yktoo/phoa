//**********************************************************************************************************************
//  $Id: udSettings.pas,v 1.18 2004-12-31 13:38:58 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, GR32, Controls, Forms, Dialogs, ConsVars, phSettings, Registry,
  phDlg, DKLang, TB2Dock, TB2Toolbar, TBX, StdCtrls, ExtCtrls;

type
  TdSettings = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    pMain: TPanel;
    dkNav: TTBXDock;
    tbNav: TTBXToolbar;
    pEditor: TPanel;
    procedure FormShow(Sender: TObject);
  private
     // Локальная копия настроек
    FLocalRootSetting: TPhoaSetting;
     // ID страницы-настройки, которую следует выбрать по умочанию
    FDefPageSettingID: Integer;
     // Prop storage
    FCurPageSetting: TPhoaPageSetting;
     // Текущий редактор настроек
    FEditor: TWinControl;
     // Создаёт кнопкtи навигации (уровень 0 из ConsVars.aPhoaSettings[])
    procedure CreateNavBar;
     // Событие нажатия NavBar-кнопки
    procedure NavBarButtonClick(Sender: TObject);
     // Событие изменения данных настроек
    procedure SettingChange(Sender: TObject);
     // Prop handlers
    procedure SetCurPageSetting(Value: TPhoaPageSetting);
  protected
    procedure InitializeDialog; override;
    procedure FinalizeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetFormRegistrySection: String; override;
    function  GetSizeable: Boolean; override;
  public
     // Props
     // -- Текущая выбранная страница-настройка (кнопка на NavBar)
    property CurPageSetting: TPhoaPageSetting read FCurPageSetting write SetCurPageSetting;
  end;

   // Отображает диалог настроек. DefPageSetting - страница-настройка навигатора, которую следует выбрать по умочанию
  function EditSettings(DefPageSettingID: Integer): Boolean;

implementation
{$R *.dfm}
uses phUtils, Main, TypInfo;

  function EditSettings(DefPageSettingID: Integer): Boolean;
  begin
    with TdSettings.Create(Application) do
      try
        FDefPageSettingID := DefPageSettingID;
        Result := Execute;
      finally
        Free;
      end;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // TdSettings
   //-------------------------------------------------------------------------------------------------------------------

  procedure TdSettings.ButtonClick_OK;
  begin
     // Копируем локальные настройки в глобальные
    RootSetting.Assign(FLocalRootSetting);
    inherited ButtonClick_OK;
  end;

  procedure TdSettings.CreateNavBar;
  var
    i: Integer;
    tbi: TTBXCustomItem;
    PPS: TPhoaPageSetting;
  begin
    for i := 0 to FLocalRootSetting.ChildCount-1 do begin
      PPS := FLocalRootSetting.Children[i] as TPhoaPageSetting;
      if PPS.Visible then begin
        tbi := TTBXItem.Create(Self);
        tbi.Caption     := ConstValEx(PPS.Name);
        tbi.HelpContext := PPS.HelpContext;
        tbi.ImageIndex  := PPS.ImageIndex;
        tbi.Tag         := Integer(PPS);
        tbi.OnClick     := NavBarButtonClick;
        if i<9 then tbi.ShortCut := 16433+i; // Ctrl+1..9 keys
        tbNav.Items.Add(tbi);
      end;
    end;
  end;

  procedure TdSettings.FinalizeDialog;
  begin
    FLocalRootSetting.Free;
    inherited FinalizeDialog;
  end;

  procedure TdSettings.FormShow(Sender: TObject);
  begin
    ActiveControl := FEditor;
  end;

  function TdSettings.GetFormRegistrySection: String;
  begin
    Result := SRegSettings_Root;
  end;

  function TdSettings.GetSizeable: Boolean;
  begin
    Result := True;
  end;

  procedure TdSettings.InitializeDialog;
  begin
    inherited InitializeDialog;
     // Копируем настройки
    FLocalRootSetting := TPhoaSetting.Create(nil, 0, '');
    FLocalRootSetting.Assign(RootSetting);
     // Создаём кнопки навигации
    CreateNavBar;
     // Выбираем стартовую кнопку
    CurPageSetting := FLocalRootSetting.Settings[FDefPageSettingID] as TPhoaPageSetting;
  end;

  procedure TdSettings.NavBarButtonClick(Sender: TObject);
  begin
    CurPageSetting := TPhoaPageSetting(TComponent(Sender).Tag);
  end;

  procedure TdSettings.SetCurPageSetting(Value: TPhoaPageSetting);
  var
    i: Integer;
    PrevEditor: TWinControl;
  begin
    FCurPageSetting := Value;
     // Нажимаем соотв. кнопку
    for i := 0 to tbNav.Items.Count-1 do
      with tbNav.Items[i] do Checked := Tag=Integer(FCurPageSetting);
     // Настраиваем HelpContext
    HelpContext := FCurPageSetting.HelpContext;
     // Загружаем редактор, если класс сменился
    if (FEditor=nil) or (FEditor.ClassType<>FCurPageSetting.EditorClass) then begin
       // Запоминаем прежний редактор
      PrevEditor := FEditor;
      try
         // Создаём новый
        FEditor := FCurPageSetting.EditorClass.Create(Self);
        (FEditor as IPhoaSettingEditor).InitAndEmbed(pEditor, SettingChange);
      finally
         // Убиваем старый редактор после создания нового, чтобы не мельтешило
        FreeAndNil(PrevEditor);
      end;
    end;
     // Инициализируем редактор
    (FEditor as IPhoaSettingEditor).RootSetting := FCurPageSetting;
     // Устанавливаем фокус
    if Visible then ActiveControl := FEditor;
  end;

  procedure TdSettings.SettingChange(Sender: TObject);
  begin
    Modified := FLocalRootSetting.Modified;
  end;

end.

