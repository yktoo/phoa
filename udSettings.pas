//**********************************************************************************************************************
//  $Id: udSettings.pas,v 1.9 2004-05-01 04:03:24 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit udSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, GR32, Controls, Forms, Dialogs, ConsVars, phSettings,
  phDlg, TB2Dock, TB2Toolbar, TBX, DTLangTools, StdCtrls, ExtCtrls,
  Placemnt;

type
  TdSettings = class(TPhoaDialog)
    pMain: TPanel;
    dkNav: TTBXDock;
    tbNav: TTBXToolbar;
    fpMain: TFormPlacement;
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
     // Декодирует строку текста сообразно правилам (если требуется, то с использованием констант)
    procedure DecodeSettingText(const sText: String; out sDecoded: String);
     // Prop handlers
    procedure SetCurPageSetting(Value: TPhoaPageSetting);
  protected
    procedure InitializeDialog; override;
    procedure FinalizeDialog; override;
    procedure ButtonClick_OK; override;
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
    s: String;
  begin
    for i := 0 to FLocalRootSetting.ChildCount-1 do begin
      PPS := FLocalRootSetting.Children[i] as TPhoaPageSetting;
      tbi := TTBXItem.Create(Self);
      DecodeSettingText(PPS.Name, s);
      tbi.Caption     := s;
      tbi.HelpContext := PPS.HelpContext;
      tbi.ImageIndex  := PPS.ImageIndex;
      tbi.Tag         := Integer(PPS);
      tbi.OnClick     := NavBarButtonClick;
      if i<9 then tbi.ShortCut := 16433+i; // Ctrl+1..9 keys
      tbNav.Items.Add(tbi);
    end;
  end;

  procedure TdSettings.DecodeSettingText(const sText: String; out sDecoded: String);
  begin
    sDecoded := sText;
    if sDecoded<>'' then
      case sDecoded[1] of
         // Если наименование начинается на '@' - это константа из TdSettings.dtlsMain
        '@': sDecoded := dtlsMain.Consts[Copy(sDecoded, 2, MaxInt)];
         // Если наименование начинается на '#' - это константа из fMain.dtlsMain
        '#': sDecoded := ConstVal(Copy(sDecoded, 2, MaxInt));
      end;
  end;

  procedure TdSettings.FinalizeDialog;
  begin
    FLocalRootSetting.Free;
    inherited FinalizeDialog;
  end;

  procedure TdSettings.InitializeDialog;
  begin
    inherited InitializeDialog;
    MakeSizeable;
     // Настраиваем fpMain
    fpMain.IniFileName := SRegRoot;
    fpMain.IniSection  := SRegSettings_Root;
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
  var i: Integer;
  begin
    FCurPageSetting := Value;
     // Нажимаем соотв. кнопку
    for i := 0 to tbNav.Items.Count-1 do
      with tbNav.Items[i] do Checked := Tag=Integer(FCurPageSetting);
     // Настраиваем HelpContext
    HelpContext := FCurPageSetting.HelpContext;
     // Загружаем редактор, если класс сменился
    if (FEditor=nil) or (FEditor.ClassType<>FCurPageSetting.EditorClass) then begin
      FreeAndNil(FEditor);
      FEditor := FCurPageSetting.EditorClass.Create(Self);
      (FEditor as IPhoaSettingEditor).InitAndEmbed(pMain, DlgDataChange, DecodeSettingText);
    end;
     // Инициализируем редактор
    (FEditor as IPhoaSettingEditor).RootSetting := FCurPageSetting;
     // Устанавливаем фокус на редактор
//    ActiveControl := FEditor;
  end;

end.
