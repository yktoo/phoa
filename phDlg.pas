//**********************************************************************************************************************
//  $Id: phDlg.pas,v 1.19 2005-04-17 08:51:19 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Registry,
  StdCtrls, ExtCtrls;

type
  TPhoaDialog = class(TForm)
    pButtonsBottom: TPanel;
    bCancel: TButton;
    bOK: TButton;
    bHelp: TButton;
    bvBottom: TBevel;
     // Обработчик события для привязывания событий TNotifyEvent, изменяющих данные диалога
    procedure DlgDataChange(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FLockCounter: Integer;
     // Prop storage
    FHasUpdates: Boolean;
    FModified: Boolean;
    FOKIgnoresDataValidity: Boolean;
    FOKIgnoresModified: Boolean;
     // Если требуется, создаёт и возвращает TRegIniFile для сохранения/загрузки настроек; иначе возвращает nil
    function  CreateRegIni: TRegIniFile;
     // Message handlers
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
     // Prop handlers
    procedure SetHasUpdates(Value: Boolean);
    procedure SetModified(Value: Boolean);
    procedure SetOKIgnoresModified(Value: Boolean);
    procedure SetOKIgnoresOKIgnoresDataValidity(Value: Boolean);
  protected
    procedure Loaded; override;
    procedure DoShow; override;
    procedure DoHide; override;
     // Инициализация/финализация диалога
    procedure InitializeDialog; virtual;
    procedure FinalizeDialog; virtual;
     // События нажатия кнопок диалога
    procedure ButtonClick_OK; virtual;
    procedure ButtonClick_Cancel; virtual;
    procedure ButtonClick_Help; virtual;
     // Вызывается при изменении статуса диалога. В базовом классе настраивает разрешённость и вид кнопок
    procedure UpdateState; virtual;
     // Должна возвращать имя раздела реестра для сохранения настроек. В базовом классе возвращает пустую строку, что
     //   означает, что сохранения настроек не требуется
    function  GetFormRegistrySection: String; virtual;
     // Процедуры, вызываемые в процессе записи/чтения настроек из реестра. Могут использоваться потомками для
     //   сохранения/восстановления собственных настроек
    procedure SettingsStore(rif: TRegIniFile); virtual;
    procedure SettingsRestore(rif: TRegIniFile); virtual;
     // Prop handlers
    function  GetDataValid: Boolean; virtual;
    function  GetSizeable: Boolean; virtual;
  public
     // Отображает диалог. Возвращает HasUpdates
    function  Execute: Boolean;
     // Установка/снятие блокировки изменения Modified
    procedure BeginUpdate;
    procedure EndUpdate;
     // True, если FLockCounter>0
    function  UpdateLocked: Boolean;
     // Вызывает обновление статуса диалога (отложенное при FLockCounter>0)
    procedure StateChanged;
     // Props
     // -- Если True, данные в диалоге допустимы. В базовом классе всегда возвращает True
    property DataValid: Boolean read GetDataValid;
     // -- True, если диалог произвёл какие-то изменения
    property HasUpdates: Boolean read FHasUpdates write SetHasUpdates;
     // -- True, если в диалоге пользователь изменил какие-то данные
    property Modified: Boolean read FModified write SetModified;
     // -- Если True, разрешённость кнопки ОК не зависит от DataValid
    property OKIgnoresDataValidity: Boolean read FOKIgnoresDataValidity write SetOKIgnoresOKIgnoresDataValidity;
     // -- Если True, разрешённость кнопки ОК не зависит от Modified
    property OKIgnoresModified: Boolean read FOKIgnoresModified write SetOKIgnoresModified;
     // -- True, если размер формы позволяется менять. В базовом классе всегда возвращает False
    property Sizeable: Boolean read GetSizeable;
  end;

implementation
{$R *.dfm}
uses phUtils, ChmHlp, ConsVars, phSettings, phObj, phGUIObj;

   //===================================================================================================================
   // TPhoaDialog
   //===================================================================================================================

  procedure TPhoaDialog.bCancelClick(Sender: TObject);
  begin
    ButtonClick_Cancel;
  end;

  procedure TPhoaDialog.BeginUpdate;
  begin
    Inc(FLockCounter);
  end;

  procedure TPhoaDialog.bHelpClick(Sender: TObject);
  begin
    ButtonClick_Help;
  end;

  procedure TPhoaDialog.bOKClick(Sender: TObject);
  begin
    ButtonClick_OK;
  end;

  procedure TPhoaDialog.ButtonClick_Cancel;
  begin
    ModalResult := mrCancel;
  end;

  procedure TPhoaDialog.ButtonClick_Help;
  begin
    HtmlHelpContext(HelpContext);
  end;

  procedure TPhoaDialog.ButtonClick_OK;
  begin
    HasUpdates := True;
    ModalResult := mrOK;
  end;

  function TPhoaDialog.CreateRegIni: TRegIniFile;
  var sRegSection: String;
  begin
    sRegSection := GetFormRegistrySection;
    if sRegSection='' then Result := nil else Result := TRegIniFile.Create(SRegRoot+'\'+sRegSection);
  end;

  procedure TPhoaDialog.DlgDataChange(Sender: TObject);
  begin
    Modified := True;
  end;

  procedure TPhoaDialog.DoHide;
  var rif: TRegIniFile;
  begin
     // Сохраняем настройки, если требуется
    rif := CreateRegIni;
    if rif<>nil then
      try
        SettingsStore(rif);
      finally
        rif.Free;
      end;
    inherited DoHide;
  end;

  procedure TPhoaDialog.DoShow;
  var rif: TRegIniFile;
  begin
    inherited DoShow;
     // Восстанавливаем настройки, если требуется
    rif := CreateRegIni;
    if rif<>nil then
      try
        SettingsRestore(rif);
      finally
        rif.Free;
      end;
  end;

  procedure TPhoaDialog.EndUpdate;
  begin
    if FLockCounter>0 then Dec(FLockCounter);
    if FLockCounter=0 then UpdateState;
  end;

  function TPhoaDialog.Execute: Boolean;
  begin
     // Инициализируем диалог
    BeginUpdate;
    try
      InitializeDialog;
    finally
      EndUpdate;
    end;
     // Кажем окошко
    try
      ShowModal;
    finally
       // Финализируем диалог
      FinalizeDialog;
    end;
    Result := HasUpdates;
  end;

  procedure TPhoaDialog.FinalizeDialog;
  begin
    { does nothing }
  end;

  procedure TPhoaDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
     // Перехватываем нажатие Ctrl+Enter и отрабатываем его как нажатие на ОК
    if (Key=VK_RETURN) and (Shift=[ssCtrl]) and DataValid then begin
      Key := 0;
      ButtonClick_OK;
    end;
  end;

  function TPhoaDialog.GetDataValid: Boolean;
  begin
    Result := True;
  end;

  function TPhoaDialog.GetFormRegistrySection: String;
  begin
    Result := '';
  end;

  function TPhoaDialog.GetSizeable: Boolean;
  begin
    Result := False;
  end;

  procedure TPhoaDialog.InitializeDialog;
  begin
     // Если нужно, делаем диалог Sizeable
    if Sizeable then begin
       // Делаем sizeable
      BorderStyle := bsSizeable;
      BorderIcons := [biSystemMenu, biMaximize];
      Position    := poDesigned;
      AutoScroll  := False;
       // Переписываем текущие размеры в качестве минимальных
      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;
       // Создаём size gripper
      TSizeGripper.Create(Self).Parent := pButtonsBottom;
    end;
     // Настраиваем шрифт
    FontFromStr(Font, SettingValueStr(ISettingID_Gen_MainFont));
  end;

  procedure TPhoaDialog.Loaded;
  begin
    inherited Loaded;
    AutoScroll := False;
  end;

  procedure TPhoaDialog.SetHasUpdates(Value: Boolean);
  begin
    FHasUpdates := Value;
    StateChanged;
  end;

  procedure TPhoaDialog.SetModified(Value: Boolean);
  begin
    if FLockCounter>0 then Exit;
    FModified := Value;
    StateChanged;
  end;

  procedure TPhoaDialog.SetOKIgnoresModified(Value: Boolean);
  begin
    if FOKIgnoresModified<>Value then begin
      FOKIgnoresModified := Value;
      StateChanged;
    end;
  end;

  procedure TPhoaDialog.SetOKIgnoresOKIgnoresDataValidity(Value: Boolean);
  begin
    if FOKIgnoresDataValidity<>Value then begin
      FOKIgnoresDataValidity := Value;
      StateChanged;
    end;
  end;

  procedure TPhoaDialog.SettingsRestore(rif: TRegIniFile);
  begin
     // Если размер формы можно менять, восстанавливаем размеры формы
    if Sizeable then FormPositionFromStr(Self, rif.ReadString('', 'Position', ''));
  end;

  procedure TPhoaDialog.SettingsStore(rif: TRegIniFile);
  begin
     // Если размер формы можно менять, сохраняем её позицию
    if Sizeable then rif.WriteString('', 'Position', FormPositionToStr(Self));
  end;

  procedure TPhoaDialog.StateChanged;
  begin
    if FLockCounter=0 then UpdateState;
  end;

  function TPhoaDialog.UpdateLocked: Boolean;
  begin
    Result := FLockCounter>0;
  end;

  procedure TPhoaDialog.UpdateState;
  begin
    bOK.Enabled := (OKIgnoresModified or Modified) and (OKIgnoresDataValidity or DataValid);
    bCancel.Caption := ConstVal(iif(HasUpdates, 'SBtn_Close', 'SBtn_Cancel'));
  end;

  procedure TPhoaDialog.WMHelp(var Msg: TWMHelp);
  begin
    HtmlHelpContext(HelpContext);
  end;

end.

