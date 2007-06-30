//**********************************************************************************************************************
//  $Id: phFrm.pas,v 1.7 2007-06-30 10:36:20 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, TntForms, Dialogs, phObj;

type
  TPhoaForm = class(TTntForm)
  private
     // Счётчик блокировки обновления
    FLockCounter: Integer;
     // Prop storage
    FHasUpdates: Boolean;
    FModified: Boolean;
     // Если требуется, создаёт и возвращает TPhoaRegIniFile для сохранения/загрузки настроек; иначе возвращает nil
    function  CreateRegIni: TPhoaRegIniFile;
     // Message handlers
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
     // Prop handlers
    function  GetRegistryKey: AnsiString;
    procedure SetHasUpdates(Value: Boolean);
    procedure SetModified(Value: Boolean);
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure DoHide; override;
    procedure DoShow; override;
    procedure Loaded; override;
     // Инициализация/финализация формы до/после модального показа
    procedure ExecuteInitialize; virtual;
    procedure ExecuteFinalize; virtual;
     // Вызывается при изменении статуса формы
    procedure UpdateState; virtual;
     // Процедуры записи/чтения начальных настроек из реестра. Могут использоваться потомками для сохранения и
     //   восстановления собственных настроек
    procedure SettingsInitialSave(rif: TPhoaRegIniFile); virtual;
    procedure SettingsInitialLoad(rif: TPhoaRegIniFile); virtual;
     // Процедуры записи/чтения настроек из реестра, вызываемые непосредственно перед показом и после скрытия
    procedure SettingsSave(rif: TPhoaRegIniFile); virtual;
    procedure SettingsLoad(rif: TPhoaRegIniFile); virtual;
     // Prop handlers
    function  GetDataValid: Boolean; virtual;
    function  GetRelativeRegistryKey: AnsiString; virtual;
    function  GetSizeable: Boolean; virtual;
  public
     // Модально отображает форму. Возвращает HasUpdates
    function  ExecuteModal: Boolean;
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
     // -- True, если форма произвела какие-то изменения
    property HasUpdates: Boolean read FHasUpdates write SetHasUpdates;
     // -- True, если в форме пользователь изменил какие-то данные
    property Modified: Boolean read FModified write SetModified;
     // -- Ключ реестра для сохранения настроек
    property RegistryKey: AnsiString read GetRegistryKey;
     // -- Ключ реестра для сохранения настроек относительно корневого ключа приложения. Если пустая строка, сохранения
     //    настроек не требуется
    property RelativeRegistryKey: AnsiString read GetRelativeRegistryKey;
     // -- True, если размер формы позволяется менять. В базовом классе всегда возвращает False
    property Sizeable: Boolean read GetSizeable;
  end;

implementation
{$R *.dfm}
uses phChmHlp, phUtils, phSettings, ConsVars;

  procedure TPhoaForm.BeginUpdate;
  begin
    Inc(FLockCounter);
  end;

  function TPhoaForm.CreateRegIni: TPhoaRegIniFile;
  var sKey: AnsiString;
  begin
    sKey := RegistryKey;
    if sKey='' then Result := nil else Result := TPhoaRegIniFile.Create(sKey);
  end;

  procedure TPhoaForm.DoCreate;
  var rif: TPhoaRegIniFile;
  begin
    inherited DoCreate;
     // Если нужно, делаем форму Sizeable
    if Sizeable then begin
       // Переписываем текущие размеры в качестве минимальных
      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;
    end else begin
      BorderStyle := bsDialog;
      BorderIcons := [biSystemMenu];
      Position    := poScreenCenter;
    end;
     // Настраиваем шрифт
    FontFromStr(Font, SettingValueWStr(ISettingID_Gen_MainFont));
     // Загружаем начальные настройки, если требуется
    rif := CreateRegIni;
    if rif<>nil then
      try
        SettingsInitialLoad(rif);
      finally
        rif.Free;
      end;
  end;

  procedure TPhoaForm.DoDestroy;
  var rif: TPhoaRegIniFile;
  begin
     // Сохраняем начальные настройки, если требуется
    rif := CreateRegIni;
    if rif<>nil then
      try
        SettingsInitialSave(rif);
      finally
        rif.Free;
      end;
    inherited DoDestroy;
  end;

  procedure TPhoaForm.DoHide;
  var rif: TPhoaRegIniFile;
  begin
     // Сохраняем настройки, если требуется
    rif := CreateRegIni;
    if rif<>nil then
      try
        SettingsSave(rif);
      finally
        rif.Free;
      end;
    inherited DoHide;
  end;

  procedure TPhoaForm.DoShow;
  var rif: TPhoaRegIniFile;
  begin
    inherited DoShow;
     // Загружаем настройки, если требуется
    rif := CreateRegIni;
    if rif<>nil then
      try
        SettingsLoad(rif);
      finally
        rif.Free;
      end;
     // Сбрасываем флаг изменённости 
    Modified := False;
  end;

  procedure TPhoaForm.EndUpdate;
  begin
    if FLockCounter>0 then Dec(FLockCounter);
    if FLockCounter=0 then UpdateState;
  end;

  procedure TPhoaForm.ExecuteFinalize;
  begin
    { does nothing }
  end;

  procedure TPhoaForm.ExecuteInitialize;
  begin
    { does nothing }
  end;

  function TPhoaForm.ExecuteModal: Boolean;
  begin
     // Инициализируем форму
    BeginUpdate;
    try
      ExecuteInitialize;
    finally
      EndUpdate;
    end;
     // Кажем окошко
    try
      ShowModal;
    finally
       // Финализируем форму
      ExecuteFinalize;
    end;
    Result := HasUpdates;
  end;

  function TPhoaForm.GetDataValid: Boolean;
  begin
    Result := True;
  end;

  function TPhoaForm.GetRegistryKey: AnsiString;
  var sRelativeKey: AnsiString;
  begin
    sRelativeKey := RelativeRegistryKey;
    if sRelativeKey='' then Result := '' else Result := SRegRoot+'\'+sRelativeKey;
  end;

  function TPhoaForm.GetRelativeRegistryKey: AnsiString;
  begin
    Result := '';
  end;

  function TPhoaForm.GetSizeable: Boolean;
  begin
    Result := False;
  end;

  procedure TPhoaForm.Loaded;
  begin
    inherited Loaded;
    AutoScroll := False;
  end;

  procedure TPhoaForm.SetHasUpdates(Value: Boolean);
  begin
    FHasUpdates := Value;
    StateChanged;
  end;

  procedure TPhoaForm.SetModified(Value: Boolean);
  begin
    FModified := Value;
    StateChanged;
  end;

  procedure TPhoaForm.SettingsInitialLoad(rif: TPhoaRegIniFile);
  begin
    { does nothing }
  end;

  procedure TPhoaForm.SettingsInitialSave(rif: TPhoaRegIniFile);
  begin
    { does nothing }
  end;

  procedure TPhoaForm.SettingsLoad(rif: TPhoaRegIniFile);
  begin
     // Если размер формы можно менять, восстанавливаем размеры формы
    if Sizeable then FormPositionFromStr(Self, rif.ReadString('', 'Position', ''));
  end;

  procedure TPhoaForm.SettingsSave(rif: TPhoaRegIniFile);
  begin
     // Если размер формы можно менять, сохраняем её позицию
    if Sizeable then rif.WriteString('', 'Position', FormPositionToStr(Self));
  end;

  procedure TPhoaForm.StateChanged;
  begin
    if FLockCounter=0 then UpdateState;
  end;

  function TPhoaForm.UpdateLocked: Boolean;
  begin
    Result := FLockCounter>0;
  end;

  procedure TPhoaForm.UpdateState;
  begin
    { does nothing }
  end;

  procedure TPhoaForm.WMHelp(var Msg: TWMHelp);
  begin
    HtmlHelpContext(HelpContext);
  end;

end.
