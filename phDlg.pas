//**********************************************************************************************************************
//  $Id: phDlg.pas,v 1.5 2004-04-23 19:26:29 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DTLangTools, ExtCtrls;

type
  TPhoaDialog = class(TForm)
    dtlsMain: TDTLanguageSwitcher;
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
  private
    FLockCounter: Integer;
     // Prop storage
    FHasUpdates: Boolean;
    FModified: Boolean;
    FOKIgnoresOKIgnoresDataValidity: Boolean;
    FOKIgnoresModified: Boolean;
     // Message handlers
    procedure WMHelp(var Msg: TWMHelp); message WM_HELP;
     // Prop handlers
    procedure SetHasUpdates(Value: Boolean);
    procedure SetModified(Value: Boolean);
    procedure SetOKIgnoresModified(Value: Boolean);
    procedure SetOKIgnoresOKIgnoresDataValidity(Value: Boolean);
  protected
    procedure Loaded; override;
     // Инициализация/финализация диалога
    procedure InitializeDialog; virtual;
    procedure FinalizeDialog; virtual;
     // События нажатия кнопок диалога
    procedure ButtonClick_OK; virtual;
    procedure ButtonClick_Cancel; virtual;
    procedure ButtonClick_Help; virtual;
     // Prop handler. В базовом классе всегда возвращает True
    function  GetDataValid: Boolean; virtual;
  public
     // Отображает диалог. Возвращает HasUpdates
    function  Execute: Boolean;
     // Установка/снятие блокировки изменения Modified
    procedure BeginUpdate;
    procedure EndUpdate;
     // Настраивает разрешённость и вид кнопок
    procedure UpdateButtons; virtual;
     // Props
     // -- Если True, данные в диалоге допустимы
    property DataValid: Boolean read GetDataValid;
     // -- True, если диалог произвёл какие-то изменения
    property HasUpdates: Boolean read FHasUpdates write SetHasUpdates;
     // -- True, если в диалоге пользователь изменил какие-то данные
    property Modified: Boolean read FModified write SetModified;
     // -- Если True, разрешённость кнопки ОК не зависит от DataValid
    property OKIgnoresDataValidity: Boolean read FOKIgnoresOKIgnoresDataValidity write SetOKIgnoresOKIgnoresDataValidity;
     // -- Если True, разрешённость кнопки ОК не зависит от Modified
    property OKIgnoresModified: Boolean read FOKIgnoresModified write SetOKIgnoresModified;
  end;

implementation
{$R *.dfm}
uses phUtils, ChmHlp, ConsVars, phSettings;

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

  procedure TPhoaDialog.DlgDataChange(Sender: TObject);
  begin
    Modified := True;
  end;

  procedure TPhoaDialog.EndUpdate;
  begin
    if FLockCounter>0 then Dec(FLockCounter);
    if FLockCounter=0 then UpdateButtons;
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

  function TPhoaDialog.GetDataValid: Boolean;
  begin
    Result := True;
  end;

  procedure TPhoaDialog.InitializeDialog;
  begin
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
    UpdateButtons;
  end;

  procedure TPhoaDialog.SetModified(Value: Boolean);
  begin
    if FLockCounter>0 then Exit;
    FModified := Value;
    UpdateButtons;
  end;

  procedure TPhoaDialog.SetOKIgnoresModified(Value: Boolean);
  begin
    if FOKIgnoresModified<>Value then begin
      FOKIgnoresModified := Value;
      UpdateButtons;
    end;
  end;

  procedure TPhoaDialog.SetOKIgnoresOKIgnoresDataValidity(Value: Boolean);
  begin
    if FOKIgnoresOKIgnoresDataValidity<>Value then begin
      FOKIgnoresOKIgnoresDataValidity := Value;
      UpdateButtons;
    end;
  end;

  procedure TPhoaDialog.UpdateButtons;
  begin
    if FLockCounter>0 then Exit;
    bOK.Enabled := (OKIgnoresModified or Modified) and (OKIgnoresDataValidity or DataValid);
    bCancel.Caption := ConstVal(iif(HasUpdates, 'SBtn_Close', 'SBtn_Cancel'));
  end;

  procedure TPhoaDialog.WMHelp(var Msg: TWMHelp);
  begin
    HtmlHelpContext(HelpContext);
  end;

end.
