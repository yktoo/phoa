unit udMsgBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars,
  phDlg, StdCtrls, ExtCtrls, DTLangTools;

type
  TdMsgBox = class(TForm)
    dtlsMain: TDTLanguageSwitcher;
    iIcon: TImage;
    lMessage: TLabel;
    cbDontShowAgain: TCheckBox;
  private
     // Вид сообщения
    FKind: TMessageBoxKind;
     // Текст сообщения
    FMessage: String;
     // Кнопки, которые должны отображаться в диалоге
    FButtons: TMessageBoxButtons;
     // Если True, то дополнительно отображает checkbox 'Больше не показывать данное сообщение'
    FDiscardable: Boolean;
     // Ширина кнопок с учётом всех границ и зазоров, инициализируется в AdjustButtons()
    FButtonWidths: Integer;
     // Набор флагов - результат выполнения
    FResults: TMessageBoxResults;
     // Инициализация диалога
    procedure InitializeDlg;
     // Настраивает заголовок окна
    procedure AdjustCaption;
     // Настраивает значок
    procedure AdjustIcon;
     // Настраивает текст сообщения
    procedure AdjustMessage;
     // Настраивает кнопки и переключатель "Больше не показывать..."
    procedure AdjustButtons;
     // Настраивает размер/положение окна
    procedure AdjustBounds;
     // Выдаёт звук, соответствующий виду диалога
    procedure AdjustSound;
     // Выполняет диалог и возвращает результат
    function Execute: TMessageBoxResults;
     // Событие клика на кнопке
    procedure BtnClick(Sender: TObject); 
     // Событие клика на кнопке Help
    procedure BtnHelpClick(Sender: TObject);
  end;

   // Отображает [почти] стандартный диалог-сообщение.
   //   AKind           - вид сообщения. Влияет на заголовок, значок и звуковой сигнал
   //   sMessage        - текст сообщения
   //   AButtons        - кнопки, которые должны отображаться в диалоге
   //   bMsgIsConstName - если True, то sMessage трактуется как имя константы, а её значение подставляется вместо текста
   //   bDiscardable    - если True, то дополнительно отображает checkbox 'Больше не показывать данное сообщение'
   //   Возвращает набор, соотоящий из одной из кнопок и, опционально, mbrDontShow (если включен переключатель "Больше
   //     не показывать..." и bDiscardable=True)
  function PhoaMsgBox(AKind: TMessageBoxKind; const sMessage: String; bMsgIsConstName, bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults; overload;
   // То же самое, но дополнительно принимает массив параметров для форматирования
  function PhoaMsgBox(AKind: TMessageBoxKind; const sMessage: String; const aParams: Array of const; bMsgIsConstName, bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults; overload;
   // Отображает диалог информации, если iSettingID<=0 или значение настройки iSettingID=True, иначе диалог не
   //   отображается. При iSettingID>0 в диалоге отображается также переключатель "Больше не показывать...". Если
   //   пользователь включит его и нажмёт ОК, то в значение настройки заносится False
   //   Если bWarning=True, то отображается в режиме mbkWarning, иначе - mbkInfo
  procedure PhoaInfo(bWarning: Boolean; const sConstName: String; iSettingID: Integer = 0); overload;
  procedure PhoaInfo(bWarning: Boolean; const sConstName: String; const aParams: Array of const; iSettingID: Integer = 0); overload;
   // Отображает диалог подтверждения, если iSettingID<=0 или значение настройки iSettingID=True, иначе диалог не
   //   отображается, а просто возвращает True. При iSettingID>0 в диалоге отображается также переключатель "Больше не
   //   показывать...". Если пользователь включит его и нажмёт ОК, то в значение настройки заносится False
   //   Если bWarning=True, то отображается в режиме mbkConfirmWarning, иначе - mbkConfirm
  function  PhoaConfirm(bWarning: Boolean; const sConstName: String; iSettingID: Integer = 0): Boolean; overload;
  function  PhoaConfirm(bWarning: Boolean; const sConstName: String; const aParams: Array of const; iSettingID: Integer = 0): Boolean; overload;
   // Отображает диалог ошибки
  procedure PhoaError(const sConstName: String); overload;
  procedure PhoaError(const sConstName: String; const aParams: Array of const); overload;

const
   // Минимальная разница между шириной экрана и шириной окна
  IMsgBox_ScreenWidthGap       = 100;
   // Минимальная разница между высотой экрана и высотой окна
  IMsgBox_ScreenHeightGap      = 100;
   // Минимальная ширина клиентской части диалога
  IMsgBox_MinClientWidth       = 300;
   // Расстояние от правого края надписи (текста сообщения) до правого края окна
  IMsgBox_LabelRightMargin     = 20;
   // Левый отступ чекбокса "Больше не показывать..."
  IMsgBox_CBDontShowLeftMargin = 12;
   // Ширина чекбокса "Больше не показывать..."
  IMsgBox_CBDontShowWidth      = 280;
   // Ширина кнопки диалога
  IMsgBox_ButtonWidth          = 79;
   // Высота кнопки диалога
  IMsgBox_ButtonHeight         = 23;
   // Расстояние от верхнего края кнопок до контролов
  IMsgBox_ButtonTopMargin      = 20;
   // Расстояние от нижнего края кнопок до края окна
  IMsgBox_ButtonBottomMargin   = 8;
   // Расстояние от правого края самой правой кнопки до правого края окна
  IMsgBox_ButtonRightMargin    = 12;
   // Минимальное расстояние от левого края самой левой кнопки до левого края окна
  IMsgBox_ButtonLeftMargin     = 12;
   // Расстояние между кнопками по горизонтали
  IMsgBox_ButtonGap            = 8;

implementation
{$R *.dfm}
uses phUtils, phSettings, ChmHlp;

  function PhoaMsgBox(AKind: TMessageBoxKind; const sMessage: String; bMsgIsConstName, bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults;
  begin
    with TdMsgBox.Create(Application) do
      try
        FKind        := AKind;
        FButtons     := AButtons;
        FDiscardable := bDiscardable;
        if bMsgIsConstName then FMessage := ConstVal(sMessage) else FMessage := sMessage;
        Result := Execute;
      finally
        Free;
      end;
  end;

  function PhoaMsgBox(AKind: TMessageBoxKind; const sMessage: String; const aParams: Array of const; bMsgIsConstName, bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults;
  begin
    if bMsgIsConstName then
      Result := PhoaMsgBox(AKind, ConstVal(sMessage, aParams), False, bDiscardable, AButtons)
    else
      Result := PhoaMsgBox(AKind, Format(sMessage, aParams), False, bDiscardable, AButtons);
  end;

  procedure PhoaInfo(bWarning: Boolean; const sConstName: String; iSettingID: Integer = 0);
  const aKinds: Array[Boolean] of TMessageBoxKind = (mbkInfo, mbkWarning);
  var mbr: TMessageBoxResults;
  begin
    if (iSettingID<=0) or SettingValueBool(iSettingID) then begin
      mbr := PhoaMsgBox(aKinds[bWarning], sConstName, True, iSettingID>0, [mbbOK]);
      if (iSettingID>0) and (mbrDontShow in mbr) then SetSettingValueBool(iSettingID, False);
    end;
  end;

  procedure PhoaInfo(bWarning: Boolean; const sConstName: String; const aParams: Array of const; iSettingID: Integer = 0);
  const aKinds: Array[Boolean] of TMessageBoxKind = (mbkInfo, mbkWarning);
  var mbr: TMessageBoxResults;
  begin
    if (iSettingID<=0) or SettingValueBool(iSettingID) then begin
      mbr := PhoaMsgBox(aKinds[bWarning], sConstName, aParams, True, iSettingID>0, [mbbOK]);
      if (iSettingID>0) and (mbrDontShow in mbr) then SetSettingValueBool(iSettingID, False);
    end;
  end;

  function PhoaConfirm(bWarning: Boolean; const sConstName: String; iSettingID: Integer = 0): Boolean;
  const aKinds: Array[Boolean] of TMessageBoxKind = (mbkConfirm, mbkConfirmWarning);
  var mbr: TMessageBoxResults;
  begin
    if (iSettingID<=0) or SettingValueBool(iSettingID) then begin
      mbr := PhoaMsgBox(aKinds[bWarning], sConstName, True, iSettingID>0, [mbbOK, mbbCancel]);
      Result := mbrOK in mbr;
      if Result and (iSettingID>0) and (mbrDontShow in mbr) then SetSettingValueBool(iSettingID, False);
    end else
      Result := True;
  end;

  function PhoaConfirm(bWarning: Boolean; const sConstName: String; const aParams: Array of const; iSettingID: Integer = 0): Boolean;
  const aKinds: Array[Boolean] of TMessageBoxKind = (mbkConfirm, mbkConfirmWarning);
  var mbr: TMessageBoxResults;
  begin
    if (iSettingID<=0) or SettingValueBool(iSettingID) then begin
      mbr := PhoaMsgBox(aKinds[bWarning], sConstName, aParams, True, iSettingID>0, [mbbOK, mbbCancel]);
      Result := mbrOK in mbr;
      if Result and (iSettingID>0) and (mbrDontShow in mbr) then SetSettingValueBool(iSettingID, False);
    end else
      Result := True;
  end;

  procedure PhoaError(const sConstName: String);
  begin
    PhoaMsgBox(mbkError, sConstName, True, False, [mbbOK]);
  end;

  procedure PhoaError(const sConstName: String; const aParams: Array of const);
  begin
    PhoaMsgBox(mbkError, sConstName, aParams, True, False, [mbbOK]);
  end;

   //===================================================================================================================
   // TdMsgBox
   //===================================================================================================================

  procedure TdMsgBox.AdjustBounds;
  var iCWidth, iCHeight: Integer;
  begin
     // Учитываем размеры надписи
    iCWidth  := lMessage.Left+lMessage.Width+IMsgBox_LabelRightMargin;
    iCHeight := iIcon.Top+Max(iIcon.Height, lMessage.Height);
     // Добавляем размеры кнопок / cbDontShowAgain
    Inc(iCHeight, IMsgBox_ButtonTopMargin+IMsgBox_ButtonHeight+IMsgBox_ButtonBottomMargin);
    iCWidth := Max(iCWidth, FButtonWidths+iif(FDiscardable, IMsgBox_CBDontShowWidth, 0));
     // Проверяем, что ширина не меньше минимальной
    iCWidth := Max(iCWidth, IMsgBox_MinClientWidth);
     // Устанавливаем размер
    SetBounds(
      0,
      0,
      Min(iCWidth+(Width-ClientWidth), Screen.WorkAreaWidth-IMsgBox_ScreenWidthGap),
      Min(iCHeight+(Height-ClientHeight), Screen.WorkAreaHeight-IMsgBox_ScreenHeightGap));
  end;

  procedure TdMsgBox.AdjustButtons;
  var
    iBtnRightX: Integer;
    btn: TMessageBoxButton;

     // Создаёт кнопку, если она требуется, и уменьшает FButtonRightX на размер кнопки + зазор между кнопками
    procedure MakeButton(mbb: TMessageBoxButton);
    const
      aBtnCaptionConsts: Array[TMessageBoxButton] of String = (
        'SBtn_Help',     // mbbHelp
        'SBtn_Cancel',   // mbbCancel
        'SBtn_OK',       // mbbOK
        'SBtn_NoToAll',  // mbbNoToAll
        'SBtn_No',       // mbbNo
        'SBtn_YesToAll', // mbbYesToAll
        'SBtn_Yes');     // mbbYes
      aBtnResults: Array[TMessageBoxButton] of TMessageBoxResult = (
        TMessageBoxResult(-1), // mbbHelp
        mbrCancel,             // mbbCancel
        mbrOK,                 // mbbOK
        mbrNoToAll,            // mbbNoToAll
        mbrNo,                 // mbbNo
        mbrYesToAll,           // mbbYesToAll
        mbrYes);               // mbbYes
    var Button: TButton;
    begin
      Button := TButton.Create(Self);
      with Button do begin
        Parent  := Self;
        SetBounds(
          iBtnRightX-IMsgBox_ButtonWidth,
          Self.ClientHeight-IMsgBox_ButtonHeight-IMsgBox_ButtonBottomMargin,
          IMsgBox_ButtonWidth,
          IMsgBox_ButtonHeight);
        Anchors := [akRight, akBottom];
        Cancel  := (mbb=mbbCancel) or ((mbb=mbbOK) and not (mbbCancel in FButtons)) or ((mbb=mbbNo) and (FButtons*[mbbOK, mbbCancel]=[]));
        Caption := ConstVal(aBtnCaptionConsts[mbb]);
        Default := (mbb=mbbOK) or ((mbb=mbbYes) and not (mbbOK in FButtons));
        if Default then Self.ActiveControl := Button;
        if mbb=mbbHelp then OnClick := BtnHelpClick else OnClick := BtnClick;
        Tag     := Byte(aBtnResults[mbb]);
      end;
    end;

  begin
    iBtnRightX := ClientWidth-IMsgBox_ButtonRightMargin;
    if FButtons<>[] then begin
       // Перебираем и создаём кнопки
      for btn := Low(btn) to High(btn) do
        if btn in FButtons then begin
          MakeButton(btn);
          Dec(iBtnRightX, IMsgBox_ButtonWidth);
          if btn<High(btn) then Dec(iBtnRightX, IMsgBox_ButtonGap);
        end;
    end;
     // Считаем ширину
    FButtonWidths := ClientWidth-iBtnRightX+IMsgBox_ButtonLeftMargin;
     // Настраиваем переключатель
    if FDiscardable then
      cbDontShowAgain.SetBounds(
        IMsgBox_CBDontShowLeftMargin,
        Self.ClientHeight-IMsgBox_ButtonBottomMargin-((IMsgBox_ButtonHeight+cbDontShowAgain.Height) div 2),
        IMsgBox_CBDontShowWidth,
        cbDontShowAgain.Height);
    cbDontShowAgain.Visible := FDiscardable;
  end;

  procedure TdMsgBox.AdjustCaption;
  const
    aCaptionConsts: Array[TMessageBoxKind] of String = (
      'SDlgTitle_Info',           // mbkInfo
      'SDlgTitle_Warning',        // mbkWarning
      'SDlgTitle_Confirm',        // mbkConfirm
      'SDlgTitle_ConfirmWarning', // mbkConfirmWarning
      'SDlgTitle_Error');         // mbkError
  begin
    Caption := ConstVal(aCaptionConsts[FKind]);
  end;

  procedure TdMsgBox.AdjustIcon;
  const
    aIconIDs: Array[TMessageBoxKind] of PAnsiChar = (
      IDI_ASTERISK,    // mbkInfo
      IDI_EXCLAMATION, // mbkWarning
      IDI_QUESTION,    // mbkConfirm
      IDI_EXCLAMATION, // mbkConfirmWarning
      IDI_HAND);       // mbkError
  begin
    iIcon.Picture.Icon.Handle := LoadIcon(0, aIconIDs[FKind]);
  end;

  procedure TdMsgBox.AdjustMessage;
  begin
    with lMessage do begin
      Caption  := FMessage;
       // Выставляем ширину текста побольше и пересчитываем размеры надписи
      Width    := Screen.WorkAreaWidth-IMsgBox_ScreenWidthGap-Left-IMsgBox_LabelRightMargin;
      AutoSize := True;
       // Если высота надписи меньше высоты значка, центрируем по вертикали надпись относительно значка
      if Height<iIcon.Height then Top := iIcon.Top+((iIcon.Height-Height) div 2);
    end;
  end;

  procedure TdMsgBox.AdjustSound;
  const
    aSndConsts: Array[TMessageBoxKind] of Integer = (
      MB_ICONINFORMATION, // mbkInfo
      MB_ICONEXCLAMATION, // mbkWarning
      MB_ICONQUESTION,    // mbkConfirm
      MB_ICONEXCLAMATION, // mbkConfirmWarning
      MB_ICONERROR);      // mbkError
  begin
    MessageBeep(aSndConsts[FKind]);
  end;

  procedure TdMsgBox.BtnClick(Sender: TObject);
  begin
    FResults := [];
    Include(FResults, TMessageBoxResult(TComponent(Sender).Tag));
    ModalResult := mrOK;
  end;

  procedure TdMsgBox.BtnHelpClick(Sender: TObject);
  begin
    HtmlHelpContext(HelpContext);
  end;

  function TdMsgBox.Execute: TMessageBoxResults;
  begin
    InitializeDlg;
    ShowModal;
    if FDiscardable and cbDontShowAgain.Checked then Include(FResults, mbrDontShow);
    Result := FResults;
  end;

  procedure TdMsgBox.InitializeDlg;
  begin
    FResults := [mbrCancel];
     // Настраиваем окошко
    AdjustCaption;
    AdjustIcon;
    AdjustMessage;
    AdjustButtons;
    AdjustBounds;
    AdjustSound;
  end;

end.
