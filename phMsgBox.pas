//**********************************************************************************************************************
//  $Id: phMsgBox.pas,v 1.2 2007-06-24 17:47:59 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phMsgBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars,
  phDlg, StdCtrls, ExtCtrls, DKLang;

type
  TdMsgBox = class(TForm)
    iIcon: TImage;
    lMessage: TLabel;
    cbDontShowAgain: TCheckBox;
    dklcMain: TDKLanguageController;
  private
     // Вид сообщения
    FKind: TMessageBoxKind;
     // Текст сообщения
    FMessage: WideString;
     // Кнопки, которые должны отображаться в диалоге
    FButtons: TMessageBoxButtons;
     // Если True, то дополнительно отображает checkbox 'Больше не показывать данное сообщение'
    FDiscardable: Boolean;
     // Кнопки диалога
    FButtonCtls: Array of TButton;
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
     // Создаёт и настраивает кнопки и переключатель "Больше не показывать..."
    procedure AdjustButtons;
     // Настраивает размер/положение окна и позиционирует кнопки (центрируя) 
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
   //   wsMessage       - текст сообщения
   //   AButtons        - кнопки, которые должны отображаться в диалоге
   //   bDiscardable    - если True, то дополнительно отображает checkbox 'Больше не показывать данное сообщение'
   //   Возвращает набор, соотоящий из одной из кнопок и, опционально, mbrDontShow (если включен переключатель "Больше
   //     не показывать..." и bDiscardable=True)
  function PhoaMsgBox(AKind: TMessageBoxKind; const wsMessage: WideString; bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults; overload;
   // То же самое, но дополнительно принимает массив параметров для форматирования
  function PhoaMsgBox(AKind: TMessageBoxKind; const wsMessage: WideString; const aParams: Array of const; bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults; overload;
   // То же самое, но вместо текста сообщения передаётся имя соответствующей константы
  function PhoaMsgBoxConst(AKind: TMessageBoxKind; const sConstName: AnsiString; bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults; overload;
  function PhoaMsgBoxConst(AKind: TMessageBoxKind; const sConstName: AnsiString; const aParams: Array of const; bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults; overload;
   // Отображает диалог информации, если iSettingID<=0 или значение настройки iSettingID=True, иначе диалог не
   //   отображается. При iSettingID>0 в диалоге отображается также переключатель "Больше не показывать...". Если
   //   пользователь включит его и нажмёт ОК, то в значение настройки заносится False
   //   Если bWarning=True, то отображается в режиме mbkWarning, иначе - mbkInfo
  procedure PhoaInfo(bWarning: Boolean; const wsConstName: WideString; iSettingID: Integer = 0); overload;
  procedure PhoaInfo(bWarning: Boolean; const wsConstName: WideString; const aParams: Array of const; iSettingID: Integer = 0); overload;
   // Отображает диалог подтверждения, если iSettingID<=0 или значение настройки iSettingID=True, иначе диалог не
   //   отображается, а просто возвращает True. При iSettingID>0 в диалоге отображается также переключатель "Больше не
   //   показывать...". Если пользователь включит его и нажмёт ОК, то в значение настройки заносится False
   //   Если bWarning=True, то отображается в режиме mbkConfirmWarning, иначе - mbkConfirm
  function  PhoaConfirm(bWarning: Boolean; const wsConstName: WideString; iSettingID: Integer = 0): Boolean; overload;
  function  PhoaConfirm(bWarning: Boolean; const wsConstName: WideString; const aParams: Array of const; iSettingID: Integer = 0): Boolean; overload;
   // Отображает диалог ошибки
  procedure PhoaError(const wsConstName: WideString); overload;
  procedure PhoaError(const wsConstName: WideString; const aParams: Array of const); overload;

const
   // Минимальная разница между шириной экрана и шириной окна
  IMsgBox_ScreenWidthGap       = 100;
   // Минимальная разница между высотой экрана и высотой окна
  IMsgBox_ScreenHeightGap      = 100;
   // Минимальная ширина клиентской части диалога
  IMsgBox_MinClientWidth       = 300;
   // Расстояние от правого края надписи (текста сообщения) до правого края окна
  IMsgBox_LabelRightMargin     = 11;
   // Расстояние от верхнего края чекбокса "Больше не показывать..." до контролов (надписи или значка)
  IMsgBox_CBDontShowTopMargin  = 11;
   // Левый отступ чекбокса "Больше не показывать..."
  IMsgBox_CBDontShowLeftMargin = 11;
   // Ширина чекбокса "Больше не показывать..."
  IMsgBox_CBDontShowWidth      = 280;
   // Ширина кнопки диалога
  IMsgBox_ButtonWidth          = 79;
   // Высота кнопки диалога
  IMsgBox_ButtonHeight         = 23;
   // Расстояние от верхнего края кнопок до контролов
  IMsgBox_ButtonTopMargin      = 11;
   // Расстояние от нижнего края кнопок до края окна
  IMsgBox_ButtonBottomMargin   = 11;
   // Расстояние от правого края самой правой кнопки до правого края окна
  IMsgBox_ButtonRightMargin    = 11;
   // Минимальное расстояние от левого края самой левой кнопки до левого края окна
  IMsgBox_ButtonLeftMargin     = 11;
   // Расстояние между кнопками по горизонтали
  IMsgBox_ButtonGap            = 6;

implementation
{$R *.dfm}
uses phUtils, phSettings, phChmHlp;

  function PhoaMsgBox(AKind: TMessageBoxKind; const wsMessage: WideString; bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults;
  begin
    with TdMsgBox.Create(Application) do
      try
        FKind        := AKind;
        FButtons     := AButtons;
        FDiscardable := bDiscardable;
        if bMsgIsConstName then FMessage := ConstVal(wsMessage) else FMessage := wsMessage;
        Result := Execute;
      finally
        Free;
      end;
  end;

  function PhoaMsgBox(AKind: TMessageBoxKind; const wsMessage: WideString; const aParams: Array of const; bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults;
  begin
    Result := PhoaMsgBox(AKind, WideFormat(wsMessage, aParams), bDiscardable, AButtons);
  end;

  function PhoaMsgBoxConst(AKind: TMessageBoxKind; const sConstName: AnsiString; bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults;
  begin
    Result := PhoaMsgBox(AKind, ConstVal(sConstName), bDiscardable, AButtons);
  end;

  function PhoaMsgBoxConst(AKind: TMessageBoxKind; const sConstName: AnsiString; const aParams: Array of const; bDiscardable: Boolean; AButtons: TMessageBoxButtons): TMessageBoxResults;
  begin
    Result := PhoaMsgBox(AKind, ConstVal(sConstName, aParams), bDiscardable, AButtons);
  end;

  procedure PhoaInfo(bWarning: Boolean; const wsConstName: WideString; iSettingID: Integer = 0);
  const aKinds: Array[Boolean] of TMessageBoxKind = (mbkInfo, mbkWarning);
  var mbr: TMessageBoxResults;
  begin
    if (iSettingID<=0) or SettingValueBool(iSettingID) then begin
      mbr := PhoaMsgBoxConst(aKinds[bWarning], wsConstName, iSettingID>0, [mbbOK]);
      if (iSettingID>0) and (mbrDontShow in mbr) then SetSettingValueBool(iSettingID, False);
    end;
  end;

  procedure PhoaInfo(bWarning: Boolean; const wsConstName: WideString; const aParams: Array of const; iSettingID: Integer = 0);
  const aKinds: Array[Boolean] of TMessageBoxKind = (mbkInfo, mbkWarning);
  var mbr: TMessageBoxResults;
  begin
    if (iSettingID<=0) or SettingValueBool(iSettingID) then begin
      mbr := PhoaMsgBoxConst(aKinds[bWarning], wsConstName, aParams, iSettingID>0, [mbbOK]);
      if (iSettingID>0) and (mbrDontShow in mbr) then SetSettingValueBool(iSettingID, False);
    end;
  end;

  function PhoaConfirm(bWarning: Boolean; const wsConstName: WideString; iSettingID: Integer = 0): Boolean;
  const aKinds: Array[Boolean] of TMessageBoxKind = (mbkConfirm, mbkConfirmWarning);
  var mbr: TMessageBoxResults;
  begin
    if (iSettingID<=0) or SettingValueBool(iSettingID) then begin
      mbr := PhoaMsgBoxConst(aKinds[bWarning], wsConstName, iSettingID>0, [mbbOK, mbbCancel]);
      Result := mbrOK in mbr;
      if Result and (iSettingID>0) and (mbrDontShow in mbr) then SetSettingValueBool(iSettingID, False);
    end else
      Result := True;
  end;

  function PhoaConfirm(bWarning: Boolean; const wsConstName: WideString; const aParams: Array of const; iSettingID: Integer = 0): Boolean;
  const aKinds: Array[Boolean] of TMessageBoxKind = (mbkConfirm, mbkConfirmWarning);
  var mbr: TMessageBoxResults;
  begin
    if (iSettingID<=0) or SettingValueBool(iSettingID) then begin
      mbr := PhoaMsgBoxConst(aKinds[bWarning], wsConstName, aParams, iSettingID>0, [mbbOK, mbbCancel]);
      Result := mbrOK in mbr;
      if Result and (iSettingID>0) and (mbrDontShow in mbr) then SetSettingValueBool(iSettingID, False);
    end else
      Result := True;
  end;

  procedure PhoaError(const wsConstName: WideString);
  begin
    PhoaMsgBoxConst(mbkError, wsConstName, False, [mbbOK]);
  end;

  procedure PhoaError(const wsConstName: WideString; const aParams: Array of const);
  begin
    PhoaMsgBoxConst(mbkError, wsConstName, aParams, False, [mbbOK]);
  end;

   //===================================================================================================================
   // TdMsgBox
   //===================================================================================================================

  procedure TdMsgBox.AdjustBounds;
  var i, iBtnX, iBtnY, iCWidth, iCHeight: Integer;
  begin
     // Учитываем размеры надписи
    iCWidth  := lMessage.Left+lMessage.Width+IMsgBox_LabelRightMargin;
    iCHeight := iIcon.Top+Max(iIcon.Height, lMessage.Height);
     // Добавляем размеры кнопок / cbDontShowAgain
    Inc(iCHeight, IMsgBox_ButtonTopMargin+IMsgBox_ButtonHeight+IMsgBox_ButtonBottomMargin);
    if FDiscardable then Inc(iCHeight, IMsgBox_CBDontShowTopMargin+cbDontShowAgain.Height);
    iCWidth := Max(iCWidth, FButtonWidths+iif(FDiscardable, IMsgBox_CBDontShowWidth, 0));
     // Проверяем, что ширина не меньше минимальной
    iCWidth := Max(iCWidth, IMsgBox_MinClientWidth);
     // Устанавливаем размер
    SetBounds(
      0,
      0,
      Min(iCWidth+(Width-ClientWidth), Screen.WorkAreaWidth-IMsgBox_ScreenWidthGap),
      Min(iCHeight+(Height-ClientHeight), Screen.WorkAreaHeight-IMsgBox_ScreenHeightGap));
     // Позиционируем кнопки по центру диалога
    iBtnX := (ClientWidth-FButtonWidths) div 2 + IMsgBox_ButtonLeftMargin;
    iBtnY := ClientHeight-IMsgBox_ButtonBottomMargin-IMsgBox_ButtonHeight;
    for i := 0 to High(FButtonCtls) do begin
      FButtonCtls[i].SetBounds(iBtnX, iBtnY, IMsgBox_ButtonWidth, IMsgBox_ButtonHeight);
      Inc(iBtnX, IMsgBox_ButtonWidth+IMsgBox_ButtonGap);
    end;
  end;

  procedure TdMsgBox.AdjustButtons;
  var
    iBtnCount: Integer;
    btn: TMessageBoxButton;

     // Создаёт возвращает кнопку
    function MakeButton(mbb: TMessageBoxButton): TButton;
    const
      asBtnCaptionConsts: Array[TMessageBoxButton] of AnsiString = (
        'SBtn_Yes',      // mbbYes
        'SBtn_YesToAll', // mbbYesToAll
        'SBtn_No',       // mbbNo
        'SBtn_NoToAll',  // mbbNoToAll
        'SBtn_OK',       // mbbOK
        'SBtn_Cancel',   // mbbCancel
        'SBtn_Help');    // mbbHelp
      aBtnResults: Array[TMessageBoxButton] of TMessageBoxResult = (
        mbrYes,                 // mbbYes
        mbrYesToAll,            // mbbYesToAll
        mbrNo,                  // mbbNo
        mbrNoToAll,             // mbbNoToAll
        mbrOK,                  // mbbOK
        mbrCancel,              // mbbCancel
        TMessageBoxResult(-1)); // mbbHelp
    begin
      Result := TButton.Create(Self);
      with Result do begin
        Parent   := Self;
        Cancel   := (mbb=mbbCancel) or ((mbb=mbbOK) and not (mbbCancel in FButtons)) or ((mbb=mbbNo) and (FButtons*[mbbOK, mbbCancel]=[]));
        Caption  := ConstVal(asBtnCaptionConsts[mbb]);
        Default  := (mbb=mbbOK) or ((mbb=mbbYes) and not (mbbOK in FButtons));
        if Default then Self.ActiveControl := Result;
        if mbb=mbbHelp then OnClick := BtnHelpClick else OnClick := BtnClick;
        Tag      := Byte(aBtnResults[mbb]);
        TabOrder := iBtnCount-1;
      end;
    end;

  begin
     // Перебираем и создаём кнопки
    iBtnCount := 0;
    for btn := Low(btn) to High(btn) do
      if btn in FButtons then begin
        Inc(iBtnCount);
        SetLength(FButtonCtls, iBtnCount);
        FButtonCtls[iBtnCount-1] := MakeButton(btn);
      end;
     // Считаем ширину
    FButtonWidths :=
      IMsgBox_ButtonLeftMargin+
      iBtnCount*IMsgBox_ButtonWidth+
      (iBtnCount-1)*IMsgBox_ButtonGap+
      IMsgBox_ButtonRightMargin;
     // Настраиваем переключатель
    if FDiscardable then
      cbDontShowAgain.SetBounds(
        IMsgBox_CBDontShowLeftMargin,
        Self.ClientHeight-IMsgBox_ButtonBottomMargin-IMsgBox_ButtonHeight-IMsgBox_ButtonTopMargin-cbDontShowAgain.Height,
        IMsgBox_CBDontShowWidth,
        cbDontShowAgain.Height);
    cbDontShowAgain.Visible := FDiscardable;
  end;

  procedure TdMsgBox.AdjustCaption;
  const
    asCaptionConsts: Array[TMessageBoxKind] of AnsiString = (
      'SDlgTitle_Info',           // mbkInfo
      'SDlgTitle_Warning',        // mbkWarning
      'SDlgTitle_Confirm',        // mbkConfirm
      'SDlgTitle_ConfirmWarning', // mbkConfirmWarning
      'SDlgTitle_Error');         // mbkError
  begin
    Caption := ConstVal(asCaptionConsts[FKind]);
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
       // Ставим точку в конце, если её нет
      if (FMessage<>'') and not (FMessage[Length(FMessage)] in ['.', '!', '?', '…']) then FMessage := FMessage+'.';
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

