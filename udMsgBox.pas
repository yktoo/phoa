unit udMsgBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars,
  phDlg, StdCtrls, ExtCtrls, DTLangTools;

type
  TdMsgBox = class(TForm)
    bvBottom: TBevel;
    pButtonsBottom: TPanel;
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
     // Положение правой границы кнопки без учёта зазора, инициализируется в AdjustButtons()
    FButtonRightX: Integer;
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
  end;

   // Отображает [почти] стандартный диалог-сообщение.
   //   AKind           - вид сообщения. Влияет на заголовок, значок и звуковой сигнал
   //   sMessage        - текст сообщения
   //   AButtons        - кнопки, которые должны отображаться в диалоге
   //   bMsgIsConstName - если True, то sMessage трактуется как имя константы, а её значение подставляется вместо текста
   //   bDiscardable    - если True, то дополнительно отображает checkbox 'Больше не показывать данное сообщение'
   //   Возвращает набор, соотоящий из одной из кнопок и, опционально, mbrDontShow (если включен переключатель "Больше
   //     не показывать..." и bDiscardable=True)
  function PhoaMsgBox(AKind: TMessageBoxKind; const sMessage: String; AButtons: TMessageBoxButtons; bMsgIsConstName, bDiscardable: Boolean): TMessageBoxResults; overload;
   // То же самое, но дополнительно принимает массив параметров для форматирования
  function PhoaMsgBox(AKind: TMessageBoxKind; const sMessage: String; const aParams: Array of const; AButtons: TMessageBoxButtons; bMsgIsConstName, bDiscardable: Boolean): TMessageBoxResults; overload;

const
   // Минимальная разница между шириной экрана и шириной окна
  IMsgBox_ScreenGap         = 100;
   // Расстояние от правого края надписи (текста сообщения) до правого края окна
  IMsgBox_LabelRightMargin  = 12;
   // Расстояние от нижнего края надписи или значка до нижнего края рабочей области
  IMsgBox_BottomMargin      = 12;
   // Ширина кнопки диалога
  IMsgBox_ButtonWidth       = 75;
   // Высота кнопки диалога
  IMsgBox_ButtonHeight      = 23;
   // Положение верхнего края кнопок внутри pButtonsBottom
  IMsgBox_ButtonTop         = 8;
   // Расстояние от правого края самой правой кнопки до правого края окна
  IMsgBox_ButtonRightMargin = 8;
   // Минимальное расстояние от левого края самой левой кнопки до левого края окна
  IMsgBox_ButtonLeftMargin  = 8;
   // Расстояние между кнопками по горизонтали
  IMsgBox_ButtonGap         = 8;

implementation
{$R *.dfm}
uses phUtils;

  function PhoaMsgBox(AKind: TMessageBoxKind; const sMessage: String; AButtons: TMessageBoxButtons; bMsgIsConstName, bDiscardable: Boolean): TMessageBoxResults;
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

  function PhoaMsgBox(AKind: TMessageBoxKind; const sMessage: String; const aParams: Array of const; AButtons: TMessageBoxButtons; bMsgIsConstName, bDiscardable: Boolean): TMessageBoxResults;
  begin
    if bMsgIsConstName then
      Result := PhoaMsgBox(AKind, ConstVal(sMessage, aParams), AButtons, False, bDiscardable)
    else
      Result := PhoaMsgBox(AKind, Format(sMessage, aParams), AButtons, False, bDiscardable);
  end;

   //===================================================================================================================
   // TdMsgBox
   //===================================================================================================================

  procedure TdMsgBox.AdjustBounds;
  var iCWidth, iCHeight: Integer;
  begin
     // Учитываем размеры надписи
    iCWidth  := lMessage.Left+lMessage.Width+IMsgBox_LabelRightMargin;
    iCHeight := Max(iIcon.Height, lMessage.Height)+IMsgBox_BottomMargin+pButtonsBottom.Height;
     // Учитываем cbDontShowAgain
    if FDiscardable then Inc(iCHeight, bvBottom.Top-cbDontShowAgain.Top);
     // Учитываем ширину кнопок
    iCWidth := Max(iCWidth, ); 
  end;

  procedure TdMsgBox.AdjustButtons;
  var btn: TMessageBoxButton;

     // Создаёт кнопку, если она требуется, и уменьшает FButtonRightX на размер кнопки + зазор между кнопками
    procedure MakeButton(mbb: TMessageBoxButton);
    const
      aBtnCaptionConsts: Array[TMessageBoxButton] of String = (
        'SBtn_Yes',      // mbbYes
        'SBtn_No',       // mbbNo
        'SBtn_OK',       // mbbOK
        'SBtn_Cancel',   // mbbCancel
        'SBtn_YesToAll', // mbbYesToAll
        'SBtn_NoToAll',  // mbbNoToAll
        'SBtn_Help');    // mbbHelp
    var Button: TButton;
    begin
      Button := TButton.Create(Self);
      with Button do begin
        Parent  := pButtonsBottom;
        SetBounds(FButtonRightX-IMsgBox_ButtonWidth, IMsgBox_ButtonTop, IMsgBox_ButtonWidth, IMsgBox_ButtonHeight);
        Anchors := [akRight, akBottom];
        Cancel  := (mbb=mbbCancel) or ((mbb=mbbNo) and not (mbbCancel in FButtons));
        Caption := ConstVal(aBtnCaptionConsts[mbb]);
        Default := (mbb=mbbOK) or ((mbb=mbbYes) and not (mbbOK in FButtons));
        OnClick := BtnClick;
        Tag     := Byte(mbb);
        FButtonRightX := Left-IMsgBox_ButtonGap;
      end;
    end;

  begin
    FButtonRightX := pButtonsBottom.ClientWidth-IMsgBox_ButtonRightMargin;
    if FButtons<>[] then begin
       // Перебираем и создаём кнопки
      for btn := Low(btn) to High(btn) do
        if btn in FButtons then MakeButton(btn);
       // Убираем последний зазор
      Inc(FButtonRightX, IMsgBox_ButtonGap);
    end;
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
  begin
    //!!!
  end;

  procedure TdMsgBox.AdjustMessage;
  begin
    with lMessage do begin
      Text  := FMessage;
       // Выставляем ширину текста побольше и пересчитываем размеры надписи
      Width := Screen.WorkAreaWidth-IMsgBox_ScreenGap-Left-IMsgBox_LabelRightMargin;
      AutoSize := True;
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
    AdjustMessage;
    AdjustButtons;
    AdjustBounds;
    AdjustSound;
  end;

end.
