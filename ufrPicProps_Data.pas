//**********************************************************************************************************************
//  $Id: ufrPicProps_Data.pas,v 1.2 2004-04-15 12:54:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit ufrPicProps_Data;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars, phObj, phWizard,
  phPicPropsDlgPage, Mask, ToolEdit, DTLangTools, StdCtrls;

type
   // Состояние значения редактируемого свойства изображения
  TPicEditorPropValueState = (
    pvsUnassigned, // Свойство ещё не обрабатывалось
    pvsUniform,    // Значение свойства совпадает у всех выбранных изображений
    pvsVarious,    // Значение свойства различно у выбранных изображений
    pvsModified);  // Значение свойства было модифицировано пользователем

   // Значение редактируемого свойства изображения
  PPicEditorPropValue = ^TPicEditorPropValue;
  TPicEditorPropValue = record
    State:   TPicEditorPropValueState; // Состояние значения свойства
    sValue:  String;                   // Значение свойства при State=[pvsUniform, pvsModified]
    Control: TWinControl;              // [Основной] контрол, отвечающий за редактирование свойства
  end;

  TPicEditorPropValues = Array[TPicProperty] of TPicEditorPropValue;

  TfrPicProps_Data = class(TPicPropsDialogPage)
    lPlace: TLabel;
    lDesc: TLabel;
    lFilmNumber: TLabel;
    lFrameNumber: TLabel;
    lAuthor: TLabel;
    lNotes: TLabel;
    cbPlace: TComboBox;
    mDesc: TMemo;
    cbFilmNumber: TComboBox;
    eFrameNumber: TEdit;
    cbAuthor: TComboBox;
    mNotes: TMemo;
    dtlsMain: TDTLanguageSwitcher;
    eDate: TDateEdit;
    eTime: TMaskEdit;
    lDate: TLabel;
    lTime: TLabel;
    lMedia: TLabel;
    cbMedia: TComboBox;
    procedure PicPropEditorChange(Sender: TObject);
  private
     // Флаг того, что контролы на странице проинициализированы
    FInitialized: Boolean;
     // Текущие значения свойств
    FPropVals: TPicEditorPropValues;
     // Флаг принудительного занесения значений в контролы
    FForcingCtlVal: Boolean;
     // Загружает в контролы данные изображений
    procedure LoadPicControls;
     // Устанавливает значение свойства в соответствующем редакторе и его состояние. При bStateOnly=True меняет только
     //   состояние
    procedure SetPropEditor(Prop: TPicProperty; bStateOnly: Boolean);
     // Вызывается в случае модификации пользователем значения свойства Prop
    procedure PropValEdited(Prop: TPicProperty);
  protected
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure AfterDisplay(ChangeMethod: TPageChangeMethod); override;
  public
    function  CanApply: Boolean; override;
    procedure Apply(FOperations: TPhoaOperations); override;
  end;

implementation
{$R *.dfm}
uses phUtils, TypInfo;

const
   // Редактируемые свойства
  EditablePicProps: TPicProperties = [
    ppDate, ppTime, ppPlace, ppFilmNumber, ppFrameNumber, ppAuthor, ppDescription, ppNotes, ppMedia];

  procedure TfrPicProps_Data.AfterDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited AfterDisplay(ChangeMethod);
    StorageForm.ActiveControl := eDate;
  end;

  procedure TfrPicProps_Data.Apply(FOperations: TPhoaOperations);
  var
    ChgList: TPicPropertyChanges;
    Prop: TPicProperty;
  begin
    inherited Apply(FOperations);
     // Если страница посещалась
    if FInitialized then begin
       // Составляем список изменений
      ChgList := TPicPropertyChanges.Create;
      try
        for Prop := Low(Prop) to High(Prop) do
          if (Prop in EditablePicProps) and (FPropVals[Prop].State=pvsModified) then ChgList.Add(FPropVals[Prop].sValue, Prop);
         // Если есть изменения - создаём операцию изменения
        if ChgList.Count>0 then TPhoaOp_InternalEditPicProps.Create(FOperations, PhoA, EditedPicArray, ChgList);
      finally
        ChgList.Free;
      end;
    end;
  end;

  procedure TfrPicProps_Data.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
    if not FInitialized then begin
       // Загружаем списки мест, плёнок, авторов, носителей
      StringsLoadPFAM(PhoA, cbPlace.Items, cbFilmNumber.Items, cbAuthor.Items, cbMedia.Items);
       // Загружаем данные изображений в контролы
      LoadPicControls;
      FInitialized := True;
    end;
  end;

  function TfrPicProps_Data.CanApply: Boolean;
  var d: TDateTime;
  begin
     // Если вообще посещалась, проверяем дату и время
    Result := not FInitialized or (CheckMaskedDateTime(eDate.Text, False, d) and CheckMaskedDateTime(eTime.Text, True, d));
  end;

  procedure TfrPicProps_Data.LoadPicControls;

     // Заполняет массив FPropVals[], выставляя состояния
    procedure FillPropVals;
    var
      i: Integer;
      Pic: TPhoaPic;
      Prop: TPicProperty;
    begin
       // Цикл по изображениям
      for i := 0 to EditedPicCount-1 do begin
        Pic := EditedPics[i];
         // Обрабатываем все редактируемые свойства с состоянием pvsUnassigned или pvsUniform
        for Prop := Low(Prop) to High(Prop) do
          if Prop in EditablePicProps then
            with FPropVals[Prop] do
              case State of
                 // Значение ещё не присваивалось, это будет первым значением
                pvsUnassigned: begin
                  State  := pvsUniform;
                  sValue := Pic.Props[Prop];
                end;
                 // Значение уже было, сверяем текущее. Если не совпало - статус устанавливаем pvsVarious
                pvsUniform: if sValue<>Pic.Props[Prop] then State  := pvsVarious;
              end;
      end;
    end;

     // Настраивает контролы, загружая в них значения
    procedure AdjustControls;
    var Prop: TPicProperty;
    begin
      for Prop := Low(Prop) to High(Prop) do
          if Prop in EditablePicProps then SetPropEditor(Prop, False);
    end;

  begin
     // Инициализируем FPropVals[]
    FPropVals[ppDate].Control        := eDate;
    FPropVals[ppTime].Control        := eTime;
    FPropVals[ppPlace].Control       := cbPlace;
    FPropVals[ppFilmNumber].Control  := cbFilmNumber;
    FPropVals[ppFrameNumber].Control := eFrameNumber;
    FPropVals[ppAuthor].Control      := cbAuthor;
    FPropVals[ppDescription].Control := mDesc;
    FPropVals[ppNotes].Control       := mNotes;
    FPropVals[ppMedia].Control       := cbMedia;
     // Проверяем, какие свойства совпадают, а какие нет
    FillPropVals;
     // Настраиваем контролы
    AdjustControls;
  end;

  procedure TfrPicProps_Data.PicPropEditorChange(Sender: TObject);
  var Prop: TPicProperty;
  begin
    if FForcingCtlVal then Exit;
     // Находим контрол
    for Prop := Low(Prop) to High(Prop) do
      if (Prop in EditablePicProps) and (FPropVals[Prop].Control=Sender) then begin
         // Отрабатываем изменение
        PropValEdited(Prop);
        Break;
      end;
  end;

  procedure TfrPicProps_Data.PropValEdited(Prop: TPicProperty);
  var pv: PPicEditorPropValue;
  begin
    pv := @FPropVals[Prop];
    case Prop of
      ppDate:     pv.sValue := DateToStr(StrToDateDef(eDate.Text, 0));
      ppTime:     pv.sValue := TimeToStr(StrToTimeDef(eTime.Text, 0));
      ppPlace,
        ppFilmNumber,
        ppFrameNumber,
        ppAuthor,
        ppMedia:  pv.sValue := GetStrProp(pv.Control, 'Text');
      ppDescription,
        ppNotes:  pv.sValue := (pv.Control as TMemo).Text;
    end;
    pv.State  := pvsModified;
    SetPropEditor(Prop, True);
    Modified;
  end;

  procedure TfrPicProps_Data.SetPropEditor(Prop: TPicProperty; bStateOnly: Boolean);
  var pv: PPicEditorPropValue;
  begin
    FForcingCtlVal := True;
    try
      pv := @FPropVals[Prop];
       // Настраиваем значение
      if not bStateOnly then
        case Prop of
          ppDate: if pv.sValue='' then eDate.Clear else eDate.Date := StrToDate(pv.sValue);
          ppTime: if pv.sValue='' then eTime.Clear else eTime.Text := pv.sValue;
          ppPlace,
            ppFilmNumber,
            ppFrameNumber,
            ppAuthor,
            ppMedia: SetStrProp(pv.Control, 'Text', pv.sValue);
          ppDescription,
            ppNotes: (pv.Control as TMemo).Text := pv.sValue;
        end;
       // Настраиваем цвет
      SetOrdProp(pv.Control, 'Color', iif(pv.State=pvsVarious, clBtnFace, clWindow));
    finally
      FForcingCtlVal := False;
    end;
  end;

end.
