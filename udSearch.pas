//**********************************************************************************************************************
//  $Id: udSearch.pas,v 1.18 2004-10-18 19:27:03 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, 
  phDlg, StdCtrls, VirtualTrees, ExtCtrls, DKLang;

type
  TdSearch = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    tvCriteria: TVirtualStringTree;
    lCriteria: TLabel;
    gbSearch: TGroupBox;
    rbAll: TRadioButton;
    rbCurGroup: TRadioButton;
    rbSearchResults: TRadioButton;
    bReset: TButton;
    procedure bResetClick(Sender: TObject);
    procedure tvCriteriaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvCriteriaPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvCriteriaCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure tvCriteriaFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  private
     // Локальный список результатов поиска (ссылок на изображения)
    FLocalResults: IPhoaMutablePicList;
     // Приложение
    FApp: IPhotoAlbumApp;
     // Группа, в которую помещать результаты
    FResultsGroup: IPhotoAlbumPicGroup;
     // Основная процедура поиска
    procedure PerformSearch;
     // Сбрасывает критерии в первоначальное состояние
    procedure ResetCriteria;
  protected
    procedure InitializeDialog; override;
    procedure FinalizeDialog; override;
    procedure ButtonClick_OK; override;
  end;

  function DoSearch(AApp: IPhotoAlbumApp; ResultsGroup: IPhotoAlbumPicGroup): Boolean;

implementation
{$R *.dfm}
uses
  TypInfo, StrUtils, Mask, ToolEdit,
  phPhoa, phUtils, ConsVars, udSelKeywords, phSettings, udMsgBox;

  function DoSearch(AApp: IPhotoAlbumApp; ResultsGroup: IPhotoAlbumPicGroup): Boolean;
  begin
    with TdSearch.Create(Application) do
      try
        FApp          := AApp;
        FResultsGroup := ResultsGroup;
        Result := Execute;
      finally
        Free;
      end;
  end;

type
  TSearchCritType = (sctInteger, sctFileMasks, sctString, sctDate, sctTime, sctKeywords);
  TIntPropCondition = (ipcLess, ipcLessEqual, ipcEqual, ipcNotEqual, ipcGreaterEqual, ipcGreater);
  TMskPropCondition = (mpcMatches, mpcNotMatches);
  TStrPropCondition = (spcSpecified, spcNotSpecified, spcStarts, spcNotStarts, spcEqual, spcNotEqual, spcEnds, spcNotEnds, spcContains, spcNotContains);
  TDatPropCondition = (dpcSpecified, dpcNotSpecified, dpcLess, dpcLessEqual, dpcEqual, dpcNotEqual, dpcGreaterEqual, dpcGreater);
  TLstPropCondition = (lpcSpecified, lpcNotSpecified, lpcAny, lpcNone, lpcAll);
   // Запись критерия поиска
  PSearchCriteriaEntry = ^TSearchCriteriaEntry;
  TSearchCriteriaEntry = record
    CritType: TSearchCritType; // Тип критерия поиска
    Prop:     TPicProperty;    // Свойство, по которому проверяется
    sValue:   String;          // Значение критерия
    sHistKey: String;          // Ключ реестра, где хранится история (для комбобоксов)
    case TSearchCritType of    // Условие сравнения текущее и по умолчанию
      sctInteger:       (IntCond, IntDefCond: TIntPropCondition);
      sctFileMasks:     (MskCond, MskDefCond: TMskPropCondition);
      sctString:        (StrCond, StrDefCond: TStrPropCondition);
      sctDate, sctTime: (DatCond, DatDefCond: TDatPropCondition);
      sctKeywords:      (LstCond, LstDefCond: TLstPropCondition);
  end;

var
  aSearchCriteria: Array[0..17] of TSearchCriteriaEntry = (
    {00}(CritType: sctInteger;   Prop: ppID;            sValue: ''; sHistKey: SRegSearch_IDMRU;       IntCond: ipcEqual;        IntDefCond: ipcEqual),
    {01}(CritType: sctFileMasks; Prop: ppFileName;      sValue: ''; sHistKey: SRegSearch_FMaskMRU;    MskCond: mpcMatches;      MskDefCond: mpcMatches),
    {02}(CritType: sctString;    Prop: ppFilePath;      sValue: ''; sHistKey: SRegSearch_FPathMRU;    StrCond: spcContains;     StrDefCond: spcContains),
    {03}(CritType: sctInteger;   Prop: ppFileSizeBytes; sValue: ''; sHistKey: SRegSearch_FSizeMRU;    IntCond: ipcEqual;        IntDefCond: ipcEqual),
    {04}(CritType: sctInteger;   Prop: ppPicWidth;      sValue: ''; sHistKey: SRegSearch_PWidthMRU;   IntCond: ipcEqual;        IntDefCond: ipcEqual),
    {05}(CritType: sctInteger;   Prop: ppPicHeight;     sValue: ''; sHistKey: SRegSearch_PHeightMRU;  IntCond: ipcEqual;        IntDefCond: ipcEqual),
    {06}(CritType: sctDate;      Prop: ppDate;          sValue: ''; sHistKey: '';                     DatCond: dpcGreaterEqual; DatDefCond: dpcGreaterEqual),
    {07}(CritType: sctDate;      Prop: ppDate;          sValue: ''; sHistKey: '';                     DatCond: dpcLessEqual;    DatDefCond: dpcLessEqual),
    {08}(CritType: sctTime;      Prop: ppTime;          sValue: ''; sHistKey: '';                     DatCond: dpcGreaterEqual; DatDefCond: dpcGreaterEqual),
    {09}(CritType: sctTime;      Prop: ppTime;          sValue: ''; sHistKey: '';                     DatCond: dpcLessEqual;    DatDefCond: dpcLessEqual),
    {10}(CritType: sctString;    Prop: ppPlace;         sValue: ''; sHistKey: '';                     StrCond: spcContains;     StrDefCond: spcContains),
    {11}(CritType: sctString;    Prop: ppFilmNumber;    sValue: ''; sHistKey: '';                     StrCond: spcContains;     StrDefCond: spcContains),
    {12}(CritType: sctString;    Prop: ppFrameNumber;   sValue: ''; sHistKey: SRegSearch_FrNumberMRU; StrCond: spcContains;     StrDefCond: spcContains),
    {13}(CritType: sctString;    Prop: ppAuthor;        sValue: ''; sHistKey: '';                     StrCond: spcContains;     StrDefCond: spcContains),
    {14}(CritType: sctString;    Prop: ppDescription;   sValue: ''; sHistKey: SRegSearch_DescMRU;     StrCond: spcContains;     StrDefCond: spcContains),
    {15}(CritType: sctString;    Prop: ppNotes;         sValue: ''; sHistKey: SRegSearch_NotesMRU;    StrCond: spcContains;     StrDefCond: spcContains),
    {16}(CritType: sctString;    Prop: ppMedia;         sValue: ''; sHistKey: '';                     StrCond: spcContains;     StrDefCond: spcContains),
    {17}(CritType: sctKeywords;  Prop: ppKeywords;      sValue: ''; sHistKey: '';                     LstCond: lpcAny;          LstDefCond: lpcAny));
   // Списки все упоминаемых мест, номеров плёнок, авторов, носителей
  SLPhoaPlaces: TStringList;
  SLPhoaFilmNumbers: TStringList;
  SLPhoaAuthors: TStringList;
  SLPhoaMedia: TStringList;

const
   // Индексы отдельных критериев из aSearchCriteria[]
  ICritIdx_ID        = 00;
  ICritIdx_FileMasks = 01;
  ICritIdx_FileSize  = 03;
  ICritIdx_PicWidth  = 04;
  ICritIdx_PicHeight = 05;
  ICritIdx_Date1     = 06;
  ICritIdx_Date2     = 07;
  ICritIdx_Time1     = 08;
  ICritIdx_Time2     = 09;
  ICritIdx_Keywords  = 17;

   // Возвращает название условия
  function PropConditionName(CritType: TSearchCritType; const Condition): String;
  var pti: PTypeInfo;
  begin
    case CritType of
      sctInteger:       pti := TypeInfo(TIntPropCondition);
      sctFileMasks:     pti := TypeInfo(TMskPropCondition);
      sctString:        pti := TypeInfo(TStrPropCondition);
      sctDate, sctTime: pti := TypeInfo(TDatPropCondition);
      sctKeywords:      pti := TypeInfo(TLstPropCondition);
      else Exit;
    end;
    Result := ConstVal(GetEnumName(pti, Byte(Condition)));
  end;

   // Возвращает список строк условий для заданного типа, разделённых символом #13
  function GetConditionStrings(CritType: TSearchCritType): String;
  var b, bMax: Byte;
  begin
    Result := '';
    case CritType of
      sctInteger:       bMax := Byte(High(TIntPropCondition));
      sctFileMasks:     bMax := Byte(High(TMskPropCondition));
      sctString:        bMax := Byte(High(TStrPropCondition));
      sctDate, sctTime: bMax := Byte(High(TDatPropCondition));
      sctKeywords:      bMax := Byte(High(TLstPropCondition));
      else Exit;
    end;
    for b := 0 to bMax do AccumulateStr(Result, #13, PropConditionName(CritType, b));
  end;

   // Функции, проверяющие соответствие условиям
  function MatchesCondition(Condition: TIntPropCondition; iValue, iCritValue: Integer): Boolean; overload;
  begin
    Result := iCritValue<0;
    if not Result then
      case Condition of
        ipcLess:         Result := iValue<iCritValue;
        ipcLessEqual:    Result := iValue<=iCritValue;
        ipcEqual:        Result := iValue=iCritValue;
        ipcNotEqual:     Result := iValue<>iCritValue;
        ipcGreaterEqual: Result := iValue>=iCritValue;
        ipcGreater:      Result := iValue>iCritValue;
      end;
  end;

  function MatchesCondition(Condition: TMskPropCondition; const sValue: String; Masks: TPhoaMasks): Boolean; overload;
  begin
    Result := (Condition=mpcMatches) = Masks.Matches(sValue);
  end;

  function MatchesCondition(Condition: TStrPropCondition; const sValue, sCritValue: String): Boolean; overload;
  begin
    case Condition of
      spcSpecified:    Result := sValue<>'';
      spcNotSpecified: Result := sValue='';
      else begin
        Result := sCritValue='';
        if not Result then
          case Condition of
            spcStarts:      Result := AnsiStartsText(sCritValue, sValue);
            spcNotStarts:   Result := not AnsiStartsText(sCritValue, sValue);
            spcEqual:       Result := AnsiSameText(sValue, sCritValue);
            spcNotEqual:    Result := not AnsiSameText(sValue, sCritValue);
            spcEnds:        Result := AnsiEndsText(sCritValue, sValue);
            spcNotEnds:     Result := not AnsiEndsText(sCritValue, sValue);
            spcContains:    Result := AnsiContainsText(sValue, sCritValue);
            spcNotContains: Result := not AnsiContainsText(sValue, sCritValue);
          end;
      end;
    end;
  end;

  function MatchesCondition(Condition: TDatPropCondition; const iValue, iCritValue: Integer): Boolean; overload;
  begin
    case Condition of
      dpcSpecified:    Result := iValue>=0;
      dpcNotSpecified: Result := iValue<0;
      else begin
        Result := iCritValue<0;
        if not Result then
          case Condition of
            dpcLess:         Result := iValue<iCritValue;
            dpcLessEqual:    Result := iValue<=iCritValue;
            dpcEqual:        Result := iValue=iCritValue;
            dpcNotEqual:     Result := iValue<>iCritValue;
            dpcGreaterEqual: Result := iValue>=iCritValue;
            dpcGreater:      Result := iValue>iCritValue;
          end;
      end;
    end;
  end;

  function MatchesCondition(Condition: TLstPropCondition; List, CritList: IPhoaKeywordList): Boolean; overload;
  var i: Integer;
  begin
    case Condition of
      lpcSpecified:    Result := List.Count>0;
      lpcNotSpecified: Result := List.Count=0;
      else begin
        Result := CritList.Count=0;
         // Проверяем каждое слово
        if not Result then begin
          Result := Condition in [lpcNone, lpcAll];
          for i := 0 to CritList.Count-1 do
            if (List.IndexOf(CritList[i])>=0) xor (Condition=lpcAll) then begin
              Result := not Result;
              Break;
            end;
        end;
      end;
    end;
  end;

   //===================================================================================================================
   // VirtualStringTree EditLink для редакторов свойств
   //===================================================================================================================
type
  TPicPropEditMoveDirection = (emdNone, emdEnter, emdLeft, emdRight, emdUp, emdDown);

  TPicPropEditLink = class(TInterfacedObject, IVTEditLink)
  private
     // One of the property editor classes
    FWControl: TWinControl;
     // A back reference to the tree calling
    FTree: TVirtualStringTree;
     // The node being edited
    FNode: PVirtualNode;
     // The column of the node being edited
    FColumn: Integer;
     // Used to capture some important messages regardless of the type of control we use
    FOldWControlWndProc: TWndMethod;
     // Флаг прерывания процесса редактирования
    FEndingEditing: Boolean;
     // Флаг запрета окончания редактирования, используется на время вызова диалогового окна выбора ключевых слов
    FPreserveEndEdit: Boolean;
     // Обработчик оконной процедуры контрола
    procedure WControlWindowProc(var Msg: TMessage);
     // Обработчики событий контрола
    procedure WKeywordButtonClick(Sender: TObject);
    procedure WControlExit(Sender: TObject);
    procedure WControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
     // Преобразует константы VK_xxx в TPicPropEditMoveDirection
    function  VirtualKeyToDirection(VKey: Word): TPicPropEditMoveDirection;
     // Заканчивает редактирование (успешно) и сдвигает выделение в дереве при Direction<>emdNone, возвращает True, если
     //   удалось
    function  MoveSelection(Direction: TPicPropEditMoveDirection): Boolean;
  public
    destructor Destroy; override;
     // IVTEditLink
    function  BeginEdit: Boolean; stdcall;
    function  CancelEdit: Boolean; stdcall;
    function  EndEdit: Boolean; stdcall;
    function  GetBounds: TRect; stdcall;
    function  PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var Message: TMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  function TPicPropEditLink.BeginEdit: Boolean;
  begin
    Result := True;
    FWControl.Show;
    FWControl.SetFocus;
     // Set a window procedure hook (aka subclassing) to get notified about important messages
    FOldWControlWndProc := FWControl.WindowProc;
    FWControl.WindowProc := WControlWindowProc;
  end;

  function TPicPropEditLink.CancelEdit: Boolean;
  begin
    Result := True;
     // Restore the edit's window proc
    FWControl.WindowProc := FOldWControlWndProc;
    FWControl.Hide;
  end;

  destructor TPicPropEditLink.Destroy;
  begin
    FWControl.Free;
    inherited Destroy;
  end;

  function TPicPropEditLink.EndEdit: Boolean;
  var
    psce: PSearchCriteriaEntry;
    d: TDateTime;
  begin
    FEndingEditing := True;
    Result := True;
     // Restore the edit's window proc
    FWControl.WindowProc := FOldWControlWndProc;
    psce := @aSearchCriteria[FNode.Index];
     // Редактировали условие
    if FColumn=1 then
      psce.IntCond := TIntPropCondition((FWControl as TComboBox).ItemIndex)
     // Редактировали значение
    else begin
      case psce.Prop of
        ppDate: begin
          if not CheckMaskedDateTime((FWControl as TDateEdit).Text, False, d) then d := -1;
          psce.sValue := iif(d<0, '', DateToStr(d));
        end;
        ppTime: begin
          if not CheckMaskedDateTime((FWControl as TMaskEdit).Text, True, d) then d := -1;
          psce.sValue := iif(d<0, '', TimeToStrX(d));
        end;
        ppKeywords: psce.sValue := (FWControl as TComboEdit).Text;
        else begin
          psce.sValue := (FWControl as TComboBox).Text;
           // Сохраняем историю
          if psce.sHistKey<>'' then RegSaveHistory(psce.sHistKey, TComboBox(FWControl), True);
        end;
      end;
    end;
    FTree.InvalidateNode(FNode);
    FWControl.Hide;
  end;

  function TPicPropEditLink.GetBounds: TRect;
  begin
    Result := FWControl.BoundsRect;
  end;

  function TPicPropEditLink.MoveSelection(Direction: TPicPropEditMoveDirection): Boolean;
  var
    n: PVirtualNode;
    iCaretPos, iCol: Integer;
  begin
    Result := False;
    if FEndingEditing or FPreserveEndEdit then Exit;
     // ComboBox
    if FWControl is TComboBox then
      with TComboBox(FWControl) do begin
         // Если ComboBox раскрыт, ничего не двигаем
        if (Direction<>emdNone) and DroppedDown then Exit;
         // Сдвигаем выделение без вопросов, если DropDownList, просто закрытие или нажато вверх или вниз
        Result := (Style=csDropDownList) or (Direction in [emdNone, emdEnter, emdUp, emdDown]);
        iCaretPos := LongRec(Perform(CB_GETEDITSEL, 0, 0)).Hi;
      end
     // Прочие контролы (EDITs)
    else begin
       // Для DateEdit: если раскрыт Popup, ничего не двигаем
      if (Direction<>emdNone) and (FWControl is TDateEdit) and TDateEdit(FWControl).PopupVisible then Exit;
       // Сдвигаем выделение без вопросов, если просто закрытие или нажато вверх или вниз
      Result := Direction in [emdNone, emdEnter, emdUp, emdDown];
      iCaretPos := LongRec(FWControl.Perform(EM_GETSEL, 0, 0)).Hi;
       // В TCustomMaskEdit при наличии маски всегда есть выделение в 1 символ длиной
      if (FWControl is TCustomMaskEdit) and (TMaskEdit(FWControl).EditMask<>'') then Dec(iCaretPos);
    end;
     // Если сдвиг влево или вправо - разрешаем в том случае, если курсор у соответствующего края текста
    if not Result then
      case Direction of
        emdLeft:  Result := iCaretPos=0;
        emdRight: Result := iCaretPos=FWControl.Perform(WM_GETTEXTLENGTH, 0, 0);
      end;
     // Сдвигаем выделение
    if Result then
      with FTree do begin
        n := FocusedNode;
        iCol := FocusedColumn;
         // Определяем узел (строку) и столбец, куда двигать выделение
        case Direction of
          emdLeft:  Dec(iCol);
          emdRight: Inc(iCol);
          emdUp:    n := GetPrevious(n);
          emdDown:  n := GetNext(n);
        end;
         // Если всё в допустимых пределах - двигаем
        if (n<>nil) and (iCol>0) and (iCol<Header.Columns.Count) then begin
          FEndingEditing := True;
          EndEditNode;
          FocusedNode   := n;
          FocusedColumn := iCol;
          Selected[n] := True;
        end;
      end;
  end;

  function TPicPropEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
  var psce: PSearchCriteriaEntry;

     // Создаёт новый комбобокс. Если bCondition=True, то для выбора условия критерия, иначе для ввода значения
    function NewCombobox(bCondition: Boolean): TComboBox;
    begin
      Result := TComboBox.Create(nil);
      with Result do begin
        Visible       := False;
        Parent        := Tree;
        DropDownCount := 16;
        OnExit        := WControlExit;
        OnKeyDown     := WControlKeyDown;
        if bCondition then begin
          Style       := csDropDownList;
          Items.Text  := GetConditionStrings(psce.CritType);
          ItemIndex   := Byte(psce.IntCond);
        end else begin
           // Загружаем списки возможных вариантов или истории
          if psce.sHistKey='' then
            case psce.Prop of
              ppPlace:      Items.Assign(SLPhoaPlaces);
              ppFilmNumber: Items.Assign(SLPhoaFilmNumbers);
              ppAuthor:     Items.Assign(SLPhoaAuthors);
              ppMedia:      Items.Assign(SLPhoaMedia);
            end
          else
            RegLoadHistory(psce.sHistKey, Result, False);
          Text        := psce.sValue;
        end;
      end;
    end;

     // Создаёт новый DateEdit
    function NewDateEdit: TDateEdit;
    begin
      Result := TDateEdit.Create(nil);
      with Result do begin
        Visible    := False;
        Parent     := Tree;
        BlanksChar := #133;
        Text       := psce.sValue;
        OnExit     := WControlExit;
        OnKeyDown  := WControlKeyDown;
        if Date=0 then Clear;
      end;
    end;

     // Создаёт новый MaskEdit для ввода времени
    function NewTimeEdit: TMaskEdit;
    begin
      Result := TMaskEdit.Create(nil);
      with Result do begin
        Visible    := False;
        Parent     := Tree;
        EditMask   := '!99:99:99;1;…';
        MaxLength  := 8;
        Text       := psce.sValue;
        OnExit     := WControlExit;
        OnKeyDown  := WControlKeyDown;
      end;
    end;

     // Создаёт новый ComboEdit для ввода ключевых слов
    function NewKeywordEdit: TComboEdit;
    begin
      Result := TComboEdit.Create(nil);
      with Result do begin
        Visible       := False;
        Parent        := Tree;
        GlyphKind     := gkEllipsis;
        Text          := psce.sValue;
        OnButtonClick := WKeywordButtonClick;
        OnExit        := WControlExit;
        OnKeyDown     := WControlKeyDown;
      end
    end;

  begin
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;
     // Determine what edit type actually is needed
    FreeAndNil(FWControl);
    psce := @aSearchCriteria[FNode.Index];
    case Column of
       // *** Ячейка с условием критерия
      1: FWControl := NewCombobox(True);
       // *** Ячейка со значением критерия
      2:
        case psce.CritType of
          sctInteger,
            sctFileMasks,
            sctString: FWControl := NewCombobox(False);
          sctDate:     FWControl := NewDateEdit;
          sctTime:     FWControl := NewTimeEdit;
          sctKeywords: FWControl := NewKeywordEdit;
        end;
    end;
    Result := FWControl<>nil;
  end;

  procedure TPicPropEditLink.ProcessMessage(var Message: TMessage);
  begin
    FWControl.WindowProc(Message);
  end;

  procedure TPicPropEditLink.SetBounds(R: TRect);
  begin
    FTree.Header.Columns.GetColumnBounds(FColumn, R.Left, R.Right);
    FWControl.BoundsRect := R;
  end;

  function TPicPropEditLink.VirtualKeyToDirection(VKey: Word): TPicPropEditMoveDirection;
  begin
    case VKey of
      VK_LEFT:   Result := emdLeft;
      VK_RIGHT:  Result := emdRight;
      VK_UP:     Result := emdUp;
      VK_DOWN:   Result := emdDown;
      VK_RETURN: Result := emdEnter;
      else       Result := emdNone;
    end;
  end;

  procedure TPicPropEditLink.WControlExit(Sender: TObject);
  begin
    MoveSelection(emdNone);
  end;

  procedure TPicPropEditLink.WControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if Shift=[] then
      case Key of
        VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_RETURN: if MoveSelection(VirtualKeyToDirection(Key)) then Key := 0;
      end;
  end;

  procedure TPicPropEditLink.WControlWindowProc(var Msg: TMessage);
  begin
    case Msg.Msg of
      WM_GETDLGCODE: begin
        FOldWControlWndProc(Msg);
        Msg.Result := Msg.Result or DLGC_WANTALLKEYS;
        Exit;
      end;
      WM_KEYDOWN:
        if ((GetKeyState(VK_SHIFT) or GetKeyState(VK_CONTROL) or GetKeyState(VK_MENU)) and $80=0) then
          case Msg.WParam of
            VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_RETURN: if MoveSelection(VirtualKeyToDirection(Msg.WParam)) then Exit;
          end;
      WM_KILLFOCUS: begin
        MoveSelection(emdNone);
        Exit;
      end;
    end;
    FOldWControlWndProc(Msg);
  end;

  procedure TPicPropEditLink.WKeywordButtonClick(Sender: TObject);
  var s: String;
  begin
    FPreserveEndEdit := True;
    try
      s := (Sender as TComboEdit).Text;
      if SelectPhoaKeywords((FTree.Owner as TdSearch).FApp.Project, s) then TComboEdit(Sender).Text := s;
    finally
      FPreserveEndEdit := False;
    end;
  end;

   //===================================================================================================================
   // TfSearch
   //===================================================================================================================

  procedure TdSearch.bResetClick(Sender: TObject);
  begin
    ResetCriteria;
  end;

  procedure TdSearch.ButtonClick_OK;
  begin
    PerformSearch;
    if FLocalResults.Count=0 then
      PhoaInfo(False, 'SPicsNotFound')
    else begin
       // Fill search results
      FResultsGroup.PicsX.Assign(FLocalResults);
      inherited ButtonClick_OK;
    end;
  end;

  procedure TdSearch.FinalizeDialog;
  begin
    FLocalResults := nil;
    SLPhoaPlaces.Free;
    SLPhoaFilmNumbers.Free;
    SLPhoaAuthors.Free;
    SLPhoaMedia.Free;
    inherited FinalizeDialog;
  end;

  procedure TdSearch.InitializeDialog;

    function NewSL: TStringList;
    begin
      Result := TStringList.Create;
      Result.Sorted     := True;
      Result.Duplicates := dupIgnore;
    end;

  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_search;
    OKIgnoresModified := True;
    FLocalResults     := NewPhotoAlbumPicList(False);
     // Загружаем списки мест, номеров плёнок, авторов
    SLPhoaPlaces      := NewSL;
    SLPhoaFilmNumbers := NewSL;
    SLPhoaAuthors     := NewSL;
    SLPhoaMedia       := NewSL;
    StringsLoadPFAM(FApp.Project, SLPhoaPlaces, SLPhoaFilmNumbers, SLPhoaAuthors, SLPhoaMedia);
     // Настраиваем контролы
    rbCurGroup.Enabled      := (FApp.CurGroup<>nil) and (FApp.CurGroup.Pics.Count>0);
    rbSearchResults.Enabled := FResultsGroup.Pics.Count>0;
     // Инициализируем дерево
    ApplyTreeSettings(tvCriteria);
    tvCriteria.RootNodeCount := High(aSearchCriteria)+1;
  end;

  procedure TdSearch.PerformSearch;
  type TSearchArea = (saAll, saCurGroup, saResults);
  var
    iDate1, iDate2, iTime1, iTime2: Integer;
    Keywords: IPhotoAlbumKeywordList;
    FMasks: TPhoaMasks;
    i, iSrchCount, iID, iFSize, iPicWidth, iPicHeight: Integer;
    SearchArea: TSearchArea;
    Pic: IPhotoAlbumPic;

    function Matches(Pic: IPhotoAlbumPic): Boolean;
    var i: Integer;
    begin
      for i := 0 to High(aSearchCriteria) do begin
        with aSearchCriteria[i] do
          case i of
            ICritIdx_ID:        Result := MatchesCondition(IntCond, Pic.ID,                        iID);
            ICritIdx_FileSize:  Result := MatchesCondition(IntCond, Pic.FileSize,                  iFSize);
            ICritIdx_PicWidth:  Result := MatchesCondition(IntCond, Pic.ImageSize.cx,              iPicWidth);
            ICritIdx_PicHeight: Result := MatchesCondition(IntCond, Pic.ImageSize.cy,              iPicHeight);
            ICritIdx_FileMasks: Result := MatchesCondition(MskCond, ExtractFileName(Pic.FileName), FMasks);
            ICritIdx_Date1:     Result := MatchesCondition(DatCond, Pic.Date,                      iDate1);
            ICritIdx_Date2:     Result := MatchesCondition(DatCond, Pic.Date,                      iDate2);
            ICritIdx_Time1:     Result := MatchesCondition(DatCond, Pic.Time,                      iTime1);
            ICritIdx_Time2:     Result := MatchesCondition(DatCond, Pic.Time,                      iTime2);
            ICritIdx_Keywords:  Result := MatchesCondition(LstCond, Pic.Keywords,                  Keywords);
            else                Result := MatchesCondition(StrCond, Pic.Props[Prop],               sValue);
          end;
         // Если какой-то из критериев не выполнился, выходим (т.к. условие "И")
        if not Result then Break;
      end;
    end;

     // Преобразует строковое значение критерия в Дату PhoA; если не удалось, возвращает -1
    function GetCritDate(iCritIndex: Integer): Integer;
    var dt: TDateTime;
    begin
      dt := StrToDateDef(aSearchCriteria[iCritIndex].sValue, -1);
      if dt>=0 then Result := DateToPhoaDate(dt) else Result := -1;
    end;

     // Преобразует строковое значение критерия во Время PhoA; если не удалось, возвращает -1
    function GetCritTime(iCritIndex: Integer): Integer;
    var dt: TDateTime;
    begin
      dt := StrToTimeDef(aSearchCriteria[iCritIndex].sValue, -1);
      if dt>=0 then Result := TimeToPhoaTime(dt) else Result := -1;
    end;

  begin
    StartWait;
    try
      FMasks := TPhoaMasks.Create(aSearchCriteria[ICritIdx_FileMasks].sValue);
      try
         // Настраиваем область поиска
        if rbAll.Checked then begin
          SearchArea := saAll;
          iSrchCount := FApp.Project.Pics.Count;
        end else if rbCurGroup.Checked then begin
          SearchArea := saCurGroup;
          iSrchCount := FApp.CurGroup.Pics.Count;
        end else begin
          SearchArea := saResults;
          iSrchCount := FResultsGroup.Pics.Count;
        end;
         // Инициализируем критерии
        iID        := StrToIntDef(aSearchCriteria[ICritIdx_ID].sValue,        -1);
        iFSize     := StrToIntDef(aSearchCriteria[ICritIdx_FileSize].sValue,  -1);
        iPicWidth  := StrToIntDef(aSearchCriteria[ICritIdx_PicWidth].sValue,  -1);
        iPicHeight := StrToIntDef(aSearchCriteria[ICritIdx_PicHeight].sValue, -1);
        iDate1     := GetCritDate(ICritIdx_Date1);
        iDate2     := GetCritDate(ICritIdx_Date2);
        iTime1     := GetCritTime(ICritIdx_Time1);
        iTime2     := GetCritTime(ICritIdx_Time2);
        Keywords := NewPhotoAlbumKeywordList;
        Keywords.CommaText := aSearchCriteria[ICritIdx_Keywords].sValue;
         // Ищем
        FLocalResults.Clear;
        for i := 0 to iSrchCount-1 do begin
          case SearchArea of
            saAll:      Pic := FApp.Project.PicsX[i];
            saCurGroup: Pic := FApp.CurGroup.PicsX[i];
            else        Pic := FResultsGroup.PicsX[i];
          end;
          if Matches(Pic) then FLocalResults.Add(Pic, True);
        end;
      finally
        FMasks.Free;
      end;
    finally
      StopWait;
    end;
  end;

  procedure TdSearch.ResetCriteria;
  var i: Integer;
  begin
    for i := 0 to High(aSearchCriteria) do
      with aSearchCriteria[i] do begin
        sValue := '';
        IntCond := IntDefCond;
      end;
    tvCriteria.Invalidate;
  end;

  procedure TdSearch.tvCriteriaCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
  begin
    EditLink := TPicPropEditLink.Create;
  end;

  procedure TdSearch.tvCriteriaFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  begin
     // Редактируем ячейку, в которую входим
    if (Node<>nil) and not (tsIncrementalSearching in Sender.TreeStates) then Sender.EditNode(Node, Column);
  end;

  procedure TdSearch.tvCriteriaGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    psce: PSearchCriteriaEntry;
    s: String;
  begin
    psce := @aSearchCriteria[Node.Index];
    case Column of
      0: s := PicPropName(psce.Prop);
      1: s := PropConditionName(psce.CritType, psce.IntCond);
      2: s := psce.sValue;
    end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TdSearch.tvCriteriaPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    if Column=0 then TargetCanvas.Font.Style := [fsBold];
  end;

end.

