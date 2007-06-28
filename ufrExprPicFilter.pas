//**********************************************************************************************************************
//  $Id: ufrExprPicFilter.pas,v 1.5 2007-06-28 18:41:40 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrExprPicFilter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Registry,
  DKLang, SynCompletionProposal, TB2MRU, TBXExtItems, Menus, TB2Item, TBX,
  ActnList, SynEdit, TB2Dock, TB2Toolbar, TntActnList;

type
  TfrExprPicFilter = class(TTntFrame)
    aCopy: TTntAction;
    aCut: TTntAction;
    alMain: TTntActionList;
    aNew: TTntAction;
    aOpen: TTntAction;
    aPaste: TTntAction;
    aRedo: TTntAction;
    aSaveAs: TTntAction;
    aSyntaxCheck: TTntAction;
    aUndo: TTntAction;
    bCopy: TTBXItem;
    bCut: TTBXItem;
    bNew: TTBXItem;
    bOpen: TTBXSubmenuItem;
    bPaste: TTBXItem;
    bRedo: TTBXItem;
    bSaveAs: TTBXItem;
    bSyntaxCheck: TTBXItem;
    bUndo: TTBXItem;
    dkExprTop: TTBXDock;
    dklcMain: TDKLanguageController;
    eExpression: TSynEdit;
    iMRUOpen: TTBXMRUListItem;
    mruOpen: TTBXMRUList;
    pmExpression: TTBXPopupMenu;
    scpMain: TSynCompletionProposal;
    smInsertOperator: TTBXSubmenuItem;
    smInsertProp: TTBXSubmenuItem;
    tbExprMain: TTBXToolbar;
    tbSepCut: TTBXSeparatorItem;
    tbSepInsertProp: TTBXSeparatorItem;
    tbSepSyntaxCheck: TTBXSeparatorItem;
    tbSepUndo: TTBXSeparatorItem;
    procedure aaCopy(Sender: TObject);
    procedure aaCut(Sender: TObject);
    procedure aaNew(Sender: TObject);
    procedure aaOpen(Sender: TObject);
    procedure aaPaste(Sender: TObject);
    procedure aaRedo(Sender: TObject);
    procedure aaSaveAs(Sender: TObject);
    procedure aaSyntaxCheck(Sender: TObject);
    procedure aaUndo(Sender: TObject);
    procedure eExpressionChange(Sender: TObject);
    procedure eExpressionStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure mruOpenClick(Sender: TObject; const Filename: String);
    procedure EnableActionsNotify(Sender: TObject);
  private
     // Счётчик блокировки
    FUpdateLock: Integer;
     // True, если настройки были загружены из реестра
    FSettingsLoaded: Boolean;
     // Prop storage
    FOnExpressionChange: TNotifyEvent;
     // Блокировка/снятие блокировки обновления
    procedure BeginUpdate;
    procedure EndUpdate;
     // Возвращает базовый ключ реестра для сохранения настроек редактора
    function  GetRegistryKey: WideString;
     // Обновляет достпуность Actions
    procedure EnableActions;
     // Событие клика на пункте вставки свойства изображения в выражение
    procedure ExprInsertPropClick(Sender: TObject);
     // Событие клика на пункте вставки оператора в выражение
    procedure ExprInsertOpClick(Sender: TObject);
     // Загрузка/сохранение текущего выражения в файле
    procedure ExpressionLoad(const wsFileName: WideString);
    procedure ExpressionSave(const wsFileName: WideString);
     // Вызывает OnExpressionChange
    procedure DoExpressionChange;
     // Загрузка/сохрвнение настроек в реестре
    procedure LoadSettings;
    procedure SaveSettings;
     // Prop handlers
    function  GetCaretPos: TPoint;
    function  GetExpression: WideString;
    procedure SetCaretPos(const Value: TPoint);
    procedure SetExpression(const Value: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
     // Фокусирует редактор, если это возможно
    procedure FocusEditor;
     // Props
     // -- Положение курсора в редакторе
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;  
     // -- Собственно текст выражения
    property Expression: WideString read GetExpression write SetExpression;
     // Events
     // -- Вызывается при изменении выражения в редакторе
    property OnExpressionChange: TNotifyEvent read FOnExpressionChange write FOnExpressionChange;
  end;

implementation
{$R *.dfm}
uses
  SynEditTypes,
  phIntf, phUtils, phSettings, phParsingPicFilter, phPicFilterHighlighter, ConsVars, phMsgBox, Main;

var
   // Файл, использовавшийся в последний раз для загрузки/сохранения файла выражения
  wsLastExpressionFile: WideString;

  procedure TfrExprPicFilter.aaCopy(Sender: TObject);
  begin
    eExpression.CopyToClipboard;
  end;

  procedure TfrExprPicFilter.aaCut(Sender: TObject);
  begin
    eExpression.CutToClipboard;
  end;

  procedure TfrExprPicFilter.aaNew(Sender: TObject);
  begin
    eExpression.Clear;
  end;

  procedure TfrExprPicFilter.aaOpen(Sender: TObject);
  begin
    with TOpenDialog.Create(Self) do
      try
        DefaultExt := SDefaultSearchExpressionFileExt;
        FileName   := sLastExpressionFile;
        Filter     := DKLangConstW('SFileFilter_SearchExpr');
        Options    := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
        Title      := DKLangConstW('SDlgTitle_OpenSearchExpr');
        if Execute then ExpressionLoad(FileName);
      finally
        Free;
      end;
  end;

  procedure TfrExprPicFilter.aaPaste(Sender: TObject);
  begin
    eExpression.PasteFromClipboard;
  end;

  procedure TfrExprPicFilter.aaRedo(Sender: TObject);
  begin
    eExpression.Redo;
  end;

  procedure TfrExprPicFilter.aaSaveAs(Sender: TObject);
  begin
    with TSaveDialog.Create(Self) do
      try
        DefaultExt := SDefaultSearchExpressionFileExt;
        FileName   := sLastExpressionFile;
        Filter     := DKLangConstW('SFileFilter_SearchExpr');
        Options    := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
        Title      := DKLangConstW('SDlgTitle_SaveSearchExprAs');
        if Execute then ExpressionSave(FileName);
      finally
        Free;
      end;
  end;

  procedure TfrExprPicFilter.aaSyntaxCheck(Sender: TObject);
  var PicFilter: IPhoaParsingPicFilter;
  begin
    PicFilter := NewPhoaParsingPicFilter;
     // Присваиваем и разбираем выражение
    PicFilter.Expression := eExpression.Text;
    PicFilter.ParseExpression(True, False);
     // Если есть ошибки
    if PicFilter.HasErrors then begin
      CaretPos := PicFilter.ParseErrorLocation;
      PhoaMsgBox(mbkError, PicFilter.ParseErrorMsg, False, False, [mbbOK]);
     // Иначе сообщаем об успехе
    end else
      PhoaInfo(False, 'SMsg_SyntaxOK');
  end;

  procedure TfrExprPicFilter.aaUndo(Sender: TObject);
  begin
    eExpression.Undo;
  end;

  procedure TfrExprPicFilter.BeginUpdate;
  begin
    Inc(FUpdateLock);
  end;

  constructor TfrExprPicFilter.Create(AOwner: TComponent);

     // Добавляет пункты меню "Вставить свойство" и для scpMain
    procedure AddExprInsertPropItems;
    var
      pp: TPicProperty;
      wsProp: WideString;
    begin
      for pp := Low(pp) to High(pp) do begin
        wsProp := '$'+PicPropToStr(pp, True);
        AddTBXMenuItem( {!!! Not Unicode-enabled solution }
          smInsertProp,
          WideFormat('%s - %s', [wsProp, PicPropName(pp)]),
          -1,
          Byte(pp),
          ExprInsertPropClick);
        scpMain.AddItem(wsProp, wsProp); {!!! Not Unicode-enabled solution }
      end;
    end;

     // Добавляет пункты меню "Вставить оператор"
    procedure AddExprInsertOperatorItems;
    var ok: TPicFilterOperatorKind;
    begin
      for ok := Low(ok) to High(ok) do
        AddTBXMenuItem(smInsertOperator, asPicFilterOperators[ok], -1, Byte(ok), ExprInsertOpClick);
    end;

  begin
    inherited Create(AOwner);
     // Создаём пункты меню "Вставить свойство" и "Вставить оператор"
    AddExprInsertPropItems;
    AddExprInsertOperatorItems;
     // Инициализируем выражение
    FontFromStr(scpMain.Font, SettingValueStr(ISettingID_Gen_MainFont));
    scpMain.TitleFont.Assign(scpMain.Font);
    pmExpression.LinkSubitems := tbExprMain.Items;
    eExpression.Highlighter   := TSynPicFilterSyn.Create(Self);
     // Загружаем настройки
    LoadSettings;
    EnableActions;
  end;

  destructor TfrExprPicFilter.Destroy;
  begin
     // Сохраняем настройки
    SaveSettings;
    inherited Destroy;
  end;

  procedure TfrExprPicFilter.DoExpressionChange;
  begin
    if Assigned(FOnExpressionChange) then FOnExpressionChange(Self);
  end;

  procedure TfrExprPicFilter.eExpressionChange(Sender: TObject);
  begin
    if FUpdateLock=0 then DoExpressionChange;
  end;

  procedure TfrExprPicFilter.eExpressionStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  begin
    EnableActions;
  end;

  procedure TfrExprPicFilter.EnableActions;
  var bSExpr, bExprText, bExprSel: Boolean;
  begin
    if FUpdateLock>0 then Exit;
    bSExpr := eExpression.Focused;
    bExprText := eExpression.Lines.Count>0;
    bExprSel  := bExprText and eExpression.SelAvail;
    aNew.Enabled         := bSExpr and bExprText;
    aOpen.Enabled        := bSExpr;
    aSaveAs.Enabled      := bSExpr and bExprText;
    aCut.Enabled         := bSExpr and bExprSel;
    aCopy.Enabled        := bSExpr and bExprSel;
    aPaste.Enabled       := bSExpr;
    aUndo.Enabled        := bSExpr and eExpression.CanUndo;
    aRedo.Enabled        := bSExpr and eExpression.CanRedo;
    aSyntaxCheck.Enabled := bSExpr and bExprText;
  end;

  procedure TfrExprPicFilter.EnableActionsNotify(Sender: TObject);
  begin
    EnableActions;
  end;

  procedure TfrExprPicFilter.EndUpdate;
  begin
    if FUpdateLock>0 then Dec(FUpdateLock);
    if FUpdateLock=0 then begin
      EnableActions;
      DoExpressionChange;
    end;
  end;

  procedure TfrExprPicFilter.ExpressionLoad(const wsFileName: WideString);
  begin
    BeginUpdate;
    try
      eExpression.Lines.LoadFromFile(wsFileName); {!!! Not Unicode-enabled solution }
      mruOpen.Add(wsFileName);
      wsLastExpressionFile := wsFileName;
    finally
      EndUpdate;
    end;
  end;

  procedure TfrExprPicFilter.ExpressionSave(const wsFileName: WideString);
  begin
    BeginUpdate;
    try
      eExpression.Lines.SaveToFile(wsFileName); {!!! Not Unicode-enabled solution }
      mruOpen.Add(wsFileName);
      wsLastExpressionFile := wsFileName;
    finally
      EndUpdate;
    end;
  end;

  procedure TfrExprPicFilter.ExprInsertOpClick(Sender: TObject);
  begin
    eExpression.SelText := asPicFilterOperators[TPicFilterOperatorKind(TComponent(Sender).Tag)];
  end;

  procedure TfrExprPicFilter.ExprInsertPropClick(Sender: TObject);
  begin
    eExpression.SelText := '$'+PicPropToStr(TPicProperty(TComponent(Sender).Tag), True);
  end;

  procedure TfrExprPicFilter.FocusEditor;
  begin
    if eExpression.CanFocus then eExpression.SetFocus;
  end;

  function TfrExprPicFilter.GetCaretPos: TPoint;
  begin
    Result := TPoint(eExpression.CaretXY);
  end;

  function TfrExprPicFilter.GetExpression: WideString;
  begin
    Result := eExpression.Text;
  end;

  function TfrExprPicFilter.GetRegistryKey: WideString;
  begin
    Result := SRegRoot+'\'+SRegDialogsRoot+'\'+SRegPicFilterExprEditor_Root;
  end;

  procedure TfrExprPicFilter.LoadSettings;
  var rif: TRegIniFile;
  begin
    rif := TRegIniFile.Create(GetRegistryKey);
    try
      mruOpen.LoadFromRegIni(rif, SRegPFilterExprEditor_OpenMRU);
    finally
      rif.Free;
    end;
    FSettingsLoaded := True;
  end;

  procedure TfrExprPicFilter.mruOpenClick(Sender: TObject; const Filename: String);
  begin
    ExpressionLoad(Filename);
  end;

  procedure TfrExprPicFilter.SaveSettings;
  var rif: TRegIniFile;
  begin
    if not FSettingsLoaded then Exit;
    rif := TRegIniFile.Create(GetRegistryKey);
    try
      mruOpen.SaveToRegIni(rif, SRegPFilterExprEditor_OpenMRU);
    finally
      rif.Free;
    end;
  end;

  procedure TfrExprPicFilter.SetCaretPos(const Value: TPoint);
  begin
    eExpression.CaretXY := TBufferCoord(Value);
  end;

  procedure TfrExprPicFilter.SetExpression(const Value: WideString);
  begin
    eExpression.Text := Value;
  end;

end.

