//**********************************************************************************************************************
//  $Id: udSettings.pas,v 1.4 2004-04-18 12:09:55 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit udSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, GR32, Controls, Forms, Dialogs, ConsVars,
  phDlg, VirtualTrees, TB2Dock, TB2Toolbar, TBX, ExtCtrls, DTLangTools,
  StdCtrls;

const
  WM_EMBEDCONTROL = WM_USER+1;

type
  TdSettings = class(TPhoaDialog)
    pMain: TPanel;
    dkNav: TTBXDock;
    tbNav: TTBXToolbar;
    tvMain: TVirtualStringTree;
    procedure EmbedControlNotify(Sender: TObject);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvMainAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure tvMainGetCellIsEmpty(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var IsEmpty: Boolean);
  private
     // Локальная копия настроек
    FLocalRootSetting: TPhoaSetting;
     // Узел, соответствующего текущей нажатой NavBar-кнопке
    FCurSetting: TPhoaPageSetting;
     // Индекс кнопки навигатора, которую следует выбрать по умочанию
    FDefNavBtnIndex: Integer;
     // Контрол для редактирования значения текущего узла
    FEditorControl: TWinControl;
     // Флаг встраивания контрола-редактора. Используется для предотвращения вызовов EmbeddedControlChange во время его
     //   начальной настройки, а также для игнорирования потери фокуса деревом 
    FEmbeddingControl: Boolean;
     // Создаёт кнопкtи навигации (уровень 0 из ConsVars.aPhoaSettings[])
    procedure CreateNavBar;
     // Загружает в tvMain дерево настроек, относящихся к детям узла Setting
    procedure LoadSettingTree(PageSetting: TPhoaPageSetting);
     // Событие нажатия NavBar-кнопки
    procedure NavBarButtonClick(Sender: TObject);
     // Декодирует строку текста сообразно правилам (если требуется, то с использованием констант)
    function  DecodeSettingText(const sText: String): String;
     // Встраивает соответствующий контрол для текущего узла, если он нужен. Если нет (в том числе при
     //   tvMain.FocusedNode=nil), удаляет текущий контрол
    procedure EmbedControl;
     // События встроенного контрола
    procedure EmbeddedControlChange(Sender: TObject);
    procedure EmbeddedControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EmbeddedFontButtonClick(Sender: TObject);
     // Message handlers
    procedure WMEmbedControl(var Msg: TMessage); message WM_EMBEDCONTROL;
  protected
    procedure InitializeDialog; override;
    procedure FinalizeDialog; override;
    procedure ButtonClick_OK; override;
  end;

   // Отображает диалог настроек. iBtnIndex - индекс кнопки навигатора, которую следует выбрать по умочанию
  function EditSettings(iBtnIndex: Integer): Boolean;

const
  ISetting_ValueGap        = 4;   // Отступ между текстом и значением пункта настройки, в пикселах

implementation
{$R *.dfm}
uses phUtils, Main, TypInfo;

  function EditSettings(iBtnIndex: Integer): Boolean;
  begin
    with TdSettings.Create(Application) do
      try
        FDefNavBtnIndex := iBtnIndex;
        Result := Execute;
      finally
        Free;
      end;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // TSettingButton - потомок TButton, перехватывающий нажатия стрелок
   //-------------------------------------------------------------------------------------------------------------------

type
  TSettingButton = class(TButton)
  private
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
  end;

  procedure TSettingButton.WMGetDlgCode(var Msg: TWMGetDlgCode);
  begin
    Msg.Result := DLGC_WANTARROWS;
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
  begin
    for i := 0 to FLocalRootSetting.ChildCount-1 do begin
      PPS := FLocalRootSetting.Children[i] as TPhoaPageSetting;
      tbi := TTBXItem.Create(Self);
      tbi.Caption     := DecodeSettingText(PPS.Name);
      tbi.HelpContext := PPS.HelpContext;
      tbi.ImageIndex  := PPS.ImageIndex;
      tbi.Tag         := Integer(PPS);
      tbi.OnClick     := NavBarButtonClick;
      if i<9 then tbi.ShortCut := 16433+i; // Ctrl+1..9 keys
      tbNav.Items.Add(tbi);
    end;
  end;

  function TdSettings.DecodeSettingText(const sText: String): String;
  begin
    Result := sText;
    if Result<>'' then
      case Result[1] of
         // Если наименование начинается на '@' - это константа из TdSettings.dtlsMain
        '@': Result := dtlsMain.Consts[Copy(Result, 2, MaxInt)];
         // Если наименование начинается на '#' - это константа из fMain.dtlsMain
        '#': Result := ConstVal(Copy(Result, 2, MaxInt));
      end;
  end;

  procedure TdSettings.EmbedControl;
  var
    Setting: TPhoaSetting;
    bBlurred: Boolean;
    CurNode: PVirtualNode;

     // Создаёт и присваивает в FEditorControl Control заданного класса в качестве редактора значения настройки
    procedure NewControl(CtlClass: TWinControlClass);

      procedure BindKeyEvent(const sPropName: String; Event: TKeyEvent);
      begin
        SetMethodProp(FEditorControl, sPropName, TMethod(Event));
      end;

      procedure BindNotifyEvent(const sPropName: String; Event: TNotifyEvent);
      begin
        SetMethodProp(FEditorControl, sPropName, TMethod(Event));
      end;

    begin
       // Создаём контрол, если его ещё нет, или он другого класса
      if (FEditorControl=nil) or (FEditorControl.ClassType<>CtlClass) then begin
        FreeAndNil(FEditorControl);
        FEditorControl := CtlClass.Create(Self);
        FEditorControl.Parent := tvMain;
      end;
       // Настраиваем размер
      FEditorControl.BoundsRect := tvMain.GetDisplayRect(CurNode, 1, False);
       // Tag должен указывать на соответствующий узел
      FEditorControl.Tag := Integer(CurNode);
       // Привязываем события
      BindNotifyEvent('OnEnter',   EmbedControlNotify);
      BindNotifyEvent('OnExit',    EmbedControlNotify);
      BindKeyEvent   ('OnKeyDown', EmbeddedControlKeyDown);
    end;

     // Создаёт и возвращает TComboBox в качестве редактора значения настройки
    procedure NewComboBox;
    var i: Integer;
    begin
      NewControl(TComboBox);
      with TComboBox(FEditorControl) do begin
         // Копируем список вариантов
        Items.Clear;
        for i := 0 to Setting.Variants.Count-1 do Items.AddObject(DecodeSettingText(Setting.Variants[i]), Setting.Variants.Objects[i]);
         // Прочие опции
        DropDownCount := 16;
        Style         := csDropDownList;
        ItemIndex     := Setting.VariantIndex;
        OnChange      := EmbeddedControlChange;
      end;
    end;

     // Создаёт и возвращает TColorBox в качестве редактора значения настройки
    procedure NewColorBox;
    begin
      NewControl(TColorBox);
      with TColorBox(FEditorControl) do begin
        DropDownCount := 16;
        Style         := [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames];
        Selected      := Setting.ValueInt;
        OnChange      := EmbeddedControlChange;
      end;
    end;

     // Создаёт и возвращает TEdit в качестве редактора значения настройки
    procedure NewEdit(iMaxLen: Integer);
    begin
      NewControl(TEdit);
      with TEdit(FEditorControl) do begin
        MaxLength := iMaxLen;
        Text      := IntToStr(Setting.ValueInt);
        OnChange  := EmbeddedControlChange;
      end;
    end;

     // Создаёт и возвращает TSettingButton в качестве редактора шрифта
    procedure NewFontButton;
    begin
      NewControl(TSettingButton);
      with TSettingButton(FEditorControl) do begin
        Caption   := GetFirstWord(Setting.ValueStr, '/');
        OnClick   := EmbeddedFontButtonClick;
      end;
    end;

  begin
     // Определяем флаг дефокусировки дерева/редактора
    bBlurred := (ActiveControl<>tvMain) and (ActiveControl<>FEditorControl);
     // Проверяем необходимость [пере]создания контрола
    CurNode := tvMain.FocusedNode;
    if (FEditorControl=nil) or bBlurred or (CurNode<>PVirtualNode(FEditorControl.Tag)) then begin
      FEmbeddingControl := True;
      try
         // Если нужно уничтожить контрол
        if (CurNode=nil) or bBlurred then
          FreeAndNil(FEditorControl)
         // Иначе - создаём
        else begin
           // Получаем пункт настроек из данных узла
          Setting := PPhoaSetting(tvMain.GetNodeData(CurNode))^;
           // Создаём или уничтожаем контрол
          case Setting.Datatype of
            sdtComboIdx,
              sdtComboObj: NewComboBox;
            sdtColor:      NewColorBox;
            sdtInt:        NewEdit(Length(IntToStr(Setting.MaxValue)));
            sdtFont:       NewFontButton;
            else           FreeAndNil(FEditorControl);
          end;
        end;
      finally
        FEmbeddingControl := False;
      end;
    end;
     // Фокусируем контрол
    if (FEditorControl<>nil) and not FEditorControl.Focused then FEditorControl.SetFocus;
  end;

  procedure TdSettings.EmbedControlNotify(Sender: TObject);
  begin
    if not FEmbeddingControl then PostMessage(Handle, WM_EMBEDCONTROL, 0, 0);
  end;

  procedure TdSettings.EmbeddedControlChange(Sender: TObject);
  var
    Node: PVirtualNode;
    Setting: TPhoaSetting;
  begin
    if FEmbeddingControl then Exit;
     // Tag контрола - это ссылка на его узел
    Node := PVirtualNode(FEditorControl.Tag);
     // Получаем пункт настроек из данных узла
    Setting := PPhoaSetting(tvMain.GetNodeData(Node))^;
    case Setting.Datatype of
      sdtComboIdx,
        sdtComboObj: Setting.VariantIndex := (FEditorControl as TComboBox).ItemIndex;
      sdtColor:      Setting.ValueInt     := (FEditorControl as TColorBox).Selected;
      sdtInt:        Setting.ValueInt     := StrToIntDef((FEditorControl as TEdit).Text, Setting.ValueInt);
    end;
    Modified := True;
  end;

  procedure TdSettings.EmbeddedControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if (Shift*[ssShift, ssCtrl, ssAlt]=[]) and (not (Sender is TCustomComboBox) or not TCustomComboBox(Sender).DroppedDown) then
      case Key of
        VK_UP, VK_DOWN: begin
          tvMain.Perform(WM_KEYDOWN, Key, 0);
          tvMain.SetFocus;
          Key := 0;
        end;
      end;
  end;

  procedure TdSettings.EmbeddedFontButtonClick(Sender: TObject);
  var Setting: TPhoaSetting;
  begin
     // Tag контрола - это ссылка на его узел. Получаем пункт настроек из данных узла
    Setting := PPhoaSetting(tvMain.GetNodeData(PVirtualNode(FEditorControl.Tag)))^;
    with TFontDialog.Create(Self) do
      try
        FontFromStr(Font, Setting.ValueStr);
        if Execute then begin
          Setting.ValueStr := FontToStr(Font);
          (FEditorControl as TSettingButton).Caption := Font.Name;
          Modified := True;
        end;
      finally
        Free;
      end;
  end;

  procedure TdSettings.FinalizeDialog;
  begin
    FEditorControl.Free;
    FLocalRootSetting.Free;
    inherited FinalizeDialog;
  end;

  procedure TdSettings.InitializeDialog;
  begin
    inherited InitializeDialog;
    ApplyTreeSettings(tvMain);
     // Каждый узел tvMain будет хранить указатель на TPhoaSetting
    tvMain.NodeDataSize := SizeOf(Pointer);
     // Копируем настройки
    FLocalRootSetting := TPhoaSetting.Create(nil, sdtStatic, 0, '');
    FLocalRootSetting.Assign(RootSetting);
     // Создаём кнопки навигации
    CreateNavBar;
     // Выбираем стартовую кнопку
    LoadSettingTree(FLocalRootSetting[FDefNavBtnIndex] as TPhoaPageSetting);
  end;

  procedure TdSettings.LoadSettingTree(PageSetting: TPhoaPageSetting);
  var i: Integer;
  begin
    FCurSetting := PageSetting;
     // Нажимаем соотв. кнопку
    for i := 0 to tbNav.Items.Count-1 do
      with tbNav.Items[i] do Checked := Tag=Integer(PageSetting);
     // Загружаем дерево
    with tvMain do begin
      BeginUpdate;
      try
         // Удаляем все узлы
        Clear;
         // Устанавливаем количество записей в корневом каталоге
        RootNodeCount := PageSetting.ChildCount;
         // Инициализируем все узлы
        ReinitChildren(nil, True);
         // Выделяем первый узел
        FocusedNode := GetFirst;
        Selected[FocusedNode] := True;
      finally
        EndUpdate;
      end;
      if Self.Visible then SetFocus;
    end;
     // Настраиваем HelpContext
    HelpContext := PageSetting.HelpContext;
  end;

  procedure TdSettings.NavBarButtonClick(Sender: TObject);
  begin
    LoadSettingTree(TPhoaPageSetting(TComponent(Sender).Tag));
  end;

  procedure TdSettings.tvMainAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  var Setting: TPhoaSetting;
  begin
    if Column=1 then begin
       // Получаем пункт
      Setting := PPhoaSetting(Sender.GetNodeData(Node))^;
       // Если это "Цвет", рисуем квадратик соответствующего цвета
      if Setting.Datatype=sdtColor then
        with TargetCanvas do begin
          Pen.Color   := clBlack;
          Brush.Color := Setting.ValueInt;
          Rectangle(CellRect.Left+2, (CellRect.Top+CellRect.Bottom-14) div 2, CellRect.Left+16, (CellRect.Top+CellRect.Bottom+14) div 2);
        end;
    end;
  end;

  procedure TdSettings.tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var Setting: TPhoaSetting;
  begin
    Setting := PPhoaSetting(Sender.GetNodeData(Node))^;
    case Setting.Datatype of
      sdtBool: begin
        Setting.ValueBool := not Setting.ValueBool;
        Modified := True;
      end;
      sdtParMsk: begin
        with PPhoaSetting(Sender.GetNodeData(Node.Parent))^ do ValueInt := ValueInt xor (Integer(1) shl Node.Index);
        Modified := True;
      end;
      sdtMutex: begin
        PPhoaSetting(Sender.GetNodeData(Node.Parent))^.ValueInt := Node.Index;
        Modified := True;
      end;
      sdtMutexInt: begin
        PPhoaSetting(Sender.GetNodeData(Node.Parent))^.ValueInt := Setting.ValueInt;
        Modified := True;
      end;
    end;
  end;

  procedure TdSettings.tvMainFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  begin
    PostMessage(Handle, WM_EMBEDCONTROL, 0, 0);
  end;

  procedure TdSettings.tvMainGetCellIsEmpty(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var IsEmpty: Boolean);
  begin
     // Ячейка значения не пустая, если настройка редактируется редактором
    if Column=1 then IsEmpty := not (PPhoaSetting(Sender.GetNodeData(Node))^.Datatype in EditableSettingDatatypes);
  end;

  procedure TdSettings.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    s: String;
    Setting: TPhoaSetting;
  begin
    s := '';
    Setting := PPhoaSetting(Sender.GetNodeData(Node))^;
    case Column of
      0: s := DecodeSettingText(Setting.Name);
      1:
        case Setting.Datatype of
          sdtComboIdx,
            sdtComboObj: s := DecodeSettingText(Setting.VariantText);
          sdtInt:        s := IntToStr(Setting.ValueInt);
          sdtFont:       s := GetFirstWord(Setting.ValueStr, '/');
        end;
    end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TdSettings.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var
    ParentItem, Setting: TPhoaSetting;
    bChecked: Boolean;
  begin
     // Ищем родительский узел
    if ParentNode=nil then ParentItem := FCurSetting else ParentItem := PPhoaSetting(Sender.GetNodeData(ParentNode))^;
     // Сохраняем пункт в Node.Data
    Setting := ParentItem[Node.Index];
    PPhoaSetting(Sender.GetNodeData(Node))^ := Setting;
     // Настраиваем CheckType и CheckState
    bChecked := False;
    case Setting.Datatype of
       // Флажок
      sdtBool:  begin
        Node.CheckType := ctCheckBox;
        bChecked := Setting.ValueBool;
      end;
       // Бит в маске родителя
      sdtParMsk: begin
        Node.CheckType := ctCheckBox;
        bChecked := ParentItem.ValueInt and (Integer(1) shl Node.Index)<>0;
      end;
       // RadioButton, где (значение родителя)=(индекс ребёнка)
      sdtMutex: begin
        Node.CheckType := ctRadioButton;
        bChecked := ParentItem.ValueInt=Integer(Node.Index);
      end;
       // RadioButton, где (значение родителя)=(значение выбранного ребёнка)
      sdtMutexInt: begin
        Node.CheckType := ctRadioButton;
        bChecked := ParentItem.ValueInt=Setting.ValueInt;
      end;
    end;
    Node.CheckState := aCheckStates[bChecked];
     // Инициализируем к-во детей
    tvMain.ChildCount[Node] := Setting.ChildCount;
     // Разворачиваем все узлы
    Include(InitialStates, ivsExpanded);
  end;

  procedure TdSettings.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
     // Выделяем жирным узлы, имеющие детей
    if Sender.ChildCount[Node]>0 then TargetCanvas.Font.Style := [fsBold];
  end;

  procedure TdSettings.WMEmbedControl(var Msg: TMessage);
  begin
    EmbedControl;
  end;

end.
