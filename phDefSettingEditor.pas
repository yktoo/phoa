//**********************************************************************************************************************
//  $Id: phDefSettingEditor.pas,v 1.1 2004-04-19 18:22:34 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phDefSettingEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, VirtualTrees, ConsVars, phSettings;

type
  TDefSettingEditor = class(TVirtualStringTree, IPhoaSettingEditor)
  private
     // Контрол для редактирования значения текущего узла
    FEditorControl: TWinControl;
     // Флаг встраивания контрола-редактора. Используется для предотвращения вызовов EmbeddedControlChange во время его
     //   начальной настройки, а также для игнорирования потери фокуса деревом
    FEmbeddingControl: Boolean;
     // Prop storage
    FOnSettingChange: TNotifyEvent;
    FOnDecodeText: TPhoaSettingDecodeTextEvent;
    FRootSetting: TPhoaPageSetting;
     // Загружает в tvMain дерево настроек, относящихся к детям узла FRootSetting
    procedure LoadTree;
     // Вызывает OnSettingChange
    procedure DoSettingChange;
     // Встраивает соответствующий контрол для текущего узла, если он нужен. Если нет (в том числе при
     //   tvMain.FocusedNode=nil), удаляет текущий контрол
    procedure EmbedControl;
     // "Отложенная" процедура EmbedControl
    procedure EmbedControlNotify(Sender: TObject);
     // События встроенного контрола
    procedure EmbeddedControlChange(Sender: TObject);
    procedure EmbeddedControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EmbeddedFontButtonClick(Sender: TObject);
     // IPhoaSettingEditor
    procedure InitAndEmbed(ParentCtl: TWinControl; AOnSettingChange: TNotifyEvent; AOnDecodeText: TPhoaSettingDecodeTextEvent);
    function  GetRootSetting: TPhoaPageSetting;
    procedure SetRootSetting(Value: TPhoaPageSetting);
     // Message handlers
    procedure WMEmbedControl(var Msg: TMessage); message WM_EMBEDCONTROL;
  protected
    function  ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean; override;
    procedure DoAfterCellPaint(TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure DoInitNode(ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
uses TypInfo, Forms, Dialogs, phUtils;

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
   // TDefSettingEditor
   //-------------------------------------------------------------------------------------------------------------------

  function TDefSettingEditor.ColumnIsEmpty(Node: PVirtualNode; Column: TColumnIndex): Boolean;
  begin
     // Ячейка значения не пустая, если настройка редактируется редактором
    if Column=1 then
      Result := not (PPhoaSetting(GetNodeData(Node))^.Datatype in EditableSettingDatatypes)
    else
      Result := inherited ColumnIsEmpty(Node, Column);
  end;

  constructor TDefSettingEditor.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    Align := alClient;
    with Header do begin
      Columns.Add.Width := 300;
      Columns.Add;
      AutoSizeIndex := 1;
      Options := Options+[hoAutoResize];
    end;
    with TreeOptions do begin
      AutoOptions      := [toAutoDropExpand, toAutoScroll, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes];
      MiscOptions      := [toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning];
      PaintOptions     := [toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages];
      SelectionOptions := [toFullRowSelect];
    end;
    HintMode := hmTooltip;
     //TODO - заменить на DoEnter/DoExit !!!
    OnEnter := EmbedControlNotify;
    OnExit  := EmbedControlNotify;
     // Применяем опции
    ApplyTreeSettings(Self);
     // Каждый узел хранит указатель на TPhoaSetting
    NodeDataSize := SizeOf(Pointer);
  end;

  destructor TDefSettingEditor.Destroy;
  begin
    FEditorControl.Free;
    inherited Destroy;
  end;

  procedure TDefSettingEditor.DoAfterCellPaint(TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  var Setting: TPhoaSetting;
  begin
    if Column=1 then begin
       // Получаем пункт
      Setting := PPhoaSetting(GetNodeData(Node))^;
       // Если это "Цвет", рисуем квадратик соответствующего цвета
      if Setting.Datatype=sdtColor then
        with TargetCanvas do begin
          Pen.Color   := clBlack;
          Brush.Color := Setting.ValueInt;
          Rectangle(CellRect.Left+2, (CellRect.Top+CellRect.Bottom-14) div 2, CellRect.Left+16, (CellRect.Top+CellRect.Bottom+14) div 2);
        end;
    end;
  end;

  procedure TDefSettingEditor.DoChecked(Node: PVirtualNode);
  var Setting: TPhoaSetting;
  begin
    Setting := PPhoaSetting(GetNodeData(Node))^;
    case Setting.Datatype of
      sdtBool: begin
        Setting.ValueBool := not Setting.ValueBool;
        DoSettingChange;
      end;
      sdtParMsk: begin
        with PPhoaSetting(GetNodeData(Node.Parent))^ do ValueInt := ValueInt xor (Integer(1) shl Node.Index);
        DoSettingChange;
      end;
      sdtMutex: begin
        PPhoaSetting(GetNodeData(Node.Parent))^.ValueInt := Node.Index;
        DoSettingChange;
      end;
      sdtMutexInt: begin
        PPhoaSetting(GetNodeData(Node.Parent))^.ValueInt := Setting.ValueInt;
        DoSettingChange;
      end;
    end;
  end;

  procedure TDefSettingEditor.DoFocusChange(Node: PVirtualNode; Column: TColumnIndex);
  begin
    PostMessage(Handle, WM_EMBEDCONTROL, 0, 0);
  end;

  procedure TDefSettingEditor.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    s: String;
    Setting: TPhoaSetting;
  begin
    s := '';
    Setting := PPhoaSetting(GetNodeData(Node))^;
    case Column of
      0: FOnDecodeText(Setting.Name, s);
      1:
        case Setting.Datatype of
          sdtComboIdx,
            sdtComboObj: FOnDecodeText(Setting.VariantText, s);
          sdtInt:        s := IntToStr(Setting.ValueInt);
          sdtFont:       s := GetFirstWord(Setting.ValueStr, '/');
        end;
    end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TDefSettingEditor.DoInitNode(ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var
    ParentItem, Setting: TPhoaSetting;
    bChecked: Boolean;
  begin
     // Ищем родительский узел
    if ParentNode=nil then ParentItem := FRootSetting else ParentItem := PPhoaSetting(GetNodeData(ParentNode))^;
     // Сохраняем пункт в Node.Data
    Setting := ParentItem[Node.Index];
    PPhoaSetting(GetNodeData(Node))^ := Setting;
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
    ChildCount[Node] := Setting.ChildCount;
     // Разворачиваем все узлы
    Include(InitialStates, ivsExpanded);
  end;

  procedure TDefSettingEditor.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
  begin
     // Выделяем жирным узлы, имеющие детей
    if ChildCount[Node]>0 then Canvas.Font.Style := [fsBold];
  end;

  procedure TDefSettingEditor.DoSettingChange;
  begin
    if Assigned(FOnSettingChange) then FOnSettingChange(Self);
  end;

  procedure TDefSettingEditor.EmbedControl;
  var
    ActCtl: TWinControl;
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
        FEditorControl.Parent := Self;
      end;
       // Настраиваем размер
      FEditorControl.BoundsRect := GetDisplayRect(CurNode, 1, False);
       // Tag должен указывать на соответствующий узел
      FEditorControl.Tag := Integer(CurNode);
       // Привязываем события
      BindNotifyEvent('OnEnter',   EmbedControlNotify);
      BindNotifyEvent('OnExit',    EmbedControlNotify);
      BindKeyEvent   ('OnKeyDown', EmbeddedControlKeyDown);
    end;

     // Создаёт и возвращает TComboBox в качестве редактора значения настройки
    procedure NewComboBox;
    var
      i: Integer;
      s: String;
    begin
      NewControl(TComboBox);
      with TComboBox(FEditorControl) do begin
         // Копируем список вариантов
        Items.Clear;
        for i := 0 to Setting.Variants.Count-1 do begin
          FOnDecodeText(Setting.Variants[i], s);
          Items.AddObject(s, Setting.Variants.Objects[i]);
        end;
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
    ActCtl := GetParentForm(Self).ActiveControl;
    bBlurred := (ActCtl<>Self) and (ActCtl<>FEditorControl);
     // Проверяем необходимость [пере]создания контрола
    CurNode := FocusedNode;
    if (FEditorControl=nil) or bBlurred or (CurNode<>PVirtualNode(FEditorControl.Tag)) then begin
      FEmbeddingControl := True;
      try
         // Если нужно уничтожить контрол
        if (CurNode=nil) or bBlurred then
          FreeAndNil(FEditorControl)
         // Иначе - создаём
        else begin
           // Получаем пункт настроек из данных узла
          Setting := PPhoaSetting(GetNodeData(CurNode))^;
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

  procedure TDefSettingEditor.EmbedControlNotify(Sender: TObject);
  begin
    if not FEmbeddingControl then PostMessage(Handle, WM_EMBEDCONTROL, 0, 0);
  end;

  procedure TDefSettingEditor.EmbeddedControlChange(Sender: TObject);
  var
    Node: PVirtualNode;
    Setting: TPhoaSetting;
  begin
    if FEmbeddingControl then Exit;
     // Tag контрола - это ссылка на его узел
    Node := PVirtualNode(FEditorControl.Tag);
     // Получаем пункт настроек из данных узла
    Setting := PPhoaSetting(GetNodeData(Node))^;
    case Setting.Datatype of
      sdtComboIdx,
        sdtComboObj: Setting.VariantIndex := (FEditorControl as TComboBox).ItemIndex;
      sdtColor:      Setting.ValueInt     := (FEditorControl as TColorBox).Selected;
      sdtInt:        Setting.ValueInt     := StrToIntDef((FEditorControl as TEdit).Text, Setting.ValueInt);
    end;
    DoSettingChange;
  end;

  procedure TDefSettingEditor.EmbeddedControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    if (Shift*[ssShift, ssCtrl, ssAlt]=[]) and (not (Sender is TCustomComboBox) or not TCustomComboBox(Sender).DroppedDown) then
      case Key of
        VK_UP, VK_DOWN: begin
          Perform(WM_KEYDOWN, Key, 0);
          SetFocus;
          Key := 0;
        end;
      end;
  end;

  procedure TDefSettingEditor.EmbeddedFontButtonClick(Sender: TObject);
  var Setting: TPhoaSetting;
  begin
     // Tag контрола - это ссылка на его узел. Получаем пункт настроек из данных узла
    Setting := PPhoaSetting(GetNodeData(PVirtualNode(FEditorControl.Tag)))^;
    with TFontDialog.Create(Self) do
      try
        FontFromStr(Font, Setting.ValueStr);
        if Execute then begin
          Setting.ValueStr := FontToStr(Font);
          (FEditorControl as TSettingButton).Caption := Font.Name;
          DoSettingChange;
        end;
      finally
        Free;
      end;
  end;

  function TDefSettingEditor.GetRootSetting: TPhoaPageSetting;
  begin
    Result := FRootSetting;
  end;

  procedure TDefSettingEditor.InitAndEmbed(ParentCtl: TWinControl; AOnSettingChange: TNotifyEvent; AOnDecodeText: TPhoaSettingDecodeTextEvent);
  begin
    Parent           := ParentCtl;
    FOnSettingChange := AOnSettingChange;
    FOnDecodeText    := AOnDecodeText;
  end;

  procedure TDefSettingEditor.LoadTree;
  begin
     // Загружаем дерево
    BeginUpdate;
    try
       // Удаляем все узлы
      Clear;
       // Устанавливаем количество записей в корневом каталоге
      RootNodeCount := FRootSetting.ChildCount;
       // Инициализируем все узлы
      ReinitChildren(nil, True);
       // Выделяем первый узел
      FocusedNode := GetFirst;
      Selected[FocusedNode] := True;
    finally
      EndUpdate;
    end;
  end;

  procedure TDefSettingEditor.SetRootSetting(Value: TPhoaPageSetting);
  begin
    if FRootSetting<>Value then begin
      FRootSetting := Value;
      LoadTree;
    end;
  end;

  procedure TDefSettingEditor.WMEmbedControl(var Msg: TMessage);
  begin
    EmbedControl;
  end;

end.
