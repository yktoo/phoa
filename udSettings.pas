unit udSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, GR32, Controls, Forms, Dialogs, ConsVars,
  phDlg, VirtualTrees, TB2Dock, TB2Toolbar, TBX, ExtCtrls, DTLangTools,
  StdCtrls;

type
  TdSettings = class(TPhoaDialog)
    pMain: TPanel;
    dkNav: TTBXDock;
    tbNav: TTBXToolbar;
    tvMain: TVirtualStringTree;
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvMainAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
     // Локальная копия настроек
    FLocalRootSetting: TPhoaSetting;
     // Узел, соответствующего текущей нажатой NavBar-кнопке
    FCurSetting: TPhoaSetting;
     // Индекс кнопки навигатора, которую следует выбрать по умочанию
    FDefNavBtnIndex: Integer;
     // Создаёт кнопки навигации (уровень 0 из ConsVars.aPhoaSettings[])
    procedure CreateNavBar;
     // Загружает в tvMain дерево настроек, относящихся к детям узла Item
    procedure LoadSettingTree(Item: TPhoaSetting);
     // Событие нажатия NavBar-кнопки
    procedure NavBarButtonClick(Sender: TObject);
     // Возвращает текст пункта
    function  GetSettingText(Item: TPhoaSetting): String;
  protected
    procedure InitializeDialog; override;
    procedure FinalizeDialog; override;
    procedure ButtonClick_OK; override;
  end;

   // Отображает диалог настроек. iBtnIndex - индекс кнопки навигатора, которую следует выбрать по умочанию
  function EditSettings(iBtnIndex: Integer): Boolean;

implementation
{$R *.dfm}
uses phUtils, Main, udStringBox;

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
    PS: TPhoaSetting;
  begin
    for i := 0 to FLocalRootSetting.ChildCount-1 do begin
      PS := FLocalRootSetting.Children[i];
      tbi := TTBXItem.Create(Self);
      tbi.Caption     := GetSettingText(PS);
      tbi.HelpContext := PS.HelpContext;
      tbi.ImageIndex  := PS.ImageIndex;
      tbi.Tag         := Integer(PS);
      tbi.OnClick     := NavBarButtonClick;
      if i<9 then tbi.ShortCut := 16433+i; // Ctrl+1..9 keys
      tbNav.Items.Add(tbi);
    end;
  end;

  procedure TdSettings.FinalizeDialog;
  begin
    FLocalRootSetting.Free;
    inherited FinalizeDialog;
  end;

  function TdSettings.GetSettingText(Item: TPhoaSetting): String;
  begin
    Result := Item.Name;
    if Result<>'' then
      case Result[1] of
         // Если наименование начинается на '@' - это константа из TdSettings.dtlsMain
        '@': Result := dtlsMain.Consts[Copy(Result, 2, MaxInt)];
         // Если наименование начинается на '#' - это константа из fMain.dtlsMain
        '#': Result := ConstVal(Copy(Result, 2, MaxInt));
      end;
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
    LoadSettingTree(FLocalRootSetting[FDefNavBtnIndex]);
  end;

  procedure TdSettings.LoadSettingTree(Item: TPhoaSetting);
  var i: Integer;
  begin
    FCurSetting := Item;
     // Нажимаем соотв. кнопку
    for i := 0 to tbNav.Items.Count-1 do
      with tbNav.Items[i] do Checked := Tag=Integer(Item);
     // Загружаем дерево
    with tvMain do begin
      BeginUpdate;
      try
         // Удаляем все узлы
        Clear;
         // Устанавливаем количество записей в корневом каталоге
        RootNodeCount := Item.ChildCount;
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
    HelpContext := Item.HelpContext;
  end;

  procedure TdSettings.NavBarButtonClick(Sender: TObject);
  begin
    LoadSettingTree(TPhoaSetting(TComponent(Sender).Tag));
  end;

  procedure TdSettings.tvMainAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  var Item: TPhoaSetting;

    procedure DoDraw(cBkColor: TColor; const sText: String);
    var r: TRect;
    begin
       // Настраиваем Canvas
      with TargetCanvas do begin
        Pen.Color   := iif(vsSelected in Node.States, clHighlight, clGrayText);
        Font.Color  := Pen.Color;
        Brush.Color := cBkColor;
      end;
       // Получаем прямоугольник текста в TargetCanvas
      r := Sender.GetDisplayRect(Node, -1, True);
      r  := Rect(r.Right+4, CellRect.Top, r.Right+4+(TargetCanvas.TextWidth(sText)+6), CellRect.Bottom);
      TargetCanvas.RoundRect(r.Left, r.Top, r.Right, r.Bottom, 7, 7);
      DrawText(TargetCanvas.Handle, PChar(sText), -1, r, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
    end;

  begin
     // Получаем пункт
    Item := PPhoaSetting(Sender.GetNodeData(Node))^;
     // "Дорисовываем"
    case Item.Datatype of
      sdtColor: DoDraw(Item.ValueInt, '    ');
      sdtInt:   DoDraw($f7f7f7, IntToStr(Item.ValueInt));
      sdtFont:  DoDraw($f7f7f7, GetFirstWord(Item.ValueStr, '/'));
    end;
  end;

  procedure TdSettings.tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var
    Item: TPhoaSetting;
    s: String;
  begin
    Item := PPhoaSetting(Sender.GetNodeData(Node))^;
    case Item.Datatype of
      sdtBool: begin
        Item.ValueBool := not Item.ValueBool;
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
        PPhoaSetting(Sender.GetNodeData(Node.Parent))^.ValueInt := Item.ValueInt;
        Modified := True;
      end;
      sdtColor:
        with TColorDialog.Create(Self) do
          try
            Color := Item.ValueInt;
            if Execute then begin
              Item.ValueInt := Color;
              Modified := True;
            end;
          finally
            Free;
          end;
      sdtInt: begin
        s := IntToStr(Item.ValueInt);
        if StringBox(s, '', GetSettingText(Item), HelpContext) and (s<>'') then begin
          Item.ValueInt := StrToInt(s);
          Modified := True;
        end;
      end;
      sdtFont:
        with TFontDialog.Create(Self) do
          try
            FontFromStr(Font, Item.ValueStr);
            if Execute then begin
              Item.ValueStr := FontToStr(Font);
              Modified := True;
            end;
          finally
            Free;
          end;
    end;
  end;

  procedure TdSettings.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Kind in [ikNormal, ikSelected] then ImageIndex := PPhoaSetting(Sender.GetNodeData(Node))^.ImageIndex;
  end;

  procedure TdSettings.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    CellText := AnsiToUnicodeCP(GetSettingText(PPhoaSetting(Sender.GetNodeData(Node))^), cMainCodePage);
  end;

  procedure TdSettings.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var
    ParentItem, Item: TPhoaSetting;
    bChecked: Boolean;
  begin
     // Ищем родительский узел
    if ParentNode=nil then ParentItem := FCurSetting else ParentItem := PPhoaSetting(Sender.GetNodeData(ParentNode))^;
     // Сохраняем пункт в Node.Data
    Item := ParentItem[Node.Index];
    PPhoaSetting(Sender.GetNodeData(Node))^ := Item;
     // Настраиваем CheckType и CheckState
    bChecked := False;
    case Item.Datatype of
       // Флажок
      sdtBool:  begin
        Node.CheckType := ctCheckBox;
        bChecked := Item.ValueBool;
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
        bChecked := ParentItem.ValueInt=Item.ValueInt;
      end;
       // Настраиваемое "извне" значение
      sdtColor, sdtInt, sdtFont: Node.CheckType := ctButton;
    end;
    Node.CheckState := aCheckStates[bChecked];
     // Инициализируем к-во детей
    tvMain.ChildCount[Node] := Item.ChildCount;
     // Разворачиваем все узлы
    Include(InitialStates, ivsExpanded);
  end;

  procedure TdSettings.tvMainPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
     // Выделяем жирным узлы, имеющие детей
    if Sender.ChildCount[Node]>0 then TargetCanvas.Font.Style := [fsBold];
  end;

end.
