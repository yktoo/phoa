//**********************************************************************************************************************
//  $Id: ufrSorting.pas,v 1.2 2004-04-15 12:54:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit ufrSorting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, phObj, ActiveX,
  StdCtrls, DTLangTools, TB2Item, TBX, Menus, VirtualTrees;

type
  TfrSorting = class(TFrame)
    lMain: TLabel;
    tvMain: TVirtualStringTree;
    pmMain: TTBXPopupMenu;
    ipmsmProp: TTBXSubmenuItem;
    ipmDelete: TTBXItem;
    ipmSep: TTBXSeparatorItem;
    ipmMoveUp: TTBXItem;
    ipmMoveDown: TTBXItem;
    procedure tvMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvMainDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure tvMainDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure tvMainDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure tvMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ipmDeleteClick(Sender: TObject);
    procedure ipmMoveUpClick(Sender: TObject);
    procedure ipmMoveDownClick(Sender: TObject);
    procedure pmMainPopup(Sender: TObject);
  private
     // Prop storage
    FSortings: TPhoaSortings;
    FOnChange: TNotifyEvent;
     // Настраивает доступность действий
    procedure EnableActions;
     // Переключает направление сортировки у пункта, соответствующего узлу Node
    procedure ToggleOrder(Node: PVirtualNode);
     // Событие клика на пункте свойства изображения (для сортировки)
    procedure SortingPropClick(Sender: TObject);
     // Возвращает True, если узел соответствует реальному пункту в списке сортировок
    function  SortingNode(Node: PVirtualNode): Boolean;
     // Вызывает OnChange
    procedure DoChange;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
     // Устанавливает стандартные пункты сортировки
    procedure Reset;
     // Настраивает tvMain (в ответ на изменение FSortings)
    procedure SyncSortings;
     // Props
     // -- Редактируемые группировки
    property Sortings: TPhoaSortings read FSortings;
     // -- Событие модификации списка
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main;

  constructor TfrSorting.Create(AOwner: TComponent);
  var
    pp: TPicProperty;
    tbi: TTBXCustomItem;
  begin
    inherited Create(AOwner);
    FSortings := TPhoaSortings.Create;
     // Создаём пункты меню "Добавить сортировку"
    for pp := Low(pp) to High(pp) do begin
      tbi := TTBXItem.Create(Self);
      with tbi do begin
        Caption    := PicPropName(pp);
        ImageIndex := iiSorting;
        Tag        := Byte(pp);
        OnClick    := SortingPropClick;
      end;
      ipmsmProp.Add(tbi);
    end;
    SyncSortings;
  end;

  destructor TfrSorting.Destroy;
  begin
    FSortings.Free;
    inherited Destroy;
  end;

  procedure TfrSorting.DoChange;
  begin
    if Assigned(FOnChange) then FOnChange(Self);
  end;

  procedure TfrSorting.EnableActions;
  var
    n: PVirtualNode;
    idx, iCnt: Integer;
  begin
    n := tvMain.FocusedNode;
    if n=nil then idx := -1 else idx := n.Index;
    iCnt := FSortings.Count;
    ipmsmProp.Enabled   := idx>=0;
    ipmDelete.Enabled   := (idx>=0) and (idx<iCnt);
    ipmMoveUp.Enabled   := (idx>0)  and (idx<iCnt);
    ipmMoveDown.Enabled := (idx>=0) and (idx<iCnt-1);
  end;

  procedure TfrSorting.ipmDeleteClick(Sender: TObject);
  begin
    FSortings.Delete(tvMain.FocusedNode.Index);
    SyncSortings;
    DoChange;
  end;

  procedure TfrSorting.ipmMoveDownClick(Sender: TObject);
  var idx: Integer;
  begin
    idx := tvMain.FocusedNode.Index;
    FSortings.Exchange(idx, idx+1);
    with tvMain do MoveTo(FocusedNode, GetNextSibling(FocusedNode), amInsertAfter, False);
    EnableActions;
    DoChange;
  end;

  procedure TfrSorting.ipmMoveUpClick(Sender: TObject);
  var idx: Integer;
  begin
    idx := tvMain.FocusedNode.Index;
    FSortings.Exchange(idx, idx-1);
    with tvMain do MoveTo(FocusedNode, GetPreviousSibling(FocusedNode), amInsertBefore, False);
    EnableActions;
    DoChange;
  end;

  procedure TfrSorting.pmMainPopup(Sender: TObject);
  var i: Integer;
  begin
     // Выключаем сортировки, которые уже есть в списке выбранных
    for i := 0 to ipmsmProp.Count-1 do
      with ipmsmProp.Items[i] do Visible := FSortings.IndexOf(TPicProperty(Tag))<0;
  end;

  procedure TfrSorting.Reset;
  begin
    FSortings.RevertToDefaults;
    SyncSortings;
    DoChange;
  end;

  procedure TfrSorting.SetParent(AParent: TWinControl);
  begin
    inherited SetParent(AParent);
     // Настраиваем tvMain после встраивания
    if AParent<>nil then begin
      tvMain.HandleNeeded;
      tvMain.Header.Columns[0].Text := ConstVal('SPicProperty');
      tvMain.Header.Columns[1].Text := ConstVal('SSort_Direction');
      ApplyTreeSettings(tvMain);
    end;
  end;

  function TfrSorting.SortingNode(Node: PVirtualNode): Boolean;
  begin
    Result := (Node<>nil) and (Integer(Node.Index)<FSortings.Count);
  end;

  procedure TfrSorting.SortingPropClick(Sender: TObject);
  var
    n: PVirtualNode;
    Prop: TPicProperty;
  begin
    n := tvMain.FocusedNode;
    Prop := TPicProperty(TComponent(Sender).Tag);
     // Смена свойства у существующего пункта
    if SortingNode(n) then begin
      FSortings.SetProp(n.Index, Prop);
      tvMain.InvalidateNode(n);
      EnableActions;
     // Добавление нового пункта
    end else begin
      FSortings.Add(Prop, soAsc);
      SyncSortings;
    end;
    DoChange;
  end;

  procedure TfrSorting.SyncSortings;
  begin
    tvMain.RootNodeCount := FSortings.Count+1;
    tvMain.Invalidate;
    EnableActions;
  end;

  procedure TfrSorting.ToggleOrder(Node: PVirtualNode);
  begin
    if SortingNode(Node) then begin
      FSortings.ToggleOrder(Node.Index);
      tvMain.InvalidateNode(Node);
      DoChange;
    end;
  end;

  procedure TfrSorting.tvMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    EnableActions;
  end;

  procedure TfrSorting.tvMainChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var p: TPoint;
  begin
    Sender.Selected[Node] := True;
    Sender.FocusedNode := Node;
    EnableActions;
    with Sender.GetDisplayRect(Node, -1, False) do p := Sender.ClientToScreen(Point(Left, Bottom));
    pmMain.Popup(p.x, p.y);
  end;

  procedure TfrSorting.tvMainDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
  begin
    Allowed := SortingNode(Node);
  end;

  procedure TfrSorting.tvMainDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
  var
    nSrc, nTgt: PVirtualNode;
    idxNew: Integer;
    am: TVTNodeAttachMode;
  begin
    nSrc := Sender.FocusedNode;
    nTgt := Sender.DropTargetNode;
    idxNew := nTgt.Index;
    if idxNew>Integer(nSrc.Index) then Dec(idxNew);
    if Mode=dmBelow then begin
      Inc(idxNew);
      am := amInsertAfter;
    end else
      am := amInsertBefore;
    FSortings.Move(nSrc.Index, idxNew);
    Sender.MoveTo(nSrc, nTgt, am, False);
    EnableActions;
    DoChange;
  end;

  procedure TfrSorting.tvMainDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
  var nSrc, nTgt: PVirtualNode;
  begin
    nSrc := Sender.FocusedNode;
    nTgt := Sender.DropTargetNode;
    Accept := (Sender=Source) and (nTgt<>nil) and (nSrc<>nTgt);
    if Accept then
      case Mode of
        dmAbove: Accept := nTgt.Index<>nSrc.Index+1;
        dmBelow: Accept := (nTgt.Index<>nSrc.Index-1) and SortingNode(nTgt);
        else     Accept := False;
      end;
    Effect := DROPEFFECT_MOVE;
  end;

  procedure TfrSorting.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
     // Если нормальный пункт сортировки
    if SortingNode(Node) then
      case Column of
        0: ImageIndex := iiSorting;
        1: ImageIndex := iif(FSortings[Node.Index].Order=soAsc, iiSortAsc, iiSortDesc);
      end;
  end;

  procedure TfrSorting.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var ps: TPhoaSorting;
  begin
     // Если нормальный пункт сортировки
    if SortingNode(Node) then begin
      ps := FSortings[Node.Index];
      case Column of
        0: CellText := AnsiToUnicodeCP(PicPropName(ps.Prop), cMainCodePage);
        1: CellText := AnsiToUnicodeCP(ConstVal(iif(ps.Order=soAsc, 'SSort_Ascending', 'SSort_Descending')), cMainCodePage);
      end;
    end;
  end;

  procedure TfrSorting.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
     // У каждого пункта есть кнопка
    Node.CheckType := ctButton;
  end;

  procedure TfrSorting.tvMainKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
  var n: PVirtualNode;
  begin
    n := Sender.FocusedNode;
     // Если жмём ПРОБЕЛ, находясь в ячейке с направлением сортировки, то переключаем это направление
    if (CharCode=VK_SPACE) and ([ssCtrl, ssAlt, ssShift]*Shift=[]) and SortingNode(n) and (Sender.FocusedColumn=1) then begin
      DoDefault := False;
      ToggleOrder(n);
    end;
  end;

  procedure TfrSorting.tvMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var hi: THitInfo;
  begin
     // При клике левой кнопкой на значке направления сортировки переключаем это направление
    if Button=mbLeft then begin
      tvMain.GetHitTestInfoAt(x, y, True, hi);
      if SortingNode(hi.HitNode) and (hi.HitColumn=1) and (hiOnNormalIcon in hi.HitPositions) then ToggleOrder(hi.HitNode);
    end;
  end;

end.
