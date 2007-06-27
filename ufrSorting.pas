//**********************************************************************************************************************
//  $Id: ufrSorting.pas,v 1.12 2007-06-27 18:29:45 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrSorting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActiveX,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps,
  StdCtrls, TB2Item, TBX, Menus, VirtualTrees;

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
    FSortings: IPhotoAlbumPicSortingList;
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
     // Устанавливает стандартные пункты сортировки
    procedure Reset;
     // Настраивает tvMain (в ответ на изменение FSortings)
    procedure SyncSortings;
     // Props
     // -- Редактируемые группировки
    property Sortings: IPhotoAlbumPicSortingList read FSortings;
     // -- Событие модификации списка
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main, phSettings;

  constructor TfrSorting.Create(AOwner: TComponent);
  var
    pp: TPicProperty;
    tbi: TTBXCustomItem;
  begin
    inherited Create(AOwner);
    FSortings := NewPhotoAlbumPicSortingList;
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
    FSortings.Move(idx, idx+1);
    with tvMain do MoveTo(FocusedNode, GetNextSibling(FocusedNode), amInsertAfter, False);
    EnableActions;
    DoChange;
  end;

  procedure TfrSorting.ipmMoveUpClick(Sender: TObject);
  var idx: Integer;
  begin
    idx := tvMain.FocusedNode.Index;
    FSortings.Move(idx, idx-1);
    with tvMain do MoveTo(FocusedNode, GetPreviousSibling(FocusedNode), amInsertBefore, False);
    EnableActions;
    DoChange;
  end;

  procedure TfrSorting.pmMainPopup(Sender: TObject);
  var i: Integer;
  begin
     // Выключаем сортировки, которые уже есть в списке выбранных
    for i := 0 to ipmsmProp.Count-1 do
      with ipmsmProp.Items[i] do Visible := FSortings.IndexOfProp(TPicProperty(Tag))<0;
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
    if AParent<>nil then ApplyTreeSettings(tvMain);
  end;

  function TfrSorting.SortingNode(Node: PVirtualNode): Boolean;
  begin
    Result := (Node<>nil) and (Integer(Node.Index)<FSortings.Count);
  end;

  procedure TfrSorting.SortingPropClick(Sender: TObject);
  var
    n: PVirtualNode;
    Prop: TPicProperty;
    Sorting: IPhotoAlbumPicSorting;
  begin
    n := tvMain.FocusedNode;
    Prop := TPicProperty(TComponent(Sender).Tag);
     // Смена свойства у существующего пункта
    if SortingNode(n) then begin
      FSortings[n.Index].Prop := Prop;
      tvMain.InvalidateNode(n);
      EnableActions;
     // Добавление нового пункта
    end else begin
      Sorting := NewPhotoAlbumPicSorting;
      Sorting.Prop := Prop;
      FSortings.Add(Sorting);
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
      FSortings[Node.Index].ToggleDirection;
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
    ActivateVTNode(Sender, Node);
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
        1: ImageIndex := iif(FSortings[Node.Index].Direction=psdAsc, iiSortAsc, iiSortDesc);
      end;
  end;

  procedure TfrSorting.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var Sorting: IPhotoAlbumPicSorting;
  begin
     // Если нормальный пункт сортировки
    if SortingNode(Node) then begin
      Sorting := FSortings[Node.Index];
      case Column of
        0: CellText := PhoaAnsiToUnicode(PicPropName(Sorting.Prop));
        1: CellText := PhoaAnsiToUnicode(DKLangConstW(iif(Sorting.Direction=psdAsc, 'SSort_Ascending', 'SSort_Descending')));
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

