//**********************************************************************************************************************
//  $Id: udViewProps.pas,v 1.11 2004-10-15 13:49:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udViewProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActiveX,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps,
  phDlg, TBX, TB2Item, Menus, StdCtrls, ExtCtrls, VirtualTrees,
  ufrSorting, DKLang;

type
  TdViewProps = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    pmGrouping: TTBXPopupMenu;
    ipmsmProp: TTBXSubmenuItem;
    ipmDelete: TTBXItem;
    ipmSep: TTBXSeparatorItem;
    ipmMoveUp: TTBXItem;
    ipmMoveDown: TTBXItem;
    lName: TLabel;
    lGrouping: TLabel;
    eName: TEdit;
    tvGrouping: TVirtualStringTree;
    frSorting: TfrSorting;
    procedure tvGroupingAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tvGroupingChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupingChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupingDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure tvGroupingDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure tvGroupingDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure tvGroupingGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvGroupingGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupingInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvGroupingKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure tvGroupingMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ipmDeleteClick(Sender: TObject);
    procedure ipmMoveUpClick(Sender: TObject);
    procedure ipmMoveDownClick(Sender: TObject);
  private
     // True, если режим добавления представления
    FIsAdding: Boolean;
     // Приложение
    FApp: IPhotoAlbumApp;
     // Буфер отката
    FUndoOperations: TPhoaOperations;
     // Редактируемые группировки
    FGroupings: IPhotoAlbumPicGroupingList;
     // Настраивает доступность действий
    procedure EnableActions;
     // Настраивает tvGrouping (в ответ на изменение FGroupings)
    procedure SyncGroupings;
     // Переключает помещение в отдельную папку неклассифицированных изображений для узла Node
    procedure ToggleUnclassified(Node: PVirtualNode);
     // Событие клика на пункте свойства для группировки
    procedure GroupingPropClick(Sender: TObject);
     // Возвращает True, если узел соответствует реальному пункту в списке группировок
    function  GroupingNode(Node: PVirtualNode): Boolean;
     // Событие изменения frSorting
    procedure frSortingChange(Sender: TObject);
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetDataValid: Boolean; override;
  end;

   // Редактирует представление
  function EditView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
   // Добавляет представление
  function AddView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main, CommCtrl, Themes, phSettings;

  function EditView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
  begin
    with TdViewProps.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := AUndoOperations;
        Result := Execute;
      finally
        Free;
      end;
  end;

  function AddView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
  begin
    with TdViewProps.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := AUndoOperations;
        FIsAdding       := True;
        Result := Execute;
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdViewProps
   //===================================================================================================================

  procedure TdViewProps.ButtonClick_OK;
  var Changes: TPhoaOperationChanges;
  begin        
     // Запоминаем данные отката/выполняем операцию
    Changes := [];
    fMain.BeginOperation;
    try      
      if FIsAdding then
        TPhoaOp_ViewNew.Create(FUndoOperations, FApp.Project, eName.Text, FGroupings, frSorting.Sortings, Changes)
      else
        TPhoaOp_ViewEdit.Create(FUndoOperations, FApp.Project, FApp.Project.CurrentViewX, eName.Text, FGroupings, frSorting.Sortings, Changes);
    finally
      fMain.EndOperation(Changes);
    end;
    inherited ButtonClick_OK;
  end;

  procedure TdViewProps.EnableActions;
  var
    n: PVirtualNode;
    idx, iCnt: Integer;
  begin
    n := tvGrouping.FocusedNode;
    if n=nil then idx := -1 else idx := n.Index;
    iCnt := FGroupings.Count;
    ipmsmProp.Enabled   := idx>=0;
    ipmDelete.Enabled   := (idx>=0) and (idx<iCnt);
    ipmMoveUp.Enabled   := (idx>0)  and (idx<iCnt);
    ipmMoveDown.Enabled := (idx>=0) and (idx<iCnt-1);
  end;

  procedure TdViewProps.frSortingChange(Sender: TObject);
  begin
    Modified := True;
  end;

  function TdViewProps.GetDataValid: Boolean;
  begin
    Result := (Trim(eName.Text)<>'') and (FGroupings.Count>0);
  end;

  function TdViewProps.GroupingNode(Node: PVirtualNode): Boolean;
  begin
    Result := (Node<>nil) and (Integer(Node.Index)<FGroupings.Count);
  end;

  procedure TdViewProps.GroupingPropClick(Sender: TObject);
  var
    n: PVirtualNode;
    GBProp: TPicGroupByProperty;
    Grouping: IPhotoAlbumPicGrouping;
  begin
    n := tvGrouping.FocusedNode;
    GBProp := TPicGroupByProperty(TComponent(Sender).Tag);
     // Смена свойства у существующего пункта
    if GroupingNode(n) then begin
      FGroupings[n.Index].Prop := GBProp;
      tvGrouping.InvalidateNode(n);
      EnableActions;
     // Добавление нового пункта
    end else begin
      Grouping := NewPhotoAlbumPicGrouping;
      Grouping.Prop := GBProp;
      Grouping.UnclassifiedInOwnFolder := True;
      FGroupings.Add(Grouping);
      SyncGroupings;
    end;
    Modified := True;
  end;

  procedure TdViewProps.InitializeDialog;
  var
    tbi: TTBCustomItem;
    gbp: TPicGroupByProperty;
    View: IPhotoAlbumView;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_view_props;
    FGroupings := NewPhotoAlbumPicGroupingList;
     // Создаём пункты меню "Добавить группировку"
    for gbp := Low(gbp) to High(gbp) do begin
      tbi := TTBXItem.Create(Self);
      with tbi do begin
        Caption    := GroupByPropName(gbp);
        ImageIndex := iiGrouping;
        Tag        := Byte(gbp);
        OnClick    := GroupingPropClick;
      end;
      ipmsmProp.Add(tbi);
    end;
    if not FIsAdding then begin
      View := FApp.Project.CurrentViewX;
      eName.Text := View.Name;
      FGroupings.Assign(View.Groupings);
      frSorting.Sortings.Assign(View.Sortings);
      frSorting.SyncSortings;
      frSorting.OnChange := frSortingChange;
    end;
     // Настраиваем tvGrouping
    ApplyTreeSettings(tvGrouping);
    SyncGroupings;
  end;

  procedure TdViewProps.ipmDeleteClick(Sender: TObject);
  begin
    FGroupings.Delete(tvGrouping.FocusedNode.Index);
    SyncGroupings;
    Modified := True;
  end;

  procedure TdViewProps.ipmMoveDownClick(Sender: TObject);
  var idx: Integer;
  begin
    idx := tvGrouping.FocusedNode.Index;
    FGroupings.Move(idx, idx+1);
    with tvGrouping do MoveTo(FocusedNode, GetNextSibling(FocusedNode), amInsertAfter, False);
    EnableActions;
    Modified := True;
  end;

  procedure TdViewProps.ipmMoveUpClick(Sender: TObject);
  var idx: Integer;
  begin
    idx := tvGrouping.FocusedNode.Index;
    FGroupings.Move(idx, idx-1);
    with tvGrouping do MoveTo(FocusedNode, GetPreviousSibling(FocusedNode), amInsertBefore, False);
    EnableActions;
    Modified := True;
  end;

  procedure TdViewProps.SyncGroupings;
  begin
    tvGrouping.RootNodeCount := FGroupings.Count+1;
    tvGrouping.Invalidate;
    EnableActions;
  end;

  procedure TdViewProps.ToggleUnclassified(Node: PVirtualNode);
  begin
    if GroupingNode(Node) then begin
      FGroupings[Node.Index].ToggleUnclassifiedInOwnFolder;
      tvGrouping.InvalidateNode(Node);
      Modified := True;
    end;
  end;

  procedure TdViewProps.tvGroupingAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  var
    Grouping: IPhotoAlbumPicGrouping;
    ElementDetails: TThemedElementDetails;
  begin
     // Рисуем Unclassified CheckBox (кроме для gbpFilePath, т.к. изображений без путей не бывает)
    if (Column=1) and GroupingNode(Node) then begin
      Grouping := FGroupings[Node.Index];
      if Grouping.Prop<>gbpFilePath then begin
         // Настраиваем Rect
        with CellRect do begin
          Left := (Left+Right-16) div 2;
          Right := Left+16;
          Top := (Top+Bottom-16) div 2;
          Bottom := Top+16;
        end;
         // Рисуем
        if ThemeServices.ThemesEnabled then
          with ThemeServices do begin
            if Grouping.UnclassifiedInOwnFolder then ElementDetails := GetElementDetails(tbCheckBoxCheckedNormal) else ElementDetails := GetElementDetails(tbCheckBoxUncheckedNormal);
            DrawElement(TargetCanvas.Handle, ElementDetails, CellRect);
          end
        else
          DrawFrameControl(TargetCanvas.Handle, CellRect, DFC_BUTTON, iif(Grouping.UnclassifiedInOwnFolder, DFCS_BUTTONCHECK or DFCS_CHECKED, DFCS_BUTTONCHECK));
      end;
    end;
  end;

  procedure TdViewProps.tvGroupingChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    EnableActions;
  end;

  procedure TdViewProps.tvGroupingChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  var p: TPoint;
  begin
    Sender.Selected[Node] := True;
    Sender.FocusedNode := Node;
    EnableActions;
    with Sender.GetDisplayRect(Node, -1, False) do p := Sender.ClientToScreen(Point(Left, Bottom));
    pmGrouping.Popup(p.x, p.y);
  end;

  procedure TdViewProps.tvGroupingDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
  begin
    Allowed := GroupingNode(Node);
  end;

  procedure TdViewProps.tvGroupingDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
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
    FGroupings.Move(nSrc.Index, idxNew);
    Sender.MoveTo(nSrc, nTgt, am, False);
    EnableActions;
    Modified := True;
  end;

  procedure TdViewProps.tvGroupingDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
  var nSrc, nTgt: PVirtualNode;
  begin
    nSrc := Sender.FocusedNode;
    nTgt := Sender.DropTargetNode;
    Accept := (Sender=Source) and (nTgt<>nil) and (nSrc<>nTgt);
    if Accept then
      case Mode of
        dmAbove: Accept := nTgt.Index<>nSrc.Index+1;
        dmBelow: Accept := (nTgt.Index<>nSrc.Index-1) and GroupingNode(nTgt);
        else     Accept := False;
      end;
    Effect := DROPEFFECT_MOVE;
  end;

  procedure TdViewProps.tvGroupingGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if GroupingNode(Node) and (Column=0) then ImageIndex := iiGrouping;
  end;

  procedure TdViewProps.tvGroupingGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    if GroupingNode(Node) and (Column=0) then CellText := AnsiToUnicodeCP(GroupByPropName(FGroupings[Node.Index].Prop), cMainCodePage);
  end;

  procedure TdViewProps.tvGroupingInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
     // У каждого пункта есть кнопка
    Node.CheckType := ctButton;
  end;

  procedure TdViewProps.tvGroupingKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
  var n: PVirtualNode;
  begin
    n := Sender.FocusedNode;
     // Если жмём ПРОБЕЛ, находясь в ячейке с Unclassified CheckBox, то переключаем этот CheckBox
    if (CharCode=VK_SPACE) and ([ssCtrl, ssAlt, ssShift]*Shift=[]) and GroupingNode(n) and (Sender.FocusedColumn=1) then begin
      DoDefault := False;
      ToggleUnclassified(n);
    end;
  end;

  procedure TdViewProps.tvGroupingMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    hi: THitInfo;
    r: TRect;
  begin
     // При клике левой кнопкой на Unclassified CheckBox переключаем этот CheckBox
    if Button=mbLeft then begin
      tvGrouping.GetHitTestInfoAt(x, y, True, hi);
      if GroupingNode(hi.HitNode) and (hi.HitColumn=1) and (FGroupings[hi.HitNode.Index].Prop<>gbpFilePath) then begin
        r := tvGrouping.GetDisplayRect(hi.HitNode, 1, False);
        with r do begin
          Left := (Left+Right-16) div 2;
          Right := Left+16;
          Top := (Top+Bottom-16) div 2;
          Bottom := Top+16;
        end;
        if PtInRect(r, Point(x, y)) then ToggleUnclassified(hi.HitNode);
      end;
    end;
  end;

end.

