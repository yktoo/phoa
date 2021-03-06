//**********************************************************************************************************************
//  $Id: udViewProps.pas,v 1.22 2007-06-30 10:36:21 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udViewProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActiveX,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps,
  phDlg, TB2Item, TBX, Menus, DKLang, ufrExprPicFilter, TntForms,
  ufrSorting, VirtualTrees, StdCtrls, TntStdCtrls, ComCtrls, TntComCtrls,
  ExtCtrls, TntExtCtrls;

type
  TdViewProps = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    eName: TTntEdit;
    frExprPicFilter: TfrExprPicFilter;
    frSorting: TfrSorting;
    ipmDelete: TTBXItem;
    ipmMoveDown: TTBXItem;
    ipmMoveUp: TTBXItem;
    ipmSep: TTBXSeparatorItem;
    ipmsmProp: TTBXSubmenuItem;
    lGrouping: TTntLabel;
    lName: TTntLabel;
    pcMain: TTntPageControl;
    pmGrouping: TTBXPopupMenu;
    tsFilterExpr: TTntTabSheet;
    tsGeneral: TTntTabSheet;
    tvGrouping: TVirtualStringTree;
    procedure ipmDeleteClick(Sender: TObject);
    procedure ipmMoveDownClick(Sender: TObject);
    procedure ipmMoveUpClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
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
  private
     // True, ���� ����� ���������� �������������
    FIsAdding: Boolean;
     // ����������
    FApp: IPhotoAlbumApp;
     // ����� ������
    FUndoOperations: TPhoaOperations;
     // ������������� �����������
    FGroupings: IPhotoAlbumPicGroupingList;
     // ����������� ����������� ��������
    procedure EnableActions;
     // ����������� tvGrouping (� ����� �� ��������� FGroupings)
    procedure SyncGroupings;
     // ����������� ��������� � ��������� ����� �������������������� ����������� ��� ���� Node
    procedure ToggleUnclassified(Node: PVirtualNode);
     // ������� ����� �� ������ �������� ��� �����������
    procedure GroupingPropClick(Sender: TObject);
     // ���������� True, ���� ���� ������������� ��������� ������ � ������ �����������
    function  GroupingNode(Node: PVirtualNode): Boolean;
  protected
    function  GetDataValid: Boolean; override;
    procedure ButtonClick_OK; override;
    procedure DoCreate; override;
    procedure ExecuteInitialize; override;
  end;

   // ����������� �������������
  function EditView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
   // ��������� �������������
  function AddView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses
  phUtils, ConsVars, Main, CommCtrl, Themes, phSettings, phParsingPicFilter;

  function EditView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
  begin
    with TdViewProps.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := AUndoOperations;
        Result := ExecuteModal(False, False);
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
        Result := ExecuteModal(False, False);
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdViewProps
   //===================================================================================================================

  procedure TdViewProps.ButtonClick_OK;
  var
    wsExpression: WideString;
    PicFilter: IPhoaParsingPicFilter;
  begin
     // ��������� ��������� ��������� �������
    wsExpression := frExprPicFilter.Expression;
    if Trim(wsExpression)<>'' then begin
      PicFilter := NewPhoaParsingPicFilter;
      PicFilter.Expression := wsExpression;
      try
        PicFilter.ParseExpression(True, True);
      except
        on EPhoaParseError do begin
          frExprPicFilter.CaretPos := PicFilter.ParseErrorLocation;
          raise;
        end;
      end;
    end;
     // ��������� ��������
    if FIsAdding then
      FApp.PerformOperation(
        'ViewNew',
        ['Name', eName.Text, 'FilterExpression', wsExpression, 'Groupings', FGroupings, 'Sortings', frSorting.Sortings])
    else
      FApp.PerformOperation(
        'ViewEdit',
        ['View', FApp.ProjectX.CurrentViewX, 'Name', eName.Text, 'FilterExpression', wsExpression,
         'Groupings', FGroupings, 'Sortings', frSorting.Sortings]);
    inherited ButtonClick_OK;
  end;

  procedure TdViewProps.DoCreate;
  var gbp: TPicGroupByProperty;
  begin
    inherited DoCreate;
    HelpContext := IDH_intf_view_props;
    FGroupings := NewPhotoAlbumPicGroupingList;
     // ����������� tvGrouping
    ApplyTreeSettings(tvGrouping);
    frSorting.OnChange                 := DlgDataChange;
    frExprPicFilter.OnExpressionChange := DlgDataChange;
     // ������ ������ ���� "�������� �����������"
    for gbp := Low(gbp) to High(gbp) do
      AddTBXMenuItem(ipmsmProp, GroupByPropName(gbp), iiGrouping, Byte(gbp), GroupingPropClick);
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

  procedure TdViewProps.ExecuteInitialize;
  var View: IPhotoAlbumView;
  begin
    inherited ExecuteInitialize;
    if not FIsAdding then begin
      View := FApp.ProjectX.CurrentViewX;
      eName.Text := View.Name;
      FGroupings.Assign(View.Groupings);
      frSorting.Sortings.Assign(View.Sortings);
      frSorting.SyncSortings;
      frExprPicFilter.Expression := View.FilterExpression;
    end;
    SyncGroupings;
    pcMain.ActivePage := tsGeneral;
  end;

  function TdViewProps.GetDataValid: Boolean;
  begin
    Result := Trim(eName.Text)<>'';
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
     // ����� �������� � ������������� ������
    if GroupingNode(n) then begin
      FGroupings[n.Index].Prop := GBProp;
      tvGrouping.InvalidateNode(n);
      EnableActions;
     // ���������� ������ ������
    end else begin
      Grouping := NewPhotoAlbumPicGrouping;
      Grouping.Prop := GBProp;
      Grouping.UnclassifiedInOwnFolder := True;
      FGroupings.Add(Grouping);
      SyncGroupings;
    end;
    Modified := True;
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

  procedure TdViewProps.pcMainChange(Sender: TObject);
  begin
    if      pcMain.ActivePage=tsGeneral    then eName.SetFocus
    else if pcMain.ActivePage=tsFilterExpr then frExprPicFilter.FocusEditor;
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
     // ������ Unclassified CheckBox (����� ��� gbpFilePath, �.�. ����������� ��� ����� �� ������)
    if (Column=1) and GroupingNode(Node) then begin
      Grouping := FGroupings[Node.Index];
      if Grouping.Prop<>gbpFilePath then begin
         // ����������� Rect
        with CellRect do begin
          Left := (Left+Right-16) div 2;
          Right := Left+16;
          Top := (Top+Bottom-16) div 2;
          Bottom := Top+16;
        end;
         // ������
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
    if GroupingNode(Node) and (Column=0) then CellText := GroupByPropName(FGroupings[Node.Index].Prop);
  end;

  procedure TdViewProps.tvGroupingInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
     // � ������� ������ ���� ������
    Node.CheckType := ctButton;
  end;

  procedure TdViewProps.tvGroupingKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
  var n: PVirtualNode;
  begin
    n := Sender.FocusedNode;
     // ���� ��� ������, �������� � ������ � Unclassified CheckBox, �� ����������� ���� CheckBox
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
     // ��� ����� ����� ������� �� Unclassified CheckBox ����������� ���� CheckBox
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

