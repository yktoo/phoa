//**********************************************************************************************************************
//  $Id: udSelPhoaGroup.pas,v 1.12 2004-10-15 13:49:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSelPhoaGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, 
  phDlg, VirtualTrees, StdCtrls, ExtCtrls, DKLang;

type
  TdSelPhoaGroup = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    lGroup: TLabel;
    tvGroups: TVirtualStringTree;
    procedure tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
     // ����������
    FApp: IPhotoAlbumApp;
     // ����� ������
    FUndoOperations: TPhoaOperations;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetDataValid: Boolean; override;
  end;

  function MakeGroupFromView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main, phSettings;

  function MakeGroupFromView(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
  begin
    with TdSelPhoaGroup.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := AUndoOperations;
        Result := Execute;
      finally
        Free;
      end;
  end;

  procedure TdSelPhoaGroup.ButtonClick_OK;
  var Changes: TPhoaOperationChanges;
  begin
    Changes := [];
    fMain.BeginOperation;
    try
      TPhoaOp_ViewMakeGroup.Create(
        FUndoOperations,
        FApp.Project,
        PPhotoAlbumPicGroup(tvGroups.GetNodeData(tvGroups.FocusedNode))^,
        Changes);
    finally
      fMain.EndOperation(Changes);
    end;
    inherited ButtonClick_OK;
  end;

  function TdSelPhoaGroup.GetDataValid: Boolean;
  begin
    Result := tvGroups.FocusedNode<>nil;
  end;

  procedure TdSelPhoaGroup.InitializeDialog;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_sel_phoa_group;
    OKIgnoresModified := True;
    ApplyTreeSettings(tvGroups);
    tvGroups.NodeDataSize  := SizeOf(Pointer);
    tvGroups.RootNodeCount := 1;
  end;

  procedure TdSelPhoaGroup.tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    Modified := True;
  end;

  procedure TdSelPhoaGroup.tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PPhotoAlbumPicGroup(Sender.GetNodeData(Node))^ := nil;
  end;

  procedure TdSelPhoaGroup.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Kind in [ikNormal, ikSelected] then ImageIndex := iif(Sender.NodeParent[Node]=nil, iiPhoA, iiFolder);
  end;

  procedure TdSelPhoaGroup.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    p: PPhotoAlbumPicGroup;
    s: String;
  begin
    p := Sender.GetNodeData(Node);
    if p<>nil then
      if TextType=ttStatic then begin
        if p^.Pics.Count>0 then s := Format('(%d)', [p^.Pics.Count]);
      end else if Sender.NodeParent[Node]<>nil then s := p^.Text
      else s := ConstVal('SPhotoAlbumNode');
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TdSelPhoaGroup.tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var p, pp: PPhotoAlbumPicGroup;
  begin
    p := Sender.GetNodeData(Node);
    if ParentNode=nil then
      p^ := FApp.Project.RootGroupX 
    else begin
      pp := Sender.GetNodeData(ParentNode);
      p^ := pp^.GroupsX[Node.Index];
    end;
    Sender.ChildCount[Node] := p^.Groups.Count;
     // ������������� �������� ���� ��� ���� ������ ���������
    if (ParentNode=nil) or p^.Expanded then Include(InitialStates, ivsExpanded);
  end;

  procedure TdSelPhoaGroup.tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    if TextType=ttStatic then TargetCanvas.Font.Color := clGrayText;
  end;

end.

