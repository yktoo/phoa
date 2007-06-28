//**********************************************************************************************************************
//  $Id: udSelPhoaGroup.pas,v 1.21 2007-06-28 18:41:40 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSelPhoaGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, 
  phDlg, DKLang, VirtualTrees, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls;

type
  TdSelPhoaGroup = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    lGroup: TTntLabel;
    tvGroups: TVirtualStringTree;
    procedure tvGroupsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure tvGroupsExpandedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
    procedure tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
     // Приложение
    FApp: IPhotoAlbumApp;
     // Буфер отката
    FUndoOperations: TPhoaOperations;
  protected
    function  GetDataValid: Boolean; override;
    procedure ButtonClick_OK; override;
    procedure DoCreate; override;
    procedure ExecuteInitialize; override;
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
        Result := ExecuteModal(False, True);
      finally
        Free;
      end;
  end;

  procedure TdSelPhoaGroup.ButtonClick_OK;
  begin
    FApp.PerformOperation('ViewMakeGroup', ['Group', PicGroupsVT_GetNodeGroup(tvGroups, tvGroups.FocusedNode)]);
    inherited ButtonClick_OK;
  end;

  procedure TdSelPhoaGroup.DoCreate;
  begin
    inherited DoCreate;
    HelpContext := IDH_intf_sel_phoa_group;
    tvGroups.HintMode     := GTreeHintModeToVTHintMode(TGroupTreeHintMode(SettingValueInt(ISettingID_Browse_GT_Hints)));
    tvGroups.NodeDataSize := SizeOf(TObject);
    ApplyTreeSettings(tvGroups);
  end;

  procedure TdSelPhoaGroup.ExecuteInitialize;
  begin
    inherited ExecuteInitialize;
    tvGroups.RootNodeCount := 1;
  end;

  function TdSelPhoaGroup.GetDataValid: Boolean;
  begin
    Result := tvGroups.FocusedNode<>nil;
  end;

  procedure TdSelPhoaGroup.tvGroupsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  begin
    PicGroupsVT_HandleBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellRect, FApp, False);
  end;

  procedure TdSelPhoaGroup.tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
  begin
    PicGroupsVT_HandleBeforeItemErase(Sender, TargetCanvas, Node, ItemRect, ItemColor, EraseAction, False);
  end;

  procedure TdSelPhoaGroup.tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    Modified := True;
  end;

  procedure TdSelPhoaGroup.tvGroupsCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  begin
    PicGroupsVT_HandleCollapsing(Sender, Node, Allowed);
  end;

  procedure TdSelPhoaGroup.tvGroupsExpandedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PicGroupsVT_HandleExpandedCollapsed(Sender, Node, False);
  end;

  procedure TdSelPhoaGroup.tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PicGroupsVT_HandleFreeNode(Sender, Node);
  end;

  procedure TdSelPhoaGroup.tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
  begin
    PicGroupsVT_HandleGetHint(Sender, Node, Column, LineBreakStyle, HintText, FApp, False);
  end;

  procedure TdSelPhoaGroup.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    PicGroupsVT_HandleGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex, False);
  end;

  procedure TdSelPhoaGroup.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    PicGroupsVT_HandleGetText(Sender, Node, Column, TextType, CellText, nil);
  end;

  procedure TdSelPhoaGroup.tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
    PicGroupsVT_HandleInitNode(Sender, ParentNode, Node, InitialStates, FApp.ProjectX.RootGroupX, nil, False, True);
  end;

  procedure TdSelPhoaGroup.tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    PicGroupsVT_HandlePaintText(Sender, TargetCanvas, Node, Column, TextType);
  end;

end.

