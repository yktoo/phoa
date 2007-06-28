//**********************************************************************************************************************
//  $Id: udPicOps.pas,v 1.20 2007-06-28 18:41:37 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udPicOps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars,
  phDlg, DKLang, VirtualTrees, StdCtrls, TntStdCtrls, ExtCtrls, TntExtCtrls;

type
  TdPicOps = class(TPhoaDialog)
    cbOp: TTntComboBox;
    dklcMain: TDKLanguageController;
    lGroup: TTntLabel;
    lOp: TTntLabel;
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

   // Отображает диалог операций с изображениями.
  function DoPicOps(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses phUtils, Main, phSettings;

  function DoPicOps(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
  begin
    with TdPicOps.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := AUndoOperations;
        Result := ExecuteModal(False, True);
      finally
        Free;
      end;
  end;

  procedure TdPicOps.ButtonClick_OK;
  begin
    FApp.PerformOperation(
      'PicOperation',
      ['SourceGroup',  FApp.CurGroup,
       'TargetGroup',  PicGroupsVT_GetNodeGroup(tvGroups, tvGroups.FocusedNode),
       'Pics',         FApp.SelectedPics,
       'PicOperation', Byte(cbOp.ItemIndex)]);
    inherited ButtonClick_OK;
  end;

  procedure TdPicOps.DoCreate;
  begin
    inherited DoCreate;
    HelpContext := IDH_intf_pic_operations;
    ApplyTreeSettings(tvGroups);
    tvGroups.HintMode     := GTreeHintModeToVTHintMode(TGroupTreeHintMode(SettingValueInt(ISettingID_Browse_GT_Hints)));
    tvGroups.NodeDataSize := SizeOf(TObject);
  end;

  procedure TdPicOps.ExecuteInitialize;
  begin
    inherited ExecuteInitialize;
     // Загружаем дерево ("Результаты поиска" тут опускаем)
    tvGroups.RootNodeCount := 1;
     // Инициализируем все узлы, чтобы они раскрылись быстро
    tvGroups.BeginUpdate;
    try
      tvGroups.ReinitChildren(nil, True);
    finally
      tvGroups.EndUpdate;
    end;
    cbOp.ItemIndex := 0;
  end;

  function TdPicOps.GetDataValid: Boolean;
  var n: PVirtualNode;
  begin
    n := tvGroups.FocusedNode;
    Result := (cbOp.ItemIndex>=0) and (n<>nil) and (PicGroupsVT_GetNodeGroup(tvGroups, n)<>FApp.CurGroup);
  end;

  procedure TdPicOps.tvGroupsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  begin
    PicGroupsVT_HandleBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellRect, FApp, False);
  end;

  procedure TdPicOps.tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
  begin
    PicGroupsVT_HandleBeforeItemErase(Sender, TargetCanvas, Node, ItemRect, ItemColor, EraseAction, False);
  end;

  procedure TdPicOps.tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    Modified := True;
  end;

  procedure TdPicOps.tvGroupsCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  begin
    PicGroupsVT_HandleCollapsing(Sender, Node, Allowed);
  end;

  procedure TdPicOps.tvGroupsExpandedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PicGroupsVT_HandleExpandedCollapsed(Sender, Node, False);
  end;

  procedure TdPicOps.tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PicGroupsVT_HandleFreeNode(Sender, Node);
  end;

  procedure TdPicOps.tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
  begin
    PicGroupsVT_HandleGetHint(Sender, Node, Column, LineBreakStyle, HintText, FApp, False);
  end;

  procedure TdPicOps.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    PicGroupsVT_HandleGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex, False);
     // Операции группы с самой собой делать бессмысленно
    if (Kind=ikOverlay) and (PicGroupsVT_GetNodeGroup(Sender, Node)=FApp.CurGroup) then ImageIndex := iiNo;
  end;

  procedure TdPicOps.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    PicGroupsVT_HandleGetText(Sender, Node, Column, TextType, CellText, nil);
  end;

  procedure TdPicOps.tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
    PicGroupsVT_HandleInitNode(Sender, ParentNode, Node, InitialStates, FApp.ProjectX.RootGroupX, nil, False, True);
  end;

  procedure TdPicOps.tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    PicGroupsVT_HandlePaintText(Sender, TargetCanvas, Node, Column, TextType);
  end;

end.

