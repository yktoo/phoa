//**********************************************************************************************************************
//  $Id: udPicOps.pas,v 1.11 2004-10-11 11:41:24 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udPicOps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, phIntf, phObj, ConsVars,
  phDlg, VirtualTrees, StdCtrls, ExtCtrls, DKLang;

type
  TdPicOps = class(TPhoaDialog)
    cbOp: TComboBox;
    dklcMain: TDKLanguageController;
    lGroup: TLabel;
    lOp: TLabel;
    tvGroups: TVirtualStringTree;
    procedure tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  private
    FPhoA: TPhotoAlbum;
    FUndoOperations: TPhoaOperations;
    FSourceGroup: IPhotoAlbumPicGroup;
    FPics: IPhoaPicList;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetDataValid: Boolean; override;
  end;

   // Отображает диалог операций с изображениями.
   //   ASourceGroup - текущая выбранная группа.
   //   APics        - список выделенных изображений
  function DoPicOps(APhoA: TPhotoAlbum; AUndoOperations: TPhoaOperations; ASourceGroup: IPhotoAlbumPicGroup; APics: IPhoaPicList): Boolean;

implementation
{$R *.dfm}
uses phUtils, Main, phSettings;

  function DoPicOps(APhoA: TPhotoAlbum; AUndoOperations: TPhoaOperations; ASourceGroup: IPhotoAlbumPicGroup; APics: IPhoaPicList): Boolean;
  begin
    with TdPicOps.Create(Application) do
      try
        FPhoA           := APhoA;
        FUndoOperations := AUndoOperations;
        FSourceGroup    := ASourceGroup;
        FPics           := APics;
        Result := Execute;
      finally
        Free;
      end;
  end;

  procedure TdPicOps.ButtonClick_OK;
  var Operation: TPhoaOperation;
  begin
    Operation := nil;
    fMain.BeginOperation;
    try
      Operation := TPhoaMultiOp_PicOperation.Create(
        FUndoOperations,
        FPhoA,
        FSourceGroup,
        PPhotoAlbumPicGroup(tvGroups.GetNodeData(tvGroups.FocusedNode))^,
        FPics,
        TPictureOperation(cbOp.ItemIndex))
    finally
      fMain.EndOperation(Operation);
    end;
    inherited ButtonClick_OK;
  end;

  function TdPicOps.GetDataValid: Boolean;
  var n: PVirtualNode;
  begin
    n := tvGroups.FocusedNode;
    Result :=
      (cbOp.ItemIndex>=0) and
      (n<>nil) and
      (PPhotoAlbumPicGroup(tvGroups.GetNodeData(n))^<>FSourceGroup);
  end;

  procedure TdPicOps.InitializeDialog;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_pic_operations;
    OKIgnoresModified := True;
    ApplyTreeSettings(tvGroups);
    tvGroups.HintMode      := GTreeHintModeToVTHintMode(TGroupTreeHintMode(SettingValueInt(ISettingID_Browse_GT_Hints)));
    tvGroups.NodeDataSize  := fMain.tvGroups.NodeDataSize;
    tvGroups.RootNodeCount := 1; // "Результаты поиска" тут опускаем
     // Инициализируем все узлы, чтобы они раскрылись быстро
    tvGroups.BeginUpdate;
    try
      tvGroups.ReinitChildren(nil, True);
    finally
      tvGroups.EndUpdate;
    end;
    cbOp.ItemIndex := 0;
  end;

  procedure TdPicOps.tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
  begin
    fMain.tvGroupsBeforeItemErase(Sender, TargetCanvas, Node, ItemRect, ItemColor, EraseAction);
  end;

  procedure TdPicOps.tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    Modified := True;
  end;

  procedure TdPicOps.tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    fMain.tvGroupsFreeNode(Sender, Node);
  end;

  procedure TdPicOps.tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    fMain.tvGroupsGetHint(Sender, Node, Column, TextType, CellText);
  end;

  procedure TdPicOps.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
     // Операции группы с самой собой делать бессмысленно
    if (Kind in [ikNormal, ikSelected]) and (PPhotoAlbumPicGroup(Sender.GetNodeData(Node))^=FSourceGroup) then
      ImageIndex := iiNo
    else
      fMain.tvGroupsGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
  end;

  procedure TdPicOps.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    fMain.tvGroupsGetText(Sender, Node, Column, TextType, CellText);
  end;

  procedure TdPicOps.tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
    fMain.tvGroupsInitNode(Sender, ParentNode, Node, InitialStates);
  end;

  procedure TdPicOps.tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    fMain.tvGroupsPaintText(Sender, TargetCanvas, Node, Column, TextType);
  end;

end.

