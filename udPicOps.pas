//**********************************************************************************************************************
//  $Id: udPicOps.pas,v 1.3 2004-04-18 16:13:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit udPicOps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, phObj, ConsVars,
  phDlg, VirtualTrees, StdCtrls, ExtCtrls, DTLangTools;

type
  TdPicOps = class(TPhoaDialog)
    lGroup: TLabel;
    lOp: TLabel;
    cbOp: TComboBox;
    tvGroups: TVirtualStringTree;
    procedure tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  private
    FPhoA: TPhotoAlbum;
    FUndoOperations: TPhoaOperations;
    FSourceGroup: TPhoaGroup;
    FSelIDs: TIDArray;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetDataValid: Boolean; override;
  end;

   // Отображает диалог операций с изображениями.
   //   SourceGroup - текущая выбранная группа.
   //   aSelIDs - массив ID выделенных изображений
  function DoPicOps(PhoA: TPhotoAlbum; UndoOperations: TPhoaOperations; SourceGroup: TPhoaGroup; const aSelIDs: TIDArray): Boolean;

implementation
{$R *.dfm}
uses phUtils, Main, phSettings;

  function DoPicOps(PhoA: TPhotoAlbum; UndoOperations: TPhoaOperations; SourceGroup: TPhoaGroup; const aSelIDs: TIDArray): Boolean;
  begin
    with TdPicOps.Create(Application) do
      try
        FPhoA           := PhoA;
        FUndoOperations := UndoOperations;
        FSourceGroup    := SourceGroup;
        FSelIDs         := aSelIDs;
        Result := Execute;
      finally
        Free;
      end;
  end;

  procedure TdPicOps.ButtonClick_OK;
  begin
     // Выполняем операцию
    fMain.PerformOperation(
      TPhoaMultiOp_PicOperation.Create(
        FUndoOperations,
        FPhoA,
        FSourceGroup,
        PPhoaGroup(tvGroups.GetNodeData(tvGroups.FocusedNode))^,
        FSelIDs,
        TPictureOperation(cbOp.ItemIndex)));
    inherited ButtonClick_OK;
  end;

  function TdPicOps.GetDataValid: Boolean;
  var n: PVirtualNode;
  begin
    n := tvGroups.FocusedNode;
    Result :=
      (cbOp.ItemIndex>=0) and
      (n<>nil) and
      (PPhoaGroup(tvGroups.GetNodeData(n))^<>FSourceGroup);
  end;

  procedure TdPicOps.InitializeDialog;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_pic_operations;
    OKIgnoresModified := True;
    ApplyTreeSettings(tvGroups);
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

  procedure TdPicOps.tvGroupsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    Modified := True;
  end;

  procedure TdPicOps.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
     // Операции группы с самой собой делать бессмысленно
    if (Kind in [ikNormal, ikSelected]) and (PPhoaGroup(Sender.GetNodeData(Node))^=FSourceGroup) then
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
