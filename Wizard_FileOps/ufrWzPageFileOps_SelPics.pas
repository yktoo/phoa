//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_SelPics.pas,v 1.4 2007-06-28 18:41:59 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_SelPics;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars,
  phWizard, StdCtrls, VirtualTrees, TB2Item, TBX, Menus,
  ActnList, DKLang, TntActnList, TntStdCtrls;

type
  TfrWzPageFileOps_SelPics = class(TWizardPage)
    aCheckAll: TTntAction;
    aInvertChecks: TTntAction;
    alMain: TTntActionList;
    aUncheckAll: TTntAction;
    dklcMain: TDKLanguageController;
    gbValidity: TTntGroupBox;
    ipmGroupsCheckAll: TTBXItem;
    ipmGroupsInvertChecks: TTBXItem;
    ipmGroupsUncheckAll: TTBXItem;
    lCountInfo: TTntLabel;
    pmGroups: TTBXPopupMenu;
    rbAllPics: TTntRadioButton;
    rbSelGroups: TTntRadioButton;
    rbSelPics: TTntRadioButton;
    rbValidityAny: TTntRadioButton;
    rbValidityInvalid: TTntRadioButton;
    rbValidityValid: TTntRadioButton;
    tvGroups: TVirtualStringTree;
    procedure aaCheckAll(Sender: TObject);
    procedure aaInvertChecks(Sender: TObject);
    procedure aaUncheckAll(Sender: TObject);
    procedure RBSelPicturesClick(Sender: TObject);
    procedure tvGroupsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure tvGroupsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
    procedure tvGroupsExpandedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
    procedure tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure UpdateCountInfoNotify(Sender: TObject);
  private
     // Количество выбранных групп
    FSelGroupCount: Integer;
     // Количество выбранных изображений
    FSelPicCount: Integer;
     // Суммарный размер файлов выбранных изображений
    FSelPicFileTotalSize: Int64;
     // Настраивает доступность [вторичных] контролов выбора изображений
    procedure AdjustPicControls;
     // Обновляет информацию о выбранных изображениях
    procedure UpdateCountInfo;
     // Ставит, снимает или инвертирует отметку у всех групп с изображениями
    procedure CheckFiles(Mode: TMassCheckMode);
     // Prop handlers
    function  GetValidityFilter: TFileOpSelPicValidityFilter;
    procedure SetValidityFilter(Value: TFileOpSelPicValidityFilter);
  protected
    function  GetDataValid: Boolean; override;
    function  NextPage: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure DoCreate; override;
     // Props
     // -- Текущий фильтр выбора по наличию соответствующих файлов
    property ValidityFilter: TFileOpSelPicValidityFilter read GetValidityFilter write SetValidityFilter;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard, Main, phSettings;

  procedure TfrWzPageFileOps_SelPics.aaCheckAll(Sender: TObject);
  begin
    CheckFiles(mcmAll);
  end;

  procedure TfrWzPageFileOps_SelPics.aaInvertChecks(Sender: TObject);
  begin
    CheckFiles(mcmInvert);
  end;

  procedure TfrWzPageFileOps_SelPics.aaUncheckAll(Sender: TObject);
  begin
    CheckFiles(mcmNone);
  end;

  procedure TfrWzPageFileOps_SelPics.AdjustPicControls;
  begin
    EnableControl(rbSelGroups.Checked, tvGroups);
    UpdateCountInfo;
  end;

  procedure TfrWzPageFileOps_SelPics.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var
    Wiz: TdFileOpsWizard;
    View: IPhotoAlbumView;
  begin
    inherited BeforeDisplay(ChangeMethod);
    Wiz := TdFileOpsWizard(StorageForm);
    rbSelPics.Enabled  := Wiz.App.SelectedPics.Count>0;
     // Настраиваем название радиокнопки "Все изображения фотоальбома/представления"
    View := Wiz.App.ProjectX.CurrentViewX;
    if View=nil then
      rbAllPics.Caption := DKLangConstW('SWzFileOps_AllPicsInPhoa')
    else
      rbAllPics.Caption := DKLangConstW('SWzFileOps_AllPicsInView', [View.Name]);
     // Устанавливаем выбранную радиокнопку в зависимости от режима
    rbSelPics.Checked   := Wiz.SelPicMode=fospmSelPics;
    rbAllPics.Checked   := Wiz.SelPicMode=fospmAll;
    rbSelGroups.Checked := Wiz.SelPicMode=fospmSelGroups;
     // Прочие опции
    ValidityFilter      := Wiz.SelPicValidityFilter;
    rbValidityInvalid.Enabled := Wiz.FileOpKind in [fokMoveFiles, fokDeleteFiles, fokRepairFileLinks];
     // Настраиваем вторичные контролы
    AdjustPicControls;
  end;

  procedure TfrWzPageFileOps_SelPics.CheckFiles(Mode: TMassCheckMode);
  var
    n: PVirtualNode;
    cs: TCheckState;
  begin
    tvGroups.BeginUpdate;
    try
      n := tvGroups.GetFirst;
      while n<>nil do begin
        if tvGroups.CheckType[n]=ctCheckBox then begin
          case Mode of
            mcmAll:  cs := csCheckedNormal;
            mcmNone: cs := csUncheckedNormal;
            else     cs := aCheckStates[n.CheckState<>csCheckedNormal];
          end;
          tvGroups.CheckState[n] := cs;
        end;
        n := tvGroups.GetNext(n);
      end;
    finally
      tvGroups.EndUpdate;
    end;
    UpdateCountInfo;
  end;

  procedure TfrWzPageFileOps_SelPics.DoCreate;
  begin
    inherited DoCreate;
     // Настраиваем дерево групп
    ApplyTreeSettings(tvGroups);
    tvGroups.HintMode      := GTreeHintModeToVTHintMode(TGroupTreeHintMode(SettingValueInt(ISettingID_Browse_GT_Hints)));
    tvGroups.NodeDataSize  := SizeOf(TObject);
    tvGroups.RootNodeCount := 1;
  end;

  function TfrWzPageFileOps_SelPics.GetDataValid: Boolean;
  begin
    Result := FSelPicCount>0;
  end;

  function TfrWzPageFileOps_SelPics.GetValidityFilter: TFileOpSelPicValidityFilter;
  begin
    if rbValidityAny.Checked        then Result := fospvfAny
    else if rbValidityValid.Checked then Result := fospvfValidOnly
    else                                 Result := fospvfInvalidOnly;
  end;

  function TfrWzPageFileOps_SelPics.NextPage: Boolean;
  var
    n: PVirtualNode;
    Wiz: TdFileOpsWizard;
  begin
     // Устанавливаем режим выбора изображений и настраиваем список выбранных групп
    Wiz := TdFileOpsWizard(StorageForm);
    Wiz.SelectedGroups.Clear;
     // -- Выбранные во вьюере изображения
    if rbSelPics.Checked then
      Wiz.SelPicMode := fospmSelPics
     // -- Все изображения фотоальбома
    else if rbAllPics.Checked then
      Wiz.SelPicMode := fospmAll
     // -- Изображения из выбранных групп
    else begin
      Wiz.SelPicMode := fospmSelGroups;
      n := tvGroups.GetFirst;
      while n<>nil do begin
        if n.CheckState=csCheckedNormal then Wiz.SelectedGroups.Add(PicGroupsVT_GetNodeGroup(tvGroups, n));
        n := tvGroups.GetNext(n);
      end;
    end;
     // Сохраняем прочие опции
    Wiz.SelPicValidityFilter := ValidityFilter;
    Result := True;
  end;

  procedure TfrWzPageFileOps_SelPics.RBSelPicturesClick(Sender: TObject);
  begin
    AdjustPicControls;
  end;

  procedure TfrWzPageFileOps_SelPics.SetValidityFilter(Value: TFileOpSelPicValidityFilter);
  var Wiz: TdFileOpsWizard;
  begin
    Wiz := TdFileOpsWizard(StorageForm);
    rbValidityAny.Checked     := Wiz.SelPicValidityFilter=fospvfAny;
    rbValidityValid.Checked   := Wiz.SelPicValidityFilter=fospvfValidOnly;
    rbValidityInvalid.Checked := Wiz.SelPicValidityFilter=fospvfInvalidOnly;
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  var Wiz: TdFileOpsWizard;
  begin
    Wiz := TdFileOpsWizard(StorageForm);
    PicGroupsVT_HandleBeforeCellPaint(Sender, TargetCanvas, Node, Column, CellRect, Wiz.App, Wiz.App.Project.ViewIndex>=0);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
  var Wiz: TdFileOpsWizard;
  begin
    Wiz := TdFileOpsWizard(StorageForm);
    PicGroupsVT_HandleBeforeItemErase(Sender, TargetCanvas, Node, ItemRect, ItemColor, EraseAction, Wiz.App.Project.ViewIndex>=0);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    if not (tsUpdating in Sender.TreeStates) then UpdateCountInfo;
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode; var Allowed: Boolean);
  begin
    PicGroupsVT_HandleCollapsing(Sender, Node, Allowed);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsExpandedCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PicGroupsVT_HandleExpandedCollapsed(Sender, Node, False);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    PicGroupsVT_HandleFreeNode(Sender, Node);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
  var Wiz: TdFileOpsWizard;
  begin
    Wiz := TdFileOpsWizard(StorageForm);
    PicGroupsVT_HandleGetHint(Sender, Node, Column, LineBreakStyle, HintText, Wiz.App, Wiz.App.Project.ViewIndex>=0);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  var Wiz: TdFileOpsWizard;
  begin
    Wiz := TdFileOpsWizard(StorageForm);
    PicGroupsVT_HandleGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex, Wiz.App.Project.ViewIndex>=0);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var Wiz: TdFileOpsWizard;
  begin
    Wiz := TdFileOpsWizard(StorageForm);
    PicGroupsVT_HandleGetText(Sender, Node, Column, TextType, CellText, Wiz.App.Project.CurrentView);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var
    Group: IPhotoAlbumPicGroup;
    Wiz: TdFileOpsWizard;
  begin
    Wiz := TdFileOpsWizard(StorageForm);
    PicGroupsVT_HandleInitNode(Sender, ParentNode, Node, InitialStates, Wiz.App.ProjectX.ViewRootGroupX, nil, False, True);
     // Получаем группу, с которой ассоциирован узел
    Group := PicGroupsVT_GetNodeGroup(Sender, Node);
     // Если у узла есть изображения, настраиваем птицу
    if Group.Pics.Count>0 then begin
      Node.CheckType  := ctCheckBox;
      Node.CheckState := aCheckStates[Wiz.SelectedGroups.IndexOfID(Group.ID)>=0];
    end;
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    PicGroupsVT_HandlePaintText(Sender, TargetCanvas, Node, Column, TextType);
  end;

  procedure TfrWzPageFileOps_SelPics.UpdateCountInfo;
  var
    n: PVirtualNode;
    Pic: IPhoaPic;
    Pics: IPhoaPicList;
    GroupPics: IPhoaMutablePicList;
    i: Integer;
    Wiz: TdFileOpsWizard;
    ValFilter: TFileOpSelPicValidityFilter;
  begin
    StartWait;
    try
      Wiz := TdFileOpsWizard(StorageForm);
      FSelPicCount         := 0;
      FSelPicFileTotalSize := 0;
      ValFilter := ValidityFilter;
       // Выбранные изображения
      if rbSelPics.Checked then begin
        FSelGroupCount := 1;
        Pics := Wiz.App.SelectedPics;
       // Все изображения фотоальбома
      end else if rbAllPics.Checked then begin
        FSelGroupCount := Wiz.App.Project.RootGroup.NestedGroupCount+1;
        Pics := Wiz.App.Project.Pics;
       // Изображения из выбранных групп. Создаём уникальный список изображений и добавляем в него изображения из
       //   отмеченных в дереве групп
      end else begin
        FSelGroupCount := 0;
        GroupPics := NewPhotoAlbumPicList(True);
        n := tvGroups.GetFirst;
        while n<>nil do begin
          if n.CheckState=csCheckedNormal then begin
            Inc(FSelGroupCount);
            GroupPics.Add(PicGroupsVT_GetNodeGroup(tvGroups, n).Pics, True);
          end;
          n := tvGroups.GetNext(n);
        end;
        Pics := GroupPics;
      end;
       // Добавляем изображения
      for i := 0 to Pics.Count-1 do begin
        Pic := Pics[i];
         // Если нужно учитывать валидность - проверяем наличие файла
        if (ValFilter=fospvfAny) or (FileExists(Pic.FileName)=(ValFilter=fospvfValidOnly)) then begin
          Inc(FSelPicCount);
          Inc(FSelPicFileTotalSize, Pic.FileSize);
        end;
      end;
       // Обновляем информацию
      lCountInfo.Caption := DKLangConstW('SWzFileOps_PicGroupSelectedCount', [FSelPicCount, FSelGroupCount, HumanReadableSize(FSelPicFileTotalSize)]);
      StateChanged;
    finally
      StopWait;
    end;
  end;

  procedure TfrWzPageFileOps_SelPics.UpdateCountInfoNotify(Sender: TObject);
  begin
    UpdateCountInfo;
  end;

end.

