//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_SelPics.pas,v 1.15 2004-10-15 13:49:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_SelPics;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars,
  phWizard, StdCtrls, VirtualTrees, TB2Item, TBX, Menus,
  ActnList, DKLang;

type
  TfrWzPageFileOps_SelPics = class(TWizardPage)
    rbAllPics: TRadioButton;
    rbSelPics: TRadioButton;
    rbSelGroups: TRadioButton;
    tvGroups: TVirtualStringTree;
    lCountInfo: TLabel;
    alMain: TActionList;
    aCheckAll: TAction;
    aUncheckAll: TAction;
    aInvertChecks: TAction;
    pmGroups: TTBXPopupMenu;
    ipmGroupsCheckAll: TTBXItem;
    ipmGroupsUncheckAll: TTBXItem;
    ipmGroupsInvertChecks: TTBXItem;
    gbValidity: TGroupBox;
    rbValidityAny: TRadioButton;
    rbValidityValid: TRadioButton;
    rbValidityInvalid: TRadioButton;
    dklcMain: TDKLanguageController;
    procedure aaCheckAll(Sender: TObject);
    procedure aaInvertChecks(Sender: TObject);
    procedure aaUncheckAll(Sender: TObject);
    procedure RBSelPicturesClick(Sender: TObject);
    procedure tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure tvGroupsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
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
    FSelPicFileTotalSize: Integer;
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
    procedure InitializePage; override;
    function  GetDataValid: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
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
    EnableWndCtl(tvGroups, rbSelGroups.Checked);
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
    View := Wiz.App.Project.CurrentViewX;
    if View=nil then
      rbAllPics.Caption := ConstVal('SWzFileOps_AllPicsInPhoa')
    else
      rbAllPics.Caption := ConstVal('SWzFileOps_AllPicsInView', [View.Name]);
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
        if not (vsDisabled in n.States) then begin
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

  procedure TfrWzPageFileOps_SelPics.InitializePage;
  begin
    inherited InitializePage;
     // Настраиваем дерево групп
    ApplyTreeSettings(tvGroups); 
    tvGroups.HintMode      := GTreeHintModeToVTHintMode(TGroupTreeHintMode(SettingValueInt(ISettingID_Browse_GT_Hints)));
    tvGroups.NodeDataSize  := SizeOf(Pointer);
    tvGroups.RootNodeCount := fMain.tvGroups.RootNodeCount;
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
        if n.CheckState=csCheckedNormal then Wiz.SelectedGroups.Add(PPhotoAlbumPicGroup(tvGroups.GetNodeData(n))^);
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

  procedure TfrWzPageFileOps_SelPics.tvGroupsBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
  begin
    fMain.tvGroupsBeforeItemErase(Sender, TargetCanvas, Node, ItemRect, ItemColor, EraseAction);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    if not (tsUpdating in Sender.TreeStates) then UpdateCountInfo;
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    fMain.tvGroupsFreeNode(Sender, Node);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    fMain.tvGroupsGetHint(Sender, Node, Column, TextType, CellText);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    fMain.tvGroupsGetImageIndex(Sender, Node, Kind, Column, Ghosted, ImageIndex);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    fMain.tvGroupsGetText(Sender, Node, Column, TextType, CellText);
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var Group: IPhotoAlbumPicGroup;
  begin
    fMain.tvGroupsInitNode(Sender, ParentNode, Node, InitialStates);
     // Получаем группу, с которой ассоциирован узел
    Group := PPhotoAlbumPicGroup(Sender.GetNodeData(Node))^;
     // Запрещаем узел, если у него нет изображений
    if Group.Pics.Count=0 then Include(InitialStates, ivsDisabled);
     // Настраиваем птицу узла
    Node.CheckType := ctCheckBox;
    Node.CheckState := aCheckStates[not (ivsDisabled in InitialStates) and (TdFileOpsWizard(StorageForm).SelectedGroups.IndexOfID(Group.ID)>=0)];
  end;

  procedure TfrWzPageFileOps_SelPics.tvGroupsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  begin
    fMain.tvGroupsPaintText(Sender, TargetCanvas, Node, Column, TextType);
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
            GroupPics.Add(PPhotoAlbumPicGroup(tvGroups.GetNodeData(n))^.Pics, True);
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
      lCountInfo.Caption := ConstVal('SWzFileOps_PicGroupSelectedCount', [FSelPicCount, FSelGroupCount, HumanReadableSize(FSelPicFileTotalSize)]);
      StatusChanged;
    finally
      StopWait;
    end;
  end;

  procedure TfrWzPageFileOps_SelPics.UpdateCountInfoNotify(Sender: TObject);
  begin
    UpdateCountInfo;
  end;

end.

