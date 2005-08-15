//**********************************************************************************************************************
//  $Id: ufrWzPageAddFiles_CheckFiles.pas,v 1.1 2005-08-15 11:16:09 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageAddFiles_CheckFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phObj, ConsVars,
  phWizard, ImgList, TB2Item, TBX, Menus, ActnList, ExtCtrls, VirtualTrees,
  DKLang, StdCtrls;

type
  TfrWzPageAddFiles_CheckFiles = class(TWizardPage, IPhoaWizardPage_PreviewInfo)
    aFilesCheckAll: TAction;
    aFilesInvertChecks: TAction;
    aFilesUncheckAll: TAction;
    alMain: TActionList;
    cbShowPreview: TCheckBox;
    dklcMain: TDKLanguageController;
    ilFiles: TImageList;
    ipmFilesCheckAll: TTBXItem;
    ipmFilesInvertChecks: TTBXItem;
    ipmFilesUncheckAll: TTBXItem;
    pBottom: TPanel;
    pmFiles: TTBXPopupMenu;
    tvFiles: TVirtualStringTree;
    procedure aaFilesCheckAll(Sender: TObject);
    procedure aaFilesInvertChecks(Sender: TObject);
    procedure aaFilesUncheckAll(Sender: TObject);
    procedure cbShowPreviewClick(Sender: TObject);
    procedure tvFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvFilesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvFilesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvFilesHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tvFilesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvFilesKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
  private
     // Список файлов
    FFiles: TFileList;
     // Количество выбранных файлов
    FCheckedFilesCount: Integer;
     // Вызывается при изменении количества выбранных/отмеченных файлов. Обновляет данные о количестве
    procedure UpdateFileListInfo;
     // Ставит, снимает или инвертирует отметку у всех файлов
    procedure CheckFiles(Mode: TMassCheckMode);
     // Настраивает разрешённость Actions
    procedure EnableActions;
     // IPhoaWizardPage_PreviewInfo
    procedure PreviewVisibilityChanged(bVisible: Boolean);
    function  GetCurrentFileName: String;
  protected
    function  GetDataValid: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure DoCreate; override;
  end;

implementation
{$R *.dfm}
uses phUtils, ufAddFilesWizard, Main, phSettings;

  procedure TfrWzPageAddFiles_CheckFiles.aaFilesCheckAll(Sender: TObject);
  begin
    CheckFiles(mcmAll);
  end;

  procedure TfrWzPageAddFiles_CheckFiles.aaFilesInvertChecks(Sender: TObject);
  begin
    CheckFiles(mcmInvert);
  end;

  procedure TfrWzPageAddFiles_CheckFiles.aaFilesUncheckAll(Sender: TObject);
  begin
    CheckFiles(mcmNone);
  end;

  procedure TfrWzPageAddFiles_CheckFiles.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
    if ChangeMethod in [pcmNextBtn, pcmForced] then begin
      tvFiles.RootNodeCount := FFiles.Count;
      tvFiles.ReinitChildren(nil, False);
      ilFiles.Handle := FFiles.SysImageListHandle;
      UpdateFileListInfo;
    end;
     // Настраиваем опции просмотра
    cbShowPreview.Checked := TfAddFilesWizard(StorageForm).ShowPreview;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.cbShowPreviewClick(Sender: TObject);
  begin
    TfAddFilesWizard(StorageForm).ShowPreview := cbShowPreview.Checked;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.CheckFiles(Mode: TMassCheckMode);
  var
    n: PVirtualNode;
    cs: TCheckState;
  begin
    tvFiles.BeginUpdate;
    try
      n := tvFiles.GetFirst;
      while n<>nil do begin
        case Mode of
          mcmAll:  cs := csCheckedNormal;
          mcmNone: cs := csUncheckedNormal;
          else     cs := aCheckStates[n.CheckState<>csCheckedNormal];
        end;
        tvFiles.CheckState[n] := cs;
        n := tvFiles.GetNext(n);
      end;
    finally
      tvFiles.EndUpdate;
    end;
    UpdateFileListInfo;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.DoCreate;
  begin
    inherited DoCreate;
    ApplyTreeSettings(tvFiles);
    FFiles := TfAddFilesWizard(StorageForm).FileList;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.EnableActions;
  var bFilesChecked, bFilesUnchecked: Boolean;
  begin
    bFilesChecked   := FCheckedFilesCount>0;
    bFilesUnchecked := FCheckedFilesCount<FFiles.Count;
     // Настраиваем Actions
    aFilesCheckAll.Enabled   := bFilesUnchecked;
    aFilesUncheckAll.Enabled := bFilesChecked;
    StateChanged;
  end;

  function TfrWzPageAddFiles_CheckFiles.GetCurrentFileName: String;
  var Node: PVirtualNode;
  begin
    Node := tvFiles.FocusedNode;
    if Node=nil then Result := '' else Result := FFiles.Files[Node.Index];
  end;

  function TfrWzPageAddFiles_CheckFiles.GetDataValid: Boolean;
  begin
    Result := FCheckedFilesCount>0;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.PreviewVisibilityChanged(bVisible: Boolean);
  begin
    cbShowPreview.Checked := bVisible;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.tvFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    TfAddFilesWizard(StorageForm).UpdatePreview;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.tvFilesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    if not (tsUpdating in Sender.TreeStates) then UpdateFileListInfo;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.tvFilesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if (Kind in [ikNormal, ikSelected]) and (Column=0) then ImageIndex := FFiles[Node.Index].iIconIndex;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.tvFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    p: PFileRec;
    s: String;
  begin
    p := FFiles[Node.Index];
    case Column of
      0: s := p.sName;
      1: s := p.sPath;
      2: s := HumanReadableSize(p.i64Size);
      3: s := DateTimeToStr(p.dModified, AppFormatSettings);
    end;
    CellText := PhoaAnsiToUnicode(s);
  end;

  procedure TfrWzPageAddFiles_CheckFiles.tvFilesHeaderClick(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var n: PVirtualNode;
  begin
    if (Button<>mbLeft) or (Shift*[ssShift, ssCtrl, ssAlt]<>[]) then Exit;
    tvFiles.BeginUpdate;
    try
      with Sender do begin
         // Настраиваем индикатор сортировки
        if SortColumn=Column then
          SortDirection := TSortDirection(1-Byte(SortDirection))
        else begin
          SortDirection := sdAscending;
          SortColumn := Column;
        end;
         // Сортируем список файлов
        FFiles.Sort(TFileListSortProperty(SortColumn), TPhoaSortDirection(SortDirection));
      end;
       // Переписываем птицы в дерево
      n := tvFiles.GetFirst;
      while n<>nil do begin
        tvFiles.CheckState[n] := aCheckStates[FFiles[n.Index].bChecked];
        n := tvFiles.GetNext(n);
      end;
    finally
      tvFiles.EndUpdate;
    end;
    tvFiles.Invalidate;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.tvFilesInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  begin
    Node.CheckType  := ctCheckBox;
    Node.CheckState := csCheckedNormal;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.tvFilesKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
  var n: PVirtualNode;
  begin
     // По нажатию ПРОБЕЛа переключаем птицы у всех выделенных пунктов
    if (Shift*[ssShift, ssCtrl, ssAlt]=[]) and (CharCode=VK_SPACE) then begin
      DoDefault := False;
      with tvFiles do begin
        BeginUpdate;
        try
          n := GetFirstSelected;
          while n<>nil do begin
            CheckState[n] := aCheckStates[n.CheckState<>csCheckedNormal];
            n := GetNextSelected(n);
          end;
        finally
          EndUpdate;
        end;
      end;
      UpdateFileListInfo;
    end;
  end;

  procedure TfrWzPageAddFiles_CheckFiles.UpdateFileListInfo;
  var
    n: PVirtualNode;
    bChecked: Boolean;
  begin
     // Считаем количество отмеченных файлов
    FCheckedFilesCount := 0;
    n := tvFiles.GetFirst;
    while n<>nil do begin
      bChecked := n.CheckState=csCheckedNormal;
      if bChecked then Inc(FCheckedFilesCount);
       // Копируем состояние файла в FFiles[]
      FFiles[n.Index].bChecked := bChecked;
      n := tvFiles.GetNext(n);
    end;
     // Обновляем информацию
    pBottom.Caption := ConstVal('SWzPageAddFiles_CheckFiles_Info', [FFiles.Count, FCheckedFilesCount]);
    EnableActions;
  end;

end.

