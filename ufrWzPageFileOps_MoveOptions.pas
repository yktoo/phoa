//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_MoveOptions.pas,v 1.4 2004-08-30 14:10:08 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit ufrWzPageFileOps_MoveOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars,
  phWizard, DTLangTools, StdCtrls, Mask;

type
  TfrWzPageFileOps_MoveOptions = class(TWizardPage)
    gbFileArranging: TGroupBox;
    rbPutFlatly: TRadioButton;
    rbMaintainFolderLayout: TRadioButton;
    rbMaintainGroupLayout: TRadioButton;
    cbAllowDuplicating: TCheckBox;
    lBaseFolder: TLabel;
    cbBaseFolder: TComboBox;
    lBaseGroup: TLabel;
    cbBaseGroup: TComboBox;
    lReplaceChar: TLabel;
    eReplaceChar: TMaskEdit;
    dklcMain: TDKLanguageController;
    procedure RBFileArrangingClick(Sender: TObject);
    procedure AdjustOptionsNotify(Sender: TObject);
    procedure eReplaceCharKeyPress(Sender: TObject; var Key: Char);
  private
     // Настраивает [вторичные] контролы опций
    procedure AdjustOptionControls;
     // Составляет список возможных базовых путей исходных файлов и помещает его в cbBaseFolder, если этого ещё не
     //   сделано
    procedure MakeBaseFoldersLoaded;
     // Составляет список возможных базовых групп исходных изображений и помещает его в cbBaseGroup, если этого ещё не
     //   сделано
    procedure MakeBaseGroupsLoaded;
     // Prop handlers
    function  GetCurMoveFileArranging: TFileOpMoveFileArranging;
    procedure SetCurMoveFileArranging(Value: TFileOpMoveFileArranging);
     // Props
     // -- Текущий режим выбора файлов
    property CurMoveFileArranging: TFileOpMoveFileArranging read GetCurMoveFileArranging write SetCurMoveFileArranging; 
  protected
    function  GetDataValid: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard, phObj;

  procedure TfrWzPageFileOps_MoveOptions.AdjustOptionControls;
  var bMFL, bMGL: Boolean;
  begin
     // MaintainFolderLayout
    bMFL := rbMaintainFolderLayout.Checked;
    lBaseFolder.Enabled := bMFL;
    EnableWndCtl(cbBaseFolder, bMFL);
    if bMFL then MakeBaseFoldersLoaded;
     // MaintainGroupLayout
    bMGL := rbMaintainGroupLayout.Checked;
    lBaseGroup.Enabled := bMGL;
    EnableWndCtl(cbBaseGroup, bMGL);
    cbAllowDuplicating.Enabled := bMGL;
    lReplaceChar.Enabled := bMGL;
    EnableWndCtl(eReplaceChar, bMGL);
    if bMGL then MakeBaseGroupsLoaded;
  end;

  procedure TfrWzPageFileOps_MoveOptions.AdjustOptionsNotify(Sender: TObject);
  begin
    AdjustOptionControls;
    StatusChanged;
  end;

  procedure TfrWzPageFileOps_MoveOptions.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var Wiz: TdFileOpsWizard;
  begin
    inherited BeforeDisplay(ChangeMethod);
    cbBaseFolder.Items.Clear;
    cbBaseGroup.Items.Clear;
    Wiz := TdFileOpsWizard(StorageForm);
     // Настраиваем опции
    CurMoveFileArranging         := Wiz.MoveFile_Arranging;
    cbAllowDuplicating.Checked   := Wiz.MoveFile_AllowDuplicating;
    eReplaceChar.Text            := Wiz.MoveFile_ReplaceChar;
    AdjustOptionControls;
  end;

  procedure TfrWzPageFileOps_MoveOptions.eReplaceCharKeyPress(Sender: TObject; var Key: Char);
  begin
     // Запрещаем вводить символы, недопустимые для пути
    if StrScan(SInvalidPathChars, Key)<>nil then Key := #0;
  end;

  function TfrWzPageFileOps_MoveOptions.GetCurMoveFileArranging: TFileOpMoveFileArranging;
  begin
    if rbPutFlatly.Checked                 then Result := fomfaPutFlatly
    else if rbMaintainFolderLayout.Checked then Result := fomfaMaintainFolderLayout
    else if rbMaintainGroupLayout.Checked  then Result := fomfaMaintainGroupLayout
    else                                        Result := TFileOpMoveFileArranging(-1);
  end;

  function TfrWzPageFileOps_MoveOptions.GetDataValid: Boolean;
  begin
    Result := False;
    case CurMoveFileArranging of
      fomfaPutFlatly:            Result := True;
      fomfaMaintainFolderLayout: Result := cbBaseFolder.ItemIndex>=0;
      fomfaMaintainGroupLayout:  Result := (cbBaseGroup.ItemIndex>=0) and (eReplaceChar.Text<>'');
    end;
  end;

  procedure TfrWzPageFileOps_MoveOptions.MakeBaseFoldersLoaded;
  var
    sPath, sPicPath: String;
    i, iBSlashPos: Integer;
  begin
    if cbBaseFolder.Items.Count=0 then begin
       // Ищем самый длинный общий путь
      sPath := '';
      for i := 0 to TdFileOpsWizard(StorageForm).SelectedPics.Count-1 do begin
        sPicPath := ExtractFilePath(TdFileOpsWizard(StorageForm).SelectedPics[i].PicFileName);
        if i=0 then sPath := sPicPath else sPath := LongestCommonPart(sPath, sPicPath);
         // Если строка уже пустая, выходим
        if sPath='' then Break;
      end;
       // Выделяем части по каждому каталогу
      if (Length(sPath)>3) and (sPath[Length(sPath)]='\') then SetLength(sPath, Length(sPath)-1);
      while sPath<>'' do begin
        cbBaseFolder.Items.Add(sPath);
        if Length(sPath)<=3 then Break;
        iBSlashPos := LastDelimiter('\', sPath);
        if iBSlashPos=0 then Break;
        Delete(sPath, iBSlashPos+iif((iBSlashPos=3) and (sPath[2]=':'), 1, 0), MaxInt);
      end;
       // Добавляем путь "(нет)"
      cbBaseFolder.Items.Add(ConstVal('SNone'));
       // Выбираем нужный пункт
      i := cbBaseFolder.Items.IndexOf(TdFileOpsWizard(StorageForm).MoveFile_BasePath);
      if i<0 then i:= 0;
      cbBaseFolder.ItemIndex := iif(i<0, 0, i);
    end;
  end;

  procedure TfrWzPageFileOps_MoveOptions.MakeBaseGroupsLoaded;
  var
    LPath, LGroupPath: TList;
    Group: TPhoaGroup;
    i: Integer;
    Wiz: TdFileOpsWizard;
    sRootGroupName: String;

     // Заполняет List группами, представляющими собой путь к Group
    procedure FillPath(Group: TPhoaGroup; List: TList);
    begin
      List.Clear;
      repeat
        List.Insert(0, Group);
        Group := Group.Owner;
      until Group=nil;
    end;

     // Оставляет в CommonList ту часть пути, которая совпадает с началом пути в CurList
    procedure FindCommonPath(CommonList, CurList: TList);
    var i: Integer;
    begin
      i := 0;
      while (i<CommonList.Count) and (i<CurList.Count) and (CommonList[i]=CurList[i]) do Inc(i);
      CommonList.Count := i;
    end;

  begin
    if cbBaseGroup.Items.Count=0 then begin
      Wiz := TdFileOpsWizard(StorageForm);
      LPath := TList.Create;
      try
         // Ищем и заносим в LPath самый длинный общий путь
        case Wiz.SelPicMode of
           // -- Выбранные во вьюере изображения - заносим путь к текущей выбранной группе
          fospmSelPics: FillPath(Wiz.ViewerSelGroup, LPath);
           // -- Все изображения - заносим только сам фотоальбом/представление
          fospmAll: if Wiz.ViewerCurView=nil then LPath.Add(Wiz.PhoA.RootGroup) else LPath.Add(Wiz.ViewerCurView.RootGroup);
           // -- Выбранные группы - проходим по всем выбранным группам, выделяя общий путь
          else {fospmSelGroups} begin
            LGroupPath := TList.Create;
            try
              for i := 0 to Wiz.SelectedGroupCount-1 do begin
                FillPath(Wiz.SelectedGroups[i], LGroupPath);
                if i=0 then LPath.Assign(LGroupPath) else FindCommonPath(LPath, LGroupPath);
                 // Если путь уже пустой (или там единственный узел - узел фотоальбома), выходим
                if LPath.Count<=1 then Break;
              end;
            finally
              LGroupPath.Free;
            end;
          end;
        end;
         // Выделяем части пути, пишем в Items[] - путь группы, в Items.Objects[] - ссылку на группу
        if Wiz.ViewerCurView=nil then sRootGroupName := ConstVal('SPhotoAlbumNode') else sRootGroupName := Wiz.ViewerCurView.Name;
        for i := LPath.Count-1 downto 0 do begin
          Group := TPhoaGroup(LPath[i]);
          cbBaseGroup.Items.AddObject(Group.Path[sRootGroupName], Group);
        end;
      finally
        LPath.Free;
      end;
       // Выбираем нужный пункт
      i := cbBaseGroup.Items.IndexOfObject(Wiz.MoveFile_BaseGroup);
      cbBaseGroup.ItemIndex := iif(i<0, 0, i);
    end;
  end;

  function TfrWzPageFileOps_MoveOptions.NextPage: Boolean;
  var
    mfa: TFileOpMoveFileArranging;
    Wiz: TdFileOpsWizard;
  begin
    Result := inherited NextPage;
    if Result then begin
       // Сохраняем режим размещения файлов
      mfa := CurMoveFileArranging;
      Wiz := TdFileOpsWizard(StorageForm);
      Wiz.MoveFile_Arranging := mfa;
       // Сохраняем прочие опции
      case mfa of
        fomfaMaintainFolderLayout:
          Wiz.MoveFile_BasePath := iif(cbBaseFolder.ItemIndex=cbBaseFolder.Items.Count-1, '', cbBaseFolder.Text);
        fomfaMaintainGroupLayout: begin
          Wiz.MoveFile_BaseGroup        := TPhoaGroup(cbBaseGroup.Items.Objects[cbBaseGroup.ItemIndex]);
          Wiz.MoveFile_AllowDuplicating := cbAllowDuplicating.Checked;
          Wiz.MoveFile_ReplaceChar      := eReplaceChar.Text[1];
        end;
      end;
    end;
  end;

  procedure TfrWzPageFileOps_MoveOptions.RBFileArrangingClick(Sender: TObject);
  begin
    AdjustOptionControls;
  end;

  procedure TfrWzPageFileOps_MoveOptions.SetCurMoveFileArranging(Value: TFileOpMoveFileArranging);
  begin
    rbPutFlatly.Checked            := Value=fomfaPutFlatly;
    rbMaintainFolderLayout.Checked := Value=fomfaMaintainFolderLayout;
    rbMaintainGroupLayout.Checked  := Value=fomfaMaintainGroupLayout;
  end;

end.
