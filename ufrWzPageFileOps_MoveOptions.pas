//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_MoveOptions.pas,v 1.14 2005-02-12 15:36:37 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_MoveOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars,
  phWizard, StdCtrls, Mask, DKLang;

type
  TfrWzPageFileOps_MoveOptions = class(TWizardPage)
    cbAllowDuplicating: TCheckBox;
    cbBaseFolder: TComboBox;
    cbBaseGroup: TComboBox;
    cbRenameFiles: TCheckBox;
    dklcMain: TDKLanguageController;
    eFileNameFormat: TEdit;
    eReplaceChar: TMaskEdit;
    gbFileArranging: TGroupBox;
    gbFileNaming: TGroupBox;
    lBaseFolder: TLabel;
    lBaseGroup: TLabel;
    lReplaceChar: TLabel;
    rbMaintainFolderLayout: TRadioButton;
    rbMaintainGroupLayout: TRadioButton;
    rbPutFlatly: TRadioButton;
    procedure AdjustOptionsNotify(Sender: TObject);
    procedure eReplaceCharKeyPress(Sender: TObject; var Key: Char);
  private
     // ����������� [���������] �������� �����
    procedure AdjustOptionControls;
     // ���������� ������ ��������� ������� ����� �������� ������ � �������� ��� � cbBaseFolder, ���� ����� ��� ��
     //   �������
    procedure MakeBaseFoldersLoaded;
     // ���������� ������ ��������� ������� ����� �������� ����������� � �������� ��� � cbBaseGroup, ���� ����� ��� ��
     //   �������
    procedure MakeBaseGroupsLoaded;
     // ���������� True, ���� eFileNameFormat.Text �������� ���������� ������ ����� �����. � ��������� ������ ����������
     //   ��������� �� ������ � ���������� False  
    function  ValidateFileNameFormat: Boolean;
     // Prop handlers
    function  GetCurMoveFileArranging: TFileOpMoveFileArranging;
    procedure SetCurMoveFileArranging(Value: TFileOpMoveFileArranging);
     // Props
     // -- ������� ����� ������ ������
    property CurMoveFileArranging: TFileOpMoveFileArranging read GetCurMoveFileArranging write SetCurMoveFileArranging; 
  protected
    function  GetDataValid: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard, udMsgBox;

  procedure TfrWzPageFileOps_MoveOptions.AdjustOptionControls;
  var bMFL, bMGL: Boolean;
  begin
     // MaintainFolderLayout
    bMFL := rbMaintainFolderLayout.Checked;
    EnableControls(bMFL, [lBaseFolder, cbBaseFolder]);
    if bMFL then MakeBaseFoldersLoaded;
     // MaintainGroupLayout
    bMGL := rbMaintainGroupLayout.Checked;
    EnableControls(bMGL, [lBaseGroup, cbBaseGroup]);
    cbAllowDuplicating.Enabled := bMGL;
    if bMGL then MakeBaseGroupsLoaded;
     // Rename files
    EnableControl(cbRenameFiles.Checked, eFileNameFormat);
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
     // ����������� �����
    CurMoveFileArranging       := Wiz.MoveFile_Arranging;
    cbAllowDuplicating.Checked := Wiz.MoveFile_AllowDuplicating;
    eReplaceChar.Text          := Wiz.MoveFile_ReplaceChar;
    cbRenameFiles.Checked      := Wiz.MoveFile_RenameFiles;
    eFileNameFormat.Text       := Wiz.MoveFile_FileNameFormat;
     // ��������� ��������� ���������
    AdjustOptionControls;
  end;

  procedure TfrWzPageFileOps_MoveOptions.eReplaceCharKeyPress(Sender: TObject; var Key: Char);
  begin
     // ��������� ������� �������, ������������ ��� ����
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
     // File arranging
    case CurMoveFileArranging of
      fomfaPutFlatly:            Result := True;
      fomfaMaintainFolderLayout: Result := cbBaseFolder.ItemIndex>=0;
      fomfaMaintainGroupLayout:  Result := cbBaseGroup.ItemIndex>=0;
      else                       Result := False;
    end;
     // File naming
    if Result and cbRenameFiles.Checked then Result := Trim(eFileNameFormat.Text)<>'';
    Result := Result and (eReplaceChar.Text<>'');
  end;

  procedure TfrWzPageFileOps_MoveOptions.MakeBaseFoldersLoaded;
  var
    sPath, sPicPath: String;
    i, iBSlashPos: Integer;
  begin
    if cbBaseFolder.Items.Count=0 then begin
       // ���� ����� ������� ����� ����
      sPath := '';
      for i := 0 to TdFileOpsWizard(StorageForm).SelectedPics.Count-1 do begin
        sPicPath := ExtractFilePath(TdFileOpsWizard(StorageForm).SelectedPics[i].FileName);
        if i=0 then sPath := sPicPath else sPath := LongestCommonPart(sPath, sPicPath);
         // ���� ������ ��� ������, �������
        if sPath='' then Break;
      end;
       // �������� ����� �� ������� ��������
      if (Length(sPath)>3) and (sPath[Length(sPath)]='\') then SetLength(sPath, Length(sPath)-1);
      while sPath<>'' do begin
        cbBaseFolder.Items.Add(sPath);
        if Length(sPath)<=3 then Break;
        iBSlashPos := LastDelimiter('\', sPath);
        if iBSlashPos=0 then Break;
        Delete(sPath, iBSlashPos+iif((iBSlashPos=3) and (sPath[2]=':'), 1, 0), MaxInt);
      end;
       // ��������� ���� "(���)"
      cbBaseFolder.Items.Add(ConstVal('SNone'));
       // �������� ������ �����
      i := cbBaseFolder.Items.IndexOf(TdFileOpsWizard(StorageForm).MoveFile_BasePath);
      if i<0 then i:= 0;
      cbBaseFolder.ItemIndex := iif(i<0, 0, i);
    end;
  end;

  procedure TfrWzPageFileOps_MoveOptions.MakeBaseGroupsLoaded;
  var
    LPath, LGroupPath: IPhotoAlbumPicGroupList;
    Group: IPhotoAlbumPicGroup;
    i: Integer;
    Wiz: TdFileOpsWizard;
    sRootGroupName: String;

     // ��������� List ��������, ��������������� ����� ���� � Group
    procedure FillPath(Group: IPhotoAlbumPicGroup; List: IPhotoAlbumPicGroupList);
    begin
      List.Clear;
      repeat
        List.Insert(0, Group);
        Group := Group.OwnerX;
      until Group=nil;
    end;

     // ��������� � CommonList �� ����� ����, ������� ��������� � ������� ���� � CurList
    procedure FindCommonPath(CommonList, CurList: IPhotoAlbumPicGroupList);
    var i: Integer;
    begin
      i := 0;
      while (i<CommonList.Count) and (i<CurList.Count) and (CommonList[i]=CurList[i]) do Inc(i);
      for i := CommonList.Count-1 downto i do CommonList.Delete(i);
    end;

  begin
    if cbBaseGroup.Items.Count=0 then begin
      Wiz := TdFileOpsWizard(StorageForm);
      LPath := NewPhotoAlbumPicGroupList(nil);
       // ���� � ������� � LPath ����� ������� ����� ����
      case Wiz.SelPicMode of
         // -- ��������� �� ������ ����������� - ������� ���� � ������� ��������� ������
        fospmSelPics: FillPath(Wiz.App.CurGroup, LPath);
         // -- ��� ����������� - ������� ������ ��� ����������/�������������
        fospmAll: LPath.Add(Wiz.App.Project.ViewRootGroup);
         // -- ��������� ������ - �������� �� ���� ��������� �������, ������� ����� ����
        else {fospmSelGroups} begin
          LGroupPath := NewPhotoAlbumPicGroupList(nil);
          for i := 0 to Wiz.SelectedGroups.Count-1 do begin
            FillPath(Wiz.SelectedGroups[i], LGroupPath);
            if i=0 then LPath.Assign(LGroupPath) else FindCommonPath(LPath, LGroupPath);
             // ���� ���� ��� ������ (��� ��� ������������ ���� - ���� �����������), �������
            if LPath.Count<=1 then Break;
          end;
        end;
      end;
       // �������� ����� ����, ����� � Items[] - ���� ������, � Items.Objects[] - ������ �� ������
      if Wiz.App.Project.CurrentView=nil then sRootGroupName := ConstVal('SPhotoAlbumNode') else sRootGroupName := Wiz.App.Project.CurrentView.Name;
      for i := LPath.Count-1 downto 0 do begin
        Group := LPath[i];
        cbBaseGroup.Items.AddObject(Group.Path[sRootGroupName], Pointer(Group));
      end;
       // �������� ������ �����
      i := cbBaseGroup.Items.IndexOfObject(Pointer(Wiz.MoveFile_BaseGroup));
      cbBaseGroup.ItemIndex := iif(i<0, 0, i);
    end;
  end;

  function TfrWzPageFileOps_MoveOptions.NextPage: Boolean;
  var
    mfa: TFileOpMoveFileArranging;
    Wiz: TdFileOpsWizard;
  begin
    Result := inherited NextPage;
    if not Result then Exit;
     // Validate file name format
    if cbRenameFiles.Checked then begin
      Result := ValidateFileNameFormat;
      if not Result then Exit;
    end;
     // ��������� ����� ���������� ������
    mfa := CurMoveFileArranging;
    Wiz := TdFileOpsWizard(StorageForm);
    Wiz.MoveFile_Arranging := mfa;
     // ��������� ������ ����� File arranging
    case mfa of
      fomfaMaintainFolderLayout:
        Wiz.MoveFile_BasePath := iif(cbBaseFolder.ItemIndex=cbBaseFolder.Items.Count-1, '', cbBaseFolder.Text);
      fomfaMaintainGroupLayout: begin
        Wiz.MoveFile_BaseGroup        := IPhotoAlbumPicGroup(Pointer(cbBaseGroup.Items.Objects[cbBaseGroup.ItemIndex]));
        Wiz.MoveFile_AllowDuplicating := cbAllowDuplicating.Checked;
        Wiz.MoveFile_ReplaceChar      := eReplaceChar.Text[1];
      end;
    end;
     // ��������� ����� File naming
    Wiz.MoveFile_RenameFiles    := cbRenameFiles.Checked;
    Wiz.MoveFile_FileNameFormat := eFileNameFormat.Text;
  end;

  procedure TfrWzPageFileOps_MoveOptions.SetCurMoveFileArranging(Value: TFileOpMoveFileArranging);
  begin
    rbPutFlatly.Checked            := Value=fomfaPutFlatly;
    rbMaintainFolderLayout.Checked := Value=fomfaMaintainFolderLayout;
    rbMaintainGroupLayout.Checked  := Value=fomfaMaintainGroupLayout;
  end;

  function TfrWzPageFileOps_MoveOptions.ValidateFileNameFormat: Boolean;
  var
    s, sProp: String;
    i1, i2: Integer;
    PProp: TPicProperty;
  begin
    Result := False;
     // ������� ��������� ���������� ������������ �������� � ����� �����
    s := eFileNameFormat.Text;
    if LastDelimiter(SInvalidPathChars, s)>0 then
      PhoaError('SErrInvalidCharsInFileName')
     // ��������� ��������� � �������� ��� �������������� ����������
    else begin
      repeat
         // ��������� ������������������ �������� ������
        i1 := Pos('{', s);
        i2 := Pos('}', s);
        if (i1=0) and (i2=0) then Break;
        if (i1=0) or (i2=0) or (i1>i2) then begin
          PhoaError('SErrUnbalancedCurlyBraces');
          Exit;
        end;
         // ��������� �������� ����� �������������� ����������
        sProp := Copy(s, i1+1, i2-i1-1);
        if sProp='' then begin
          PhoaError('SErrPicPropNameMissing');
          Exit;
        end;
         // ��������� ������������ ����� �������������� ����������
        PProp := StrToPicProp(sProp, False);
        if not (PProp in [Low(PProp)..High(PProp)]) then begin
          PhoaError('SErrInvalidPicPropName', [sProp]);
          Exit;
        end;
         // ���������, ��� �� ������������ ��������, ���������� ���� � �����
        if PProp in [ppFullFileName, ppFilePath] then begin
          PhoaError('SErrCannotUsePathPropsInFormat', [sProp]);
          Exit;
        end;
         // ������� ������������ ����� ������ �� s
        Delete(s, 1, i2); 
      until s='';
      Result := True;
    end;
  end;

end.

