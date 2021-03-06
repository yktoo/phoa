//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_RepairOptions.pas,v 1.2 2007-06-28 18:41:59 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_RepairOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ConsVars,
  Dialogs, phWizard, StdCtrls, ExtCtrls, Mask, DKLang, TntStdCtrls;

type
  TfrWzPageFileOps_RepairOptions = class(TWizardPage)
    gbMatchFindingOptions: TTntGroupBox;
    rbSearchByName: TTntRadioButton;
    rbSearchBySize: TTntRadioButton;
    rbSearchByNameSize: TTntRadioButton;
    cbLookSubfolders: TTntCheckBox;
    gbAlreadyInUseMode: TTntGroupBox;
    rbSkipFilesInUse: TTntRadioButton;
    rbRelinkFilesInUse: TTntRadioButton;
    dklcMain: TDKLanguageController;
  private
  protected
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard;

  procedure TfrWzPageFileOps_RepairOptions.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var Wiz: TdFileOpsWizard;
  begin
    inherited BeforeDisplay(ChangeMethod);
     // ����������� �����
    Wiz := TdFileOpsWizard(StorageForm);
    rbSearchByName.Checked     := Wiz.Repair_MatchFlags=[formfName];
    rbSearchBySize.Checked     := Wiz.Repair_MatchFlags=[formfSize];
    rbSearchByNameSize.Checked := Wiz.Repair_MatchFlags=[formfName, formfSize];
    cbLookSubfolders.Checked   := Wiz.Repair_LookSubfolders;
    rbSkipFilesInUse.Checked   := not Wiz.Repair_RelinkFilesInUse;
    rbRelinkFilesInUse.Checked := Wiz.Repair_RelinkFilesInUse;
  end;

  function TfrWzPageFileOps_RepairOptions.NextPage: Boolean;
  var
    Wiz: TdFileOpsWizard;
    mf: TFileOpRepairMatchFlags;
  begin
    Result := inherited NextPage;
    if Result then begin
       // ��������� �����
      Wiz := TdFileOpsWizard(StorageForm);
      mf := [];
      if rbSearchByName.Checked or rbSearchByNameSize.Checked then Include(mf, formfName);
      if rbSearchBySize.Checked or rbSearchByNameSize.Checked then Include(mf, formfSize);
      Wiz.Repair_MatchFlags       := mf;
      Wiz.Repair_LookSubfolders   := cbLookSubfolders.Checked;
      Wiz.Repair_RelinkFilesInUse := rbRelinkFilesInUse.Checked;
    end;
  end;

end.

