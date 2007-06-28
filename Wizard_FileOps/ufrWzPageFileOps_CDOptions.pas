//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_CDOptions.pas,v 1.2 2007-06-28 18:41:59 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_CDOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ConsVars,
  Dialogs, phWizard, StdCtrls, ExtCtrls, Mask, DKLang, TntStdCtrls;

type
  TfrWzPageFileOps_CDOptions = class(TWizardPage)
    cbCopyExecutable: TTntCheckBox;
    cbCopyIniSettings: TTntCheckBox;
    cbCopyLangFile: TTntCheckBox;
    cbCreateAutorun: TTntCheckBox;
    cbCreatePhoa: TTntCheckBox;
    cbIncludeViews: TTntCheckBox;
    dklcMain: TDKLanguageController;
    eMediaLabel: TTntEdit;
    ePhoaFileName: TTntEdit;
    lMediaLabel: TTntLabel;
    lPhoaDesc: TTntLabel;
    mPhoaDesc: TTntMemo;
    procedure AdjustOptionsNotify(Sender: TObject);
  private
     // Настраивает [вторичные] контролы опций
    procedure AdjustOptionControls;
  protected
    function  GetDataValid: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard, phObj;

  procedure TfrWzPageFileOps_CDOptions.AdjustOptionControls;
  var bCreatePhoa: Boolean;
  begin
     // Create photo album
    bCreatePhoa := cbCreatePhoa.Checked;
    EnableControls(bCreatePhoa, [ePhoaFileName, lPhoaDesc, mPhoaDesc]);
    cbIncludeViews.Enabled := bCreatePhoa;
     // Copy the executable
    cbCopyIniSettings.Enabled := cbCopyExecutable.Checked;
    cbCopyLangFile.Enabled    := cbCopyExecutable.Checked and (LangManager.LanguageID<>LangManager.DefaultLanguageID);
     // Create autorun.inf
    cbCreateAutorun.Enabled := bCreatePhoa;
    EnableControls(bCreatePhoa and cbCreateAutorun.Checked, [lMediaLabel, eMediaLabel]);
  end;

  procedure TfrWzPageFileOps_CDOptions.AdjustOptionsNotify(Sender: TObject);
  begin
    AdjustOptionControls;
    StateChanged;
  end;

  procedure TfrWzPageFileOps_CDOptions.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var Wiz: TdFileOpsWizard;
  begin
    inherited BeforeDisplay(ChangeMethod);
    Wiz := TdFileOpsWizard(StorageForm);
     // Настраиваем опции
    cbCreatePhoa.Checked      := Wiz.CDOpt_CreatePhoa;
    ePhoaFileName.Text        := Wiz.CDOpt_PhoaFileName;
    mPhoaDesc.Text            := Wiz.CDOpt_PhoaDesc;
    cbIncludeViews.Checked    := Wiz.CDOpt_IncludeViews;
    cbCopyExecutable.Checked  := Wiz.CDOpt_CopyExecutable;
    cbCopyIniSettings.Checked := Wiz.CDOpt_CopyIniSettings;
    cbCopyLangFile.Checked    := Wiz.CDOpt_CopyLangFile;
    cbCreateAutorun.Checked   := Wiz.CDOpt_CreateAutorun;
    eMediaLabel.Text          := Wiz.CDOpt_MediaLabel;
    AdjustOptionControls;
  end;

  function TfrWzPageFileOps_CDOptions.GetDataValid: Boolean;
  begin
    Result := True;
    if cbCreatePhoa.Checked then Result := Trim(ePhoaFileName.Text)<>'';
  end;

  function TfrWzPageFileOps_CDOptions.NextPage: Boolean;
  var Wiz: TdFileOpsWizard;
  begin
    Result := inherited NextPage;
    if Result then begin
       // Сохраняем опции
      Wiz := TdFileOpsWizard(StorageForm);
      Wiz.CDOpt_CreatePhoa      := cbCreatePhoa.Checked;
      Wiz.CDOpt_PhoaFileName    := ePhoaFileName.Text;
      Wiz.CDOpt_PhoaDesc        := mPhoaDesc.Text;
      Wiz.CDOpt_IncludeViews    := cbIncludeViews.Checked;
      Wiz.CDOpt_CopyExecutable  := cbCopyExecutable.Checked;
      Wiz.CDOpt_CopyIniSettings := cbCopyIniSettings.Checked;
      Wiz.CDOpt_CopyLangFile    := cbCopyLangFile.Checked;
      Wiz.CDOpt_CreateAutorun   := cbCreateAutorun.Checked;
      Wiz.CDOpt_MediaLabel      := eMediaLabel.Text;
    end;
  end;

end.

