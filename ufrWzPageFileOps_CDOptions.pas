//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_CDOptions.pas,v 1.6 2004-09-11 17:52:36 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_CDOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ConsVars,
  Dialogs, phWizard, StdCtrls, ExtCtrls, Mask, DKLang;

type
  TfrWzPageFileOps_CDOptions = class(TWizardPage)
    cbCopyExecutable: TCheckBox;
    cbCreatePhoa: TCheckBox;
    ePhoaFileName: TEdit;
    cbCreateAutorun: TCheckBox;
    lMediaLabel: TLabel;
    eMediaLabel: TEdit;
    lPhoaDesc: TLabel;
    mPhoaDesc: TMemo;
    cbIncludeViews: TCheckBox;
    cbCopyIniSettings: TCheckBox;
    dklcMain: TDKLanguageController;
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
  var bCreatePhoa, bCreateAutorun: Boolean;
  begin
     // Create photo album
    bCreatePhoa := cbCreatePhoa.Checked;
    EnableWndCtl(ePhoaFileName, bCreatePhoa);
    EnableWndCtl(mPhoaDesc,     bCreatePhoa);
    lPhoaDesc.Enabled      := bCreatePhoa;
    cbIncludeViews.Enabled := bCreatePhoa;
     // Copy the executable
    cbCopyIniSettings.Enabled := cbCopyExecutable.Checked;
     // Create autorun.inf
    cbCreateAutorun.Enabled := bCreatePhoa;
    bCreateAutorun := cbCreateAutorun.Enabled and cbCreateAutorun.Checked;
    lMediaLabel.Enabled := bCreateAutorun;
    EnableWndCtl(eMediaLabel, bCreateAutorun);
  end;

  procedure TfrWzPageFileOps_CDOptions.AdjustOptionsNotify(Sender: TObject);
  begin
    AdjustOptionControls;
    StatusChanged;
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
      Wiz.CDOpt_CreateAutorun   := cbCreateAutorun.Checked;
      Wiz.CDOpt_MediaLabel      := eMediaLabel.Text;
    end;
  end;

end.

