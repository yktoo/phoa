//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_DelOptions.pas,v 1.5 2004-09-02 14:20:38 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit ufrWzPageFileOps_DelOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ConsVars,
  Dialogs, phWizard, StdCtrls, ExtCtrls, Mask, DKLang;

type
  TfrWzPageFileOps_DelOptions = class(TWizardPage)
    cbDeleteToRecycleBin: TCheckBox;
    dklcMain: TDKLanguageController;
  protected
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard;

  procedure TfrWzPageFileOps_DelOptions.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
     // Настраиваем опции
    cbDeleteToRecycleBin.Checked := TdFileOpsWizard(StorageForm).DelFile_DeleteToRecycleBin;
  end;

  function TfrWzPageFileOps_DelOptions.NextPage: Boolean;
  begin
    Result := inherited NextPage;
    if Result then begin
       // Сохраняем опции
      TdFileOpsWizard(StorageForm).DelFile_DeleteToRecycleBin := cbDeleteToRecycleBin.Checked;
    end;
  end;

end.

