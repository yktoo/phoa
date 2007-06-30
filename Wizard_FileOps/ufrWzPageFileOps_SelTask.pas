//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_SelTask.pas,v 1.3 2007-06-30 10:36:21 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_SelTask;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, ConsVars,
  Dialogs, phWizard, StdCtrls, DKLang, TntStdCtrls;

type
  TfrWzPageFileOps_SelTask = class(TWizardPage)
    dklcMain: TDKLanguageController;
    lCopyFiles: TTntLabel;
    lDeleteFiles: TTntLabel;
    lMoveFiles: TTntLabel;
    lNBUndoable: TTntLabel;
    lRebuildThumbs: TTntLabel;
    lRepairFileLinks: TTntLabel;
    rbCopyFiles: TTntRadioButton;
    rbDeleteFiles: TTntRadioButton;
    rbMoveFiles: TTntRadioButton;
    rbRebuildThumbs: TTntRadioButton;
    rbRepairFileLinks: TTntRadioButton;
  private
     // Массив соответствия радиокнопок видам операций
    FKindRadioButtons: Array[TFileOperationKind] of TTntRadioButton;
  protected
    function  GetDataValid: Boolean; override;
    function  NextPage: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure DoCreate; override;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard;

  procedure TfrWzPageFileOps_SelTask.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var fok: TFileOperationKind;
  begin
    inherited BeforeDisplay(ChangeMethod);
     // Настраиваем выбранность радиокнопок
    for fok := Low(fok) to High(fok) do FKindRadioButtons[fok].Checked := TdFileOpsWizard(StorageForm).FileOpKind=fok;
  end;

  procedure TfrWzPageFileOps_SelTask.DoCreate;
  var fok: TFileOperationKind;
  begin
    inherited DoCreate;
     // Инициализируем массив радиокнопок
    FKindRadioButtons[fokCopyFiles]       := rbCopyFiles;
    FKindRadioButtons[fokMoveFiles]       := rbMoveFiles;
    FKindRadioButtons[fokDeleteFiles]     := rbDeleteFiles;
    FKindRadioButtons[fokRebuildThumbs]   := rbRebuildThumbs;
    FKindRadioButtons[fokRepairFileLinks] := rbRepairFileLinks;
     // Выделяем радиокнопки жирным
    for fok := Low(fok) to High(fok) do
      with FKindRadioButtons[fok].Font do Style := Style+[fsBold];
     // Выделяем lNBUndoable жирным
    with lNBUndoable.Font do Style := Style+[fsBold];
  end;

  function TfrWzPageFileOps_SelTask.GetDataValid: Boolean;
  var fok: TFileOperationKind;
  begin
    Result := False;
     // Можно идти дальше, если выбрана операция радиокнопкой
    for fok := Low(fok) to High(fok) do
      if FKindRadioButtons[fok].Checked then begin
        Result := True;
        Break;
      end;
  end;

  function TfrWzPageFileOps_SelTask.NextPage: Boolean;
  var fok: TFileOperationKind;
  begin
    Result := True;
     // Сохраняем выбранность радиокнопок
    for fok := Low(fok) to High(fok) do
      if FKindRadioButtons[fok].Checked then begin
        TdFileOpsWizard(StorageForm).FileOpKind := fok;
        Break;
      end;
  end;

end.

