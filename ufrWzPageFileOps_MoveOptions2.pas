unit ufrWzPageFileOps_MoveOptions2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars,
  phWizard, DTLangTools, StdCtrls;

type
  TfrWzPageFileOps_MoveOptions2 = class(TWizardPage)
    lNoOriginalFileMode: TLabel;
    cbNoOriginalFileMode: TComboBox;
    cbDeleteOriginal: TCheckBox;
    cbDeleteToRecycleBin: TCheckBox;
    dtlsMain: TDTLanguageSwitcher;
    cbUseCDOptions: TCheckBox;
    lOverwriteMode: TLabel;
    cbOverwriteMode: TComboBox;
    procedure AdjustOptionsNotify(Sender: TObject);
  private
     // Настраивает [вторичные] контролы опций
    procedure AdjustOptionControls;
  protected
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard;

  procedure TfrWzPageFileOps_MoveOptions2.AdjustOptionControls;
  begin
    cbDeleteToRecycleBin.Enabled := cbDeleteOriginal.Enabled and cbDeleteOriginal.Checked;
  end;

  procedure TfrWzPageFileOps_MoveOptions2.AdjustOptionsNotify(Sender: TObject);
  begin
    AdjustOptionControls;
    StatusChanged;
  end;

  procedure TfrWzPageFileOps_MoveOptions2.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var
    Wiz: TdFileOpsWizard;
    bMoveOp: Boolean;
  begin
    inherited BeforeDisplay(ChangeMethod);
    Wiz := TdFileOpsWizard(StorageForm);
     // Настраиваем опции
    cbNoOriginalFileMode.ItemIndex := Byte(Wiz.MoveFile_NoOriginalMode);
    cbDeleteOriginal.Checked       := Wiz.MoveFile_DeleteOriginal;
    cbDeleteToRecycleBin.Checked   := Wiz.MoveFile_DeleteToRecycleBin;
    cbOverwriteMode.ItemIndex      := Byte(Wiz.MoveFile_OverwriteMode);
    cbUseCDOptions.Checked         := Wiz.MoveFile_UseCDOptions;
     // Контролы "Что делать, если нет исходного файла" и "Стирать исходные файлы" доступны только при операции перемещения
    bMoveOp := Wiz.FileOpKind=fokMoveFiles;
    lNoOriginalFileMode.Enabled := bMoveOp;
    EnableWndCtl(cbNoOriginalFileMode, bMoveOp);
    cbDeleteOriginal.Enabled := bMoveOp;
    AdjustOptionControls;
  end;

  function TfrWzPageFileOps_MoveOptions2.NextPage: Boolean;
  var Wiz: TdFileOpsWizard;
  begin
    Result := inherited NextPage;
    if Result then begin
      Wiz := TdFileOpsWizard(StorageForm);
       // Сохраняем опции
      Wiz.MoveFile_OverwriteMode := TFileOpMoveFileOverwriteMode(cbOverwriteMode.ItemIndex);
      Wiz.MoveFile_UseCDOptions := cbUseCDOptions.Checked;
       // Если вид операции - "Переместить файлы", сохраняем специфичные для неё опции
      if Wiz.FileOpKind=fokMoveFiles then begin
        Wiz.MoveFile_NoOriginalMode     := TFileOpMoveFileNoOriginalMode(cbNoOriginalFileMode.ItemIndex);
        Wiz.MoveFile_DeleteOriginal     := cbDeleteOriginal.Checked;
        Wiz.MoveFile_DeleteToRecycleBin := cbDeleteToRecycleBin.Checked;
      end;
    end;
  end;

end.
