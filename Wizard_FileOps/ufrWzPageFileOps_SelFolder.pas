//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_SelFolder.pas,v 1.2 2007-06-28 18:41:59 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_SelFolder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, phWizard, StdCtrls, VirtualTrees, ExtCtrls, VirtualExplorerTree,
  DKLang, TntStdCtrls, TntExtCtrls;

type
  TfrWzPageFileOps_SelFolder = class(TWizardPage)
    bCreateFolder: TTntButton;
    dklcMain: TDKLanguageController;
    eFolderPath: TTntEdit;
    pFolderOptions: TTntPanel;
    tvFolder: TVirtualExplorerTree;
    procedure bCreateFolderClick(Sender: TObject);
    procedure tvFolderChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvFolderEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  private
     // Настраивает контролы папки
    procedure AdjustFolderControls;
  protected
    function  GetDataValid: Boolean; override;
    function  NextPage: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure DoCreate; override;
  end;

implementation
{$R *.dfm}
uses phUtils, ConsVars, udFileOpsWizard, Main, phObj, VirtualShellUtilities;

  procedure TfrWzPageFileOps_SelFolder.AdjustFolderControls;
  var NS: TNamespace;
  begin
    bCreateFolder.Enabled := tvFolder.ValidateNamespace(tvFolder.FocusedNode, NS) and NS.FileSystem;
    eFolderPath.Text := tvFolder.SelectedPath;
  end;

  procedure TfrWzPageFileOps_SelFolder.bCreateFolderClick(Sender: TObject);
  begin
    tvFolder.CreateNewFolder(tvFolder.SelectedPath);
  end;

  procedure TfrWzPageFileOps_SelFolder.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
    AdjustFolderControls;
  end;

  procedure TfrWzPageFileOps_SelFolder.DoCreate;
  begin
    inherited DoCreate;
     // Настраиваем файл-браузер
    tvFolder.Active := True;
    tvFolder.BrowseTo(TdFileOpsWizard(StorageForm).DestinationFolder, False, True, False, True);
  end;

  function TfrWzPageFileOps_SelFolder.GetDataValid: Boolean;
  begin
    Result := eFolderPath.Text<>'';
  end;

  function TfrWzPageFileOps_SelFolder.NextPage: Boolean;
  begin
    TdFileOpsWizard(StorageForm).DestinationFolder := ExcludeTrailingPathDelimiter(eFolderPath.Text);
    Result := True;
  end;

  procedure TfrWzPageFileOps_SelFolder.tvFolderChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    AdjustFolderControls;
  end;

  procedure TfrWzPageFileOps_SelFolder.tvFolderEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
  begin
     // Обновляем путь после переименования
    AdjustFolderControls; 
  end;

end.

