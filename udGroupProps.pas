unit udGroupProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, phObj,
  phDlg, StdCtrls, ExtCtrls, DKLang;

type
  TdGroupProps = class(TPhoaDialog)
    lID: TLabel;
    eID: TEdit;
    lText: TLabel;
    eText: TEdit;
    lDesc: TLabel;
    mDescription: TMemo;
  private
    FPhoA: TPhotoAlbum; 
    FGroup: TPhoaGroup;
    FUndoOperations: TPhoaOperations;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetDataValid: Boolean; override;
  end;

  function EditPicGroup(APhoA: TPhotoAlbum; AGroup: TPhoaGroup; UndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses ConsVars, phUtils, Main;

  function EditPicGroup(APhoA: TPhotoAlbum; AGroup: TPhoaGroup; UndoOperations: TPhoaOperations): Boolean;
  begin
    with TdGroupProps.Create(Application) do
      try
        FPhoA           := APhoA;
        FGroup          := AGroup;
        FUndoOperations := UndoOperations;
        Result := Execute;
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdGroupProps
   //===================================================================================================================

  procedure TdGroupProps.ButtonClick_OK;
  var Operation: TPhoaOperation;
  begin
    Operation := nil;
    fMain.BeginOperation;
    try
      Operation := TPhoaOp_GroupEdit.Create(FUndoOperations, FPhoA, FGroup, eText.Text, mDescription.Lines.Text);
    finally
      fMain.EndOperation(Operation);
    end;
    inherited ButtonClick_OK;
  end;

  function TdGroupProps.GetDataValid: Boolean;
  begin
    Result := eText.Text<>'';
  end;

  procedure TdGroupProps.InitializeDialog;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_group_props;
    eID.Text                := IntToStr(FGroup.ID);
    eText.Text              := FGroup.Text;
    mDescription.Lines.Text := FGroup.Description;
  end;

end.

