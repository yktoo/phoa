unit udGroupProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, 
  phDlg, StdCtrls, ExtCtrls, DKLang;

type
  TdGroupProps = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    lID: TLabel;
    eID: TEdit;
    lText: TLabel;
    eText: TEdit;
    lDesc: TLabel;
    mDescription: TMemo;
  private
     // Приложение
    FApp: IPhotoAlbumApp;
     // Буфер отката
    FUndoOperations: TPhoaOperations;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetDataValid: Boolean; override;
  end;

  function EditPicGroup(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses ConsVars, phUtils, Main;

  function EditPicGroup(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
  begin
    with TdGroupProps.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := AUndoOperations;
        Result := Execute;
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdGroupProps
   //===================================================================================================================

  procedure TdGroupProps.ButtonClick_OK;
  var Changes: TPhoaOperationChanges;
  begin
    Changes := [];
    fMain.BeginOperation;
    try
      TPhoaOp_GroupEdit.Create(
        FUndoOperations,
        FApp.Project,
        NewPhoaOperationParams(['Group', FApp.CurGroup, 'NewText', eText.Text, 'NewDescription', mDescription.Lines.Text]),
        Changes);
    finally
      fMain.EndOperation(Changes);
    end;
    inherited ButtonClick_OK;
  end;

  function TdGroupProps.GetDataValid: Boolean;
  begin
    Result := eText.Text<>'';
  end;

  procedure TdGroupProps.InitializeDialog;
  var Group: IPhotoAlbumPicGroup;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_group_props;
    Group := FApp.CurGroup;
    eID.Text                := IntToStr(Group.ID);
    eText.Text              := Group.Text;
    mDescription.Lines.Text := Group.Description;
  end;

end.

