//**********************************************************************************************************************
//  $Id: udPhoAProps.pas,v 1.7 2004-10-12 12:38:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udPhoAProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps,
  phDlg, RXSpin, ComCtrls, StdCtrls, ExtCtrls, DKLang;

type
  TdPhoAProps = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    lDesc: TLabel;
    lThumbSize: TLabel;
    lThumbSizeX: TLabel;
    lThumbQuality: TLabel;
    lThQuality1: TLabel;
    lThQuality2: TLabel;
    mDesc: TMemo;
    tbThumbQuality: TTrackBar;
    eThumbSizeX: TRxSpinEdit;
    eThumbSizeY: TRxSpinEdit;
  private
    FProject: IPhotoAlbumProject;
    FUndoOperations: TPhoaOperations;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
  end;

  function EditPhoA(AProject: IPhotoAlbumProject; UndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.DFM}
uses ConsVars, phUtils, Main;

  function EditPhoA(AProject: IPhotoAlbumProject; UndoOperations: TPhoaOperations): Boolean;
  begin
    with TdPhoAProps.Create(Application) do
      try
        FProject        := AProject;
        FUndoOperations := UndoOperations;
        Result := Execute;
      finally
        Free;
      end;
  end;

  procedure TdPhoAProps.ButtonClick_OK;
  var Operation: TPhoaOperation;
  begin
    Operation := nil;
    fMain.BeginOperation;
    try
      Operation := TPhoaOp_PhoAEdit.Create(
        FUndoOperations,
        FProject,
        Size(eThumbSizeX.AsInteger, eThumbSizeY.AsInteger),
        tbThumbQuality.Position,
        mDesc.Lines.Text);
    finally
      fMain.EndOperation(Operation);
    end;
    inherited ButtonClick_OK;
  end;

  procedure TdPhoAProps.InitializeDialog;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_album_props;
    eThumbSizeX.AsInteger   := FProject.ThumbnailSize.cx;
    eThumbSizeY.AsInteger   := FProject.ThumbnailSize.cy;
    tbThumbQuality.Position := FProject.ThumbnailQuality;
    mDesc.Lines.Text        := FProject.Description;
  end;

end.


