//**********************************************************************************************************************
//  $Id: udPhoAProps.pas,v 1.2 2004-04-15 12:54:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit udPhoAProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, phObj,
  phDlg, RXSpin, ComCtrls, StdCtrls, ExtCtrls, DTLangTools;

type
  TdPhoAProps = class(TPhoaDialog)
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
    FPhoA: TPhotoAlbum;
    FUndoOperations: TPhoaOperations;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
  end;

  function EditPhoA(PhoA: TPhotoAlbum; UndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.DFM}
uses ConsVars, phUtils, Main;

  function EditPhoA(PhoA: TPhotoAlbum; UndoOperations: TPhoaOperations): Boolean;
  begin
    with TdPhoAProps.Create(Application) do
      try
        FPhoA           := PhoA;
        FUndoOperations := UndoOperations;
        Result := Execute;
      finally
        Free;
      end;
  end;

  procedure TdPhoAProps.ButtonClick_OK;
  begin
    fMain.PerformOperation(TPhoaOp_PhoAEdit.Create(
      FUndoOperations,
      FPhoA,
      eThumbSizeX.AsInteger,
      eThumbSizeY.AsInteger,
      tbThumbQuality.Position,
      mDesc.Lines.Text));
    inherited ButtonClick_OK;
  end;

  procedure TdPhoAProps.InitializeDialog;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_album_props;
    with FPhoA do begin
      eThumbSizeX.AsInteger   := ThumbnailWidth;
      eThumbSizeY.AsInteger   := ThumbnailHeight;
      tbThumbQuality.Position := ThumbnailQuality;
      mDesc.Lines.Text        := Description;
    end;
  end;

end.

