//**********************************************************************************************************************
//  $Id: udPhoAProps.pas,v 1.10 2004-10-19 15:03:31 dale Exp $
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
    eThumbSizeX: TRxSpinEdit;
    eThumbSizeY: TRxSpinEdit;
    lDesc: TLabel;
    lThQuality1: TLabel;
    lThQuality2: TLabel;
    lThumbQuality: TLabel;
    lThumbSize: TLabel;
    lThumbSizeX: TLabel;
    mDesc: TMemo;
    tbThumbQuality: TTrackBar;
  private
     // ����������
    FApp: IPhotoAlbumApp;
     // ����� ������
    FUndoOperations: TPhoaOperations;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
  end;

  function EditPhoA(AApp: IPhotoAlbumApp; UndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.DFM}
uses ConsVars, phUtils, Main;

  function EditPhoA(AApp: IPhotoAlbumApp; UndoOperations: TPhoaOperations): Boolean;
  begin
    with TdPhoAProps.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := UndoOperations;
        Result := Execute;
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdPhoAProps
   //===================================================================================================================

  procedure TdPhoAProps.ButtonClick_OK;
  begin
    FApp.PerformOperation(
      'ProjectEdit',
      ['NewThWidth',     eThumbSizeX.AsInteger,
       'NewThHeight',    eThumbSizeY.AsInteger,
       'NewThQuality',   tbThumbQuality.Position,
       'NewDescription', mDesc.Text]);
    inherited ButtonClick_OK;
  end;

  procedure TdPhoAProps.InitializeDialog;
  var Project: IPhotoAlbumProject;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_album_props;
     // ����������� ��������
    Project := FApp.Project;
    eThumbSizeX.AsInteger   := Project.ThumbnailSize.cx;
    eThumbSizeY.AsInteger   := Project.ThumbnailSize.cy;
    tbThumbQuality.Position := Project.ThumbnailQuality;
    mDesc.Lines.Text        := Project.Description;
  end;

end.


