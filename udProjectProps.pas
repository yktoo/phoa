//**********************************************************************************************************************
//  $Id: udProjectProps.pas,v 1.2 2004-12-31 13:38:58 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udProjectProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps,
  phDlg, RXSpin, ComCtrls, StdCtrls, ExtCtrls, DKLang;

type
  TdProjectProps = class(TPhoaDialog)
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
     // Приложение
    FApp: IPhotoAlbumApp;
     // Буфер отката
    FUndoOperations: TPhoaOperations;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
  end;

  function EditProject(AApp: IPhotoAlbumApp; UndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.DFM}
uses ConsVars, phUtils, Main;

  function EditProject(AApp: IPhotoAlbumApp; UndoOperations: TPhoaOperations): Boolean;
  begin
    with TdProjectProps.Create(Application) do
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

  procedure TdProjectProps.ButtonClick_OK;
  begin
    FApp.PerformOperation(
      'ProjectEdit',
      ['NewThWidth',     eThumbSizeX.AsInteger,
       'NewThHeight',    eThumbSizeY.AsInteger,
       'NewThQuality',   Byte(tbThumbQuality.Position),
       'NewDescription', mDesc.Text]);
    inherited ButtonClick_OK;
  end;

  procedure TdProjectProps.InitializeDialog;
  var Project: IPhotoAlbumProject;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_album_props;
     // Настраиваем контролы
    Project := FApp.Project;
    eThumbSizeX.AsInteger   := Project.ThumbnailSize.cx;
    eThumbSizeY.AsInteger   := Project.ThumbnailSize.cy;
    tbThumbQuality.Position := Project.ThumbnailQuality;
    mDesc.Lines.Text        := Project.Description;
  end;

end.


