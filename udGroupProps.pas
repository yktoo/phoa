unit udGroupProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps,
  phDlg, GR32_Image, ActnList, DKLang, StdCtrls, ExtCtrls;

type
  TdGroupProps = class(TPhoaDialog)
    aGroupIconReset: TAction;
    aGroupIconSelect: TAction;
    alMain: TActionList;
    bGroupIconReset: TButton;
    bGroupIconSelect: TButton;
    dklcMain: TDKLanguageController;
    eID: TEdit;
    eText: TEdit;
    gbGroupIcon: TGroupBox;
    iGroupIcon: TImage32;
    lDesc: TLabel;
    lID: TLabel;
    lText: TLabel;
    mDescription: TMemo;
    procedure aaGroupIconReset(Sender: TObject);
    procedure aaGroupIconSelect(Sender: TObject);
  private
     // Приложение
    FApp: IPhotoAlbumApp;
     // Буфер отката
    FUndoOperations: TPhoaOperations;
     // PNG-данные значка группы
    FGroupIconData: String;
     // Обновляет отображаемый значок группы
    procedure UpdateGroupIcon;
  protected
    function  GetDataValid: Boolean; override;
    procedure ButtonClick_OK; override;
    procedure DoCreate; override;
    procedure ExecuteInitialize; override;
    procedure UpdateState; override;
  end;

  function EditPicGroup(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;

implementation
{$R *.dfm}
uses GR32, GraphicEx, ConsVars, phUtils, phGraphics, Main, phFrm;

  function EditPicGroup(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations): Boolean;
  begin
    with TdGroupProps.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := AUndoOperations;
        Result := ExecuteModal(False, False);
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdGroupProps
   //===================================================================================================================

  procedure TdGroupProps.aaGroupIconReset(Sender: TObject);
  begin
    FGroupIconData := '';
    UpdateGroupIcon;
    Modified := True;
  end;

  procedure TdGroupProps.aaGroupIconSelect(Sender: TObject);
  var ImageSize, ThumbSize: TSize;
  begin
    with TOpenDialog.Create(Self) do
      try
        Filter   := FileFormatList.GetGraphicFilter([], fstExtension, [foCompact, foIncludeAll, foIncludeExtension], nil);
        Options  := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
        Title    := ConstVal('SDlgTitle_SelectPicFile');
        if Execute then begin
          FGroupIconData := GetBmp32ThumbnailData(FileName, Size(16, 16), psfLanczos, ImageSize, ThumbSize);
          UpdateGroupIcon;
          Modified := True;
        end;
      finally
        Free;
      end;
  end;

  procedure TdGroupProps.ButtonClick_OK;
  begin
    FApp.PerformOperation(
      'GroupEdit',
      ['Group', FApp.CurGroup, 'NewText', eText.Text, 'NewDescription', mDescription.Text,
       'NewIconData', FGroupIconData]);
    inherited ButtonClick_OK;
  end;

  procedure TdGroupProps.DoCreate;
  begin
    inherited DoCreate;
    HelpContext := IDH_intf_group_props;
  end;

  procedure TdGroupProps.ExecuteInitialize;
  var Group: IPhotoAlbumPicGroup;
  begin
    inherited ExecuteInitialize;
    Group := FApp.CurGroupX;
    eID.Text          := IntToStr(Group.ID);
    eText.Text        := Group.Text;
    mDescription.Text := Group.Description;
    FGroupIconData    := Group.IconData;
    UpdateGroupIcon;
  end;

  function TdGroupProps.GetDataValid: Boolean;
  begin
    Result := eText.Text<>'';
  end;

  procedure TdGroupProps.UpdateGroupIcon;
  begin
    PaintGroupIcon(FGroupIconData, iGroupIcon.Bitmap, C32Transparent, False, FApp);
  end;

  procedure TdGroupProps.UpdateState;
  begin
    inherited UpdateState;
    aGroupIconReset.Enabled := FGroupIconData<>'';
  end;

end.

