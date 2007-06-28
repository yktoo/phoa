//**********************************************************************************************************************
//  $Id: udSortPics.pas,v 1.18 2007-06-28 18:41:40 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSortPics;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActiveX,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, 
  phDlg, DKLang, TntForms, ufrSorting, StdCtrls, TntStdCtrls, ExtCtrls,
  TntExtCtrls;

type
  TdSortPics = class(TPhoaDialog)
    bReset: TTntButton;
    dklcMain: TDKLanguageController;
    frSorting: TfrSorting;
    gbWhereToSort: TTntGroupBox;
    rbAllGroups: TTntRadioButton;
    rbCurGroup: TTntRadioButton;
    procedure bResetClick(Sender: TObject);
  private
     // Приложение
    FApp: IPhotoAlbumApp;
     // Буфер отката
    FUndoOperations: TPhoaOperations;
     // Если True, сортировать напрямую, без сохранения данных отката
    FDirectSort: Boolean;
  protected
    function  GetDataValid: Boolean; override;
    procedure ButtonClick_OK; override;
    procedure DoCreate; override;
    procedure ExecuteInitialize; override;
  end;

   // Отображает диалог сортировки изображений. Если bDirectSort=True, то сортирует выбранную группу непосредственно,
   //   без сохранения информации для отката (используется для сортировки результатов поиска)
  function DoSortPics(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations; bDirectSort: Boolean): Boolean;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main;

  function DoSortPics(AApp: IPhotoAlbumApp; AUndoOperations: TPhoaOperations; bDirectSort: Boolean): Boolean;
  begin
    with TdSortPics.Create(Application) do
      try
        FApp            := AApp;
        FUndoOperations := AUndoOperations;
        FDirectSort     := bDirectSort;
        Result := ExecuteModal(False, True);
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdSortPics
   //===================================================================================================================

  procedure TdSortPics.bResetClick(Sender: TObject);
  begin
    frSorting.Reset;
  end;

  procedure TdSortPics.ButtonClick_OK;
  var Group: IPhotoAlbumPicGroup;
  begin
     // Создаём операцию
    if rbAllGroups.Checked then Group := FApp.ProjectX.RootGroupX else Group := FApp.CurGroupX;
    if rbCurGroup.Checked and FDirectSort then begin
      Group.PicsX.SortingsSort(frSorting.Sortings);
      fMain.RefreshViewer;
    end else 
      FApp.PerformOperation('PicSort', ['Group', Group, 'Sortings', frSorting.Sortings, 'Recursive', rbAllGroups.Checked]);
     // Сохраняем использованные сортировки
    frSorting.Sortings.RegSave(SRegRoot, SRegSort_LastSortings);
    inherited ButtonClick_OK;
  end;

  procedure TdSortPics.DoCreate;
  begin
    inherited DoCreate;
    HelpContext := IDH_intf_sort_pics;
    frSorting.OnChange := DlgDataChange;
  end;

  procedure TdSortPics.ExecuteInitialize;
  var b: Boolean;
  begin
    inherited ExecuteInitialize;
     // Настраиваем frSorting
    frSorting.Sortings.RegLoad(SRegRoot, SRegSort_LastSortings);
    frSorting.SyncSortings;
     // Adjust RadioButtons
    b := FApp.CurGroup<>nil;
    rbCurGroup.Enabled  := b;
    rbCurGroup.Checked  := b;
    rbAllGroups.Checked := not b;
  end;

  function TdSortPics.GetDataValid: Boolean;
  begin
    Result := frSorting.Sortings.Count>0;
  end;

end.
