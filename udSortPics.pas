//**********************************************************************************************************************
//  $Id: udSortPics.pas,v 1.15 2004-12-31 13:38:58 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSortPics;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActiveX,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, 
  phDlg, VirtualTrees, TBX, TB2Item, Menus, StdCtrls, ExtCtrls, ufrSorting, DKLang;

type
  TdSortPics = class(TPhoaDialog)
    dklcMain: TDKLanguageController;
    bReset: TButton;
    gbWhereToSort: TGroupBox;
    rbCurGroup: TRadioButton;
    rbAllGroups: TRadioButton;
    frSorting: TfrSorting;
    procedure bResetClick(Sender: TObject);
  private
     // Приложение
    FApp: IPhotoAlbumApp;
     // Буфер отката
    FUndoOperations: TPhoaOperations;
     // Если True, сортировать напрямую, без сохранения данных отката
    FDirectSort: Boolean;
     // Событие изменения frSorting
    procedure frSortingChange(Sender: TObject);
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
    function  GetDataValid: Boolean; override;
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
        Result := Execute;
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
    if rbAllGroups.Checked then Group := FApp.Project.RootGroupX else Group := FApp.CurGroup;
    if rbCurGroup.Checked and FDirectSort then begin
      Group.PicsX.SortingsSort(frSorting.Sortings);
      fMain.RefreshViewer;
    end else 
      FApp.PerformOperation('PicSort', ['Group', Group, 'Sortings', frSorting.Sortings, 'Recursive', rbAllGroups.Checked]);
     // Сохраняем использованные сортировки
    frSorting.Sortings.RegSave(SRegRoot, SRegSort_LastSortings);
    inherited ButtonClick_OK;
  end;

  procedure TdSortPics.frSortingChange(Sender: TObject);
  begin
    Modified := True;
  end;

  function TdSortPics.GetDataValid: Boolean;
  begin
    Result := frSorting.Sortings.Count>0;
  end;

  procedure TdSortPics.InitializeDialog;
  var b: Boolean;
  begin
    inherited InitializeDialog;
    HelpContext := IDH_intf_sort_pics;
    OKIgnoresModified := True;
    frSorting.Sortings.RegLoad(SRegRoot, SRegSort_LastSortings);
    frSorting.SyncSortings;
    frSorting.OnChange := frSortingChange;
     // Adjust RadioButtons
    b := FApp.CurGroup<>nil;
    rbCurGroup.Enabled  := b;
    rbCurGroup.Checked  := b;
    rbAllGroups.Checked := not b;
  end;

end.
