//**********************************************************************************************************************
//  $Id: udSortPics.pas,v 1.7 2004-09-27 17:07:23 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udSortPics;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, phObj, ActiveX,
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
     // Фотоальбом
    FPhoA: TPhotoAlbum;
     // Текущая выбранная группа в дереве
    FGroup: TPhoaGroup;
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
  function DoSortPics(PhoA: TPhotoAlbum; Group: TPhoaGroup; UndoOperations: TPhoaOperations; bDirectSort: Boolean): Boolean;

implementation
{$R *.dfm}
uses phUtils, ConsVars, Main;

  function DoSortPics(PhoA: TPhotoAlbum; Group: TPhoaGroup; UndoOperations: TPhoaOperations; bDirectSort: Boolean): Boolean;
  begin
    with TdSortPics.Create(Application) do
      try
        FPhoA           := PhoA;
        FGroup          := Group;
        FUndoOperations := UndoOperations;
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
  var
    Operation: TPhoaOperation;
    g: TPhoaGroup;
  begin
     // Создаём операцию
    if rbAllGroups.Checked then g := FPhoA.RootGroup else g := FGroup;
    if rbCurGroup.Checked and FDirectSort then begin
      g.SortPics(frSorting.Sortings, FPhoA.Pics);
      fMain.RefreshViewer;
    end else begin
      Operation := nil;
      fMain.BeginOperation;
      try
        Operation := TPhoaMultiOp_PicSort.Create(FUndoOperations, FPhoA, g, frSorting.Sortings, rbAllGroups.Checked);
      finally
        fMain.EndOperation(Operation);
      end;
    end;
     // Сохраняем использованные сортировки
    frSorting.Sortings.RegSave(SRegSort_LastSortings);
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
    frSorting.Sortings.RegLoad(SRegSort_LastSortings);
    frSorting.SyncSortings;
    frSorting.OnChange := frSortingChange;
     // Adjust RadioButtons
    b := FGroup<>nil;
    rbCurGroup.Enabled  := b;
    rbCurGroup.Checked  := b;
    rbAllGroups.Checked := not b;
  end;

end.
