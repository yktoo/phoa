//**********************************************************************************************************************
//  $Id: ufrWzPageAddFiles_SelFiles.pas,v 1.18 2004-10-26 13:51:18 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageAddFiles_SelFiles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, VirtualShellUtilities, GraphicEx,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, 
  phWizard, VirtualTrees, VirtualExplorerTree, StdCtrls,
  ExtCtrls, Mask, ToolEdit, DKLang;

type
  TfrWzPageAddFiles_SelFiles = class(TWizardPage)
    gbFilter: TGroupBox;
    lFileDateFrom: TLabel;
    lFileMasks: TLabel;
    lPresence: TLabel;
    lFileDateTo: TLabel;
    cbFileMasks: TComboBox;
    eFileDateFrom: TDateEdit;
    cbPresence: TComboBox;
    eFileDateTo: TDateEdit;
    eFileTimeFrom: TMaskEdit;
    eFileTimeTo: TMaskEdit;
    pMain: TPanel;
    cbRecurseFolders: TCheckBox;
    bAdvanced: TButton;
    tvMain: TVirtualExplorerTree;
    dklcMain: TDKLanguageController;
    procedure tvMainEnumFolder(Sender: TCustomVirtualExplorerTree; Namespace: TNamespace; var AllowAsChild: Boolean);
    procedure bAdvancedClick(Sender: TObject);
    procedure tvMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
     // Настраивает кнопку и панель расширенных опций
    procedure AdjustAdvancedCtls(bShowAdvanced: Boolean);
  protected
    function  GetDataValid: Boolean; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
  end;

implementation
{$R *.dfm}
uses
  phUtils, ufAddFilesWizard, ConsVars, phSettings, udMsgBox;

  procedure TfrWzPageAddFiles_SelFiles.AdjustAdvancedCtls(bShowAdvanced: Boolean);
  begin
    gbFilter.Visible := bShowAdvanced;
    bAdvanced.Tag := iif(bShowAdvanced, 1, 0);
    bAdvanced.Caption := ConstVal(iif(bShowAdvanced, 'SBtn_ExpandOn', 'SBtn_ExpandOff'));
  end;

  procedure TfrWzPageAddFiles_SelFiles.bAdvancedClick(Sender: TObject);
  begin
    AdjustAdvancedCtls(bAdvanced.Tag=0);
  end;

  procedure TfrWzPageAddFiles_SelFiles.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var Wiz: TfAddFilesWizard;
  begin
    inherited BeforeDisplay(ChangeMethod);
    if ChangeMethod<>pcmBackBtn then begin
      Wiz := TfAddFilesWizard(StorageForm);
       // Настраиваем браузер
       // -- Скрытые файлы/папки
      if SettingValueBool(ISettingID_Dlgs_APW_ShowHidden) then
        tvMain.FileObjects := [foFolders, foNonFolders, foHidden]
      else
        tvMain.FileObjects := [foFolders, foNonFolders];
       // -- Активизируем
      tvMain.Active := True;
       // -- Выставляем выделение
      tvMain.BrowseTo(Wiz.DefaultPath, False, True, False, True);
      cbRecurseFolders.Checked := Wiz.RecurseFolders;
       // Настраиваем фильтр
      cbPresence.ItemIndex := Byte(Wiz.Filter_Presence);
      RegLoadHistory(SRegAddFiles_MaskMRU, cbFileMasks, False);
      cbFileMasks.Text := Wiz.Filter_Masks;
      if Wiz.Filter_DateFrom>0  then eFileDateFrom.Date := Wiz.Filter_DateFrom;
      if Wiz.Filter_DateTo>0    then eFileDateTo.Date   := Wiz.Filter_DateTo;
      if Wiz.Filter_TimeFrom>=0 then eFileTimeFrom.Text := FormatDateTime('hh:nn', Wiz.Filter_TimeFrom);
      if Wiz.Filter_TimeTo>=0   then eFileTimeTo.Text   := FormatDateTime('hh:nn', Wiz.Filter_TimeTo);
       // Настраиваем расширенные опции
      AdjustAdvancedCtls(Wiz.ShowAdvancedOptions);
    end;
  end;

  function TfrWzPageAddFiles_SelFiles.GetDataValid: Boolean;
  var
    Namespace: TNamespace;
    Node: PVirtualNode;
  begin
    Result := False;
     // Проверяем, что есть выделение и всё выделенное является объектами файловой системы
    Node := tvMain.GetFirstSelected;
    if Node<>nil then begin
      repeat
        if not tvMain.ValidateNamespace(Node, Namespace) or not Namespace.FileSystem then Exit;
        Node := tvMain.GetNextSelected(Node);
      until Node=nil;
      Result := True;
    end;
  end;

  function TfrWzPageAddFiles_SelFiles.NextPage: Boolean;
  var
    Wiz: TfAddFilesWizard;
    Namespace: TNamespace;
    Node: PVirtualNode;
  begin
    Result := True;
    Wiz := TfAddFilesWizard(StorageForm);
     // Сохраняем критерии
    Wiz.Filter_Presence     := TAddFilePresenceFilter(cbPresence.ItemIndex);
    Wiz.Filter_Masks        := cbFileMasks.Text;
    Wiz.Filter_DateFrom     := StrToDateDef(eFileDateFrom.Text, -1);
    Wiz.Filter_DateTo       := StrToDateDef(eFileDateTo.Text,   -1);
    Wiz.Filter_TimeFrom     := StrToTimeDef(eFileTimeFrom.Text, -1);
    Wiz.Filter_TimeTo       := StrToTimeDef(eFileTimeTo.Text,   -1);
    Wiz.RecurseFolders      := cbRecurseFolders.Checked;
    Wiz.DefaultPath         := tvMain.SelectedPath;
    Wiz.ShowAdvancedOptions := bAdvanced.Tag<>0;
     // Записываем в Мастер список выбранных файлов/папок
    Wiz.AddList.Clear; 
    Node := tvMain.GetFirstSelected;
    while Assigned(Node) do begin
      if tvMain.ValidateNamespace(Node, Namespace) and Namespace.FileSystem then Wiz.AddList.Add(Namespace.NameParseAddress);
      Node := tvMain.GetNextSelected(Node);
    end;
     // Записываем настройки
    RegSaveHistory(SRegAddFiles_MaskMRU, cbFileMasks, True);
  end;

  procedure TfrWzPageAddFiles_SelFiles.tvMainChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  begin
    StatusChanged;
  end;

  procedure TfrWzPageAddFiles_SelFiles.tvMainEnumFolder(Sender: TCustomVirtualExplorerTree; Namespace: TNamespace; var AllowAsChild: Boolean);
  begin
    if Namespace.Folder then
      AllowAsChild := not Namespace.Browsable // Отсеиваем .ZIP-псевдопапки
    else
      AllowAsChild := FileFormatList.GraphicFromExtension(Namespace.FileName)<>nil;
  end;

end.

