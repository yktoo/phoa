unit udToolProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, phSettings, phToolSetting,
  Dialogs, phDlg, StdCtrls, ExtCtrls, DKLang;

type
  TdToolProps = class(TPhoaDialog)
    bBrowseRunCommand: TButton;
    bBrowseRunFolder: TButton;
    cbKind: TComboBox;
    cbRunShowCommand: TComboBox;
    cbUsageGroups: TCheckBox;
    cbUsageThViewer: TCheckBox;
    cbUsageTools: TCheckBox;
    cbUsageViewMode: TCheckBox;
    dklcMain: TDKLanguageController;
    eHint: TEdit;
    eMasks: TEdit;
    eName: TEdit;
    eRunCommand: TEdit;
    eRunFolder: TEdit;
    eRunParams: TEdit;
    gbUsage: TGroupBox;
    lHint: TLabel;
    lKind: TLabel;
    lMasks: TLabel;
    lName: TLabel;
    lRunCommand: TLabel;
    lRunFolder: TLabel;
    lRunParams: TLabel;
    lRunShowCommand: TLabel;
    odRunCommand: TOpenDialog;
    procedure bBrowseRunCommandClick(Sender: TObject);
    procedure bBrowseRunFolderClick(Sender: TObject);
    procedure cbKindChange(Sender: TObject);
    procedure cbKindDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
     // Редактируемая настройка инструмента (nil, если это "пустая" настройка, т.е. происходит добавление)
    FTool: TPhoaToolSetting;
     // Страничная настройка - владелец инструментов
    FPage: TPhoaToolPageSetting;
     // Настраивает доступность контролов в зависимости от текущего вида инструмента
    procedure ApplyCurKind; 
  protected
    procedure ButtonClick_OK; override;
    procedure DoCreate; override;
    procedure ExecuteInitialize; override;
  end;

  function EditTool(ATool: TPhoaToolSetting; APage: TPhoaToolPageSetting): Boolean;

implementation
{$R *.dfm}
uses FileCtrl, phUtils, Main, ImgList, ConsVars;

  function EditTool(ATool: TPhoaToolSetting; APage: TPhoaToolPageSetting): Boolean;
  begin
    with TdToolProps.Create(Application) do
      try
        FTool := ATool;
        FPage := APage;
        Result := ExecuteModal(False, False);
      finally
        Free;
      end;
  end;

   //===================================================================================================================
   // TdToolProps
   //===================================================================================================================

  procedure TdToolProps.ApplyCurKind;
  var k: TPhoaToolKind;
  begin
    k := TPhoaToolKind(cbKind.ItemIndex);
    EnableControls(not (k in [ptkSeparator, ptkExtViewer]), [lName, eName, lHint, eHint]);
    EnableControls(
      k in [ptkCustom, ptkExtViewer],
      [lRunCommand, eRunCommand, bBrowseRunCommand, lRunParams, eRunParams, lRunShowCommand, cbRunShowCommand,
       lRunFolder, eRunFolder, bBrowseRunFolder]);
    cbUsageTools.Enabled    := k<>ptkExtViewer;
    cbUsageGroups.Enabled   := k<>ptkExtViewer;
    cbUsageThViewer.Enabled := k<>ptkExtViewer;
    cbUsageViewMode.Enabled := k<>ptkExtViewer;
  end;

  procedure TdToolProps.bBrowseRunCommandClick(Sender: TObject);
  begin
    odRunCommand.FileName := eRunCommand.Text;
    if odRunCommand.Execute then eRunCommand.Text := odRunCommand.FileName;
  end;

  procedure TdToolProps.bBrowseRunFolderClick(Sender: TObject);
  var s: String;
  begin
    s := eRunFolder.Text;
    if SelectDirectory(ConstVal('SDlgTitle_SelectFolder'), '', s) then eRunFolder.Text := s;
  end;

  procedure TdToolProps.ButtonClick_OK;
  var
    Kind: TPhoaToolKind;
    iShowCmd: Integer;
    Usages: TPhoaToolUsages;
    sName, sHint, sRunCommand, sRunFolder, sRunParams: String;
  begin
    Kind    := TPhoaToolKind(cbKind.ItemIndex);
    if Kind in [ptkSeparator, ptkExtViewer] then begin
      sName := '';
      sHint := '';
    end else begin
      sName := eName.Text;
      sHint := eHint.Text;
    end;
    if Kind in [ptkCustom, ptkExtViewer] then begin
      iShowCmd    := GetCurrentCBObject(cbRunShowCommand);
      sRunCommand := eRunCommand.Text;
      sRunFolder  := eRunFolder.Text;
      sRunParams  := eRunParams.Text;
    end else begin
      iShowCmd    := SW_SHOWNORMAL;
      sRunCommand := '';
      sRunFolder  := '';
      sRunParams  := '';
    end;
    Usages := [];
    if Kind<>ptkExtViewer then begin
      if cbUsageTools.Checked    then Include(Usages, ptuToolsMenu);
      if cbUsageGroups.Checked   then Include(Usages, ptuGroupPopupMenu);
      if cbUsageThViewer.Checked then Include(Usages, ptuThViewerPopupMenu);
      if cbUsageViewMode.Checked then Include(Usages, ptuViewModePopupMenu);
    end;
     // Новый инструмент
    if FTool=nil then begin
      FTool := TPhoaToolSetting.Create(FPage, sName, sHint, sRunCommand, sRunFolder, sRunParams, eMasks.Text, Kind, iShowCmd, Usages);
       // Новый помечаем как изменённый
      FTool.Modified := True;
     // Существующий инструмент
    end else begin
      FTool.Kind           := Kind;
      FTool.Name           := sName;
      FTool.Masks          := eMasks.Text;
      FTool.Hint           := sHint;
      FTool.RunCommand     := sRunCommand;
      FTool.RunParameters  := sRunParams;
      FTool.RunFolder      := sRunFolder;
      FTool.RunShowCommand := iShowCmd;
      FTool.Usages         := Usages;
    end;
    inherited ButtonClick_OK;
  end;

  procedure TdToolProps.cbKindChange(Sender: TObject);
  begin
    ApplyCurKind;
    Modified := True;
  end;

  procedure TdToolProps.cbKindDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  begin
     // Стираем
    cbKind.Canvas.FillRect(Rect);
     // Рисуем значок
    fMain.ilActionsSmall.Draw(cbKind.Canvas, Rect.Left, Rect.Top, aToolImageIndexes[TPhoaToolKind(Index)]);
     // Рисуем текст
    Inc(Rect.Left, 20);
    DrawText(cbKind.Canvas.Handle, PChar(cbKind.Items[Index]), -1, Rect, DT_LEFT or DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER);
  end;

  procedure TdToolProps.DoCreate;
  var k: TPhoaToolKind;
  begin
    inherited DoCreate;
    HelpContext := IDH_intf_tool_props;
     // Заполняем cbKind
    for k := Low(k) to High(k) do cbKind.Items.Add(PhoaToolKindName(k));
     // Заполняем cbRunShowCommand
    with cbRunShowCommand.Items do begin
      Objects[0] := Pointer(SW_SHOWNORMAL);
      Objects[1] := Pointer(SW_SHOWMINIMIZED);
      Objects[2] := Pointer(SW_SHOWMAXIMIZED);
    end;
  end;

  procedure TdToolProps.ExecuteInitialize;
  begin
    inherited ExecuteInitialize;
     // Инициализируем значения контролов
    if FTool=nil then begin
      cbKind.ItemIndex           := Byte(ptkOpen);
      SetCurrentCBObject(cbRunShowCommand, SW_SHOWNORMAL);
    end else begin
      cbKind.ItemIndex           := Byte(FTool.Kind);
      eName.Text                 := ConstValEx(FTool.Name);
      eMasks.Text                := FTool.Masks;
      eHint.Text                 := ConstValEx(FTool.Hint);
      eRunCommand.Text           := FTool.RunCommand;
      eRunParams.Text            := FTool.RunParameters;
      eRunFolder.Text            := FTool.RunFolder;
      SetCurrentCBObject(cbRunShowCommand, FTool.RunShowCommand);
      cbUsageTools.Checked       := ptuToolsMenu         in FTool.Usages;
      cbUsageGroups.Checked      := ptuGroupPopupMenu    in FTool.Usages;
      cbUsageThViewer.Checked    := ptuThViewerPopupMenu in FTool.Usages;
      cbUsageViewMode.Checked    := ptuViewModePopupMenu in FTool.Usages;
    end;
    ApplyCurKind;
  end;

end.

