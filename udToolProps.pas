unit udToolProps;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, phSettings, phToolSetting,
  Dialogs, phDlg, DTLangTools, StdCtrls, ExtCtrls;

type
  TdToolProps = class(TPhoaDialog)
    lKind: TLabel;
    cbKind: TComboBox;
    lName: TLabel;
    eName: TEdit;
    lHint: TLabel;
    eHint: TEdit;
    lRunCommand: TLabel;
    eRunCommand: TEdit;
    bBrowseRunCommand: TButton;
    lRunParams: TLabel;
    eRunParams: TEdit;
    lRunFolder: TLabel;
    eRunFolder: TEdit;
    bBrowseRunFolder: TButton;
    lRunShowCommand: TLabel;
    cbRunShowCommand: TComboBox;
    gbUsage: TGroupBox;
    cbUsageTools: TCheckBox;
    cbUsageGroups: TCheckBox;
    cbUsageThViewer: TCheckBox;
    lMasks: TLabel;
    eMasks: TEdit;
    odRunCommand: TOpenDialog;
    procedure bBrowseRunCommandClick(Sender: TObject);
    procedure bBrowseRunFolderClick(Sender: TObject);
  private
     // Редактируемая настройка инструмента (nil, если это "пустая" настройка, т.е. происходит добавление)
    FTool: TPhoaToolSetting;
     // Страничная настройка - владелец инструментов
    FPage: TPhoaToolPageSetting;
  protected
    procedure InitializeDialog; override;
    procedure ButtonClick_OK; override;
  end;

  function EditTool(ATool: TPhoaToolSetting; APage: TPhoaToolPageSetting): Boolean;

implementation
{$R *.dfm}
uses FileCtrl, phUtils;

  function EditTool(ATool: TPhoaToolSetting; APage: TPhoaToolPageSetting): Boolean;
  begin
    with TdToolProps.Create(Application) do
      try
        FTool := ATool;
        FPage := APage;
        Result := Execute;
      finally
        Free;
      end;
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
    ShowCmd: Integer;
    Usages: TPhoaToolUsages;
  begin
    Kind    := TPhoaToolKind(cbKind.ItemIndex);
    ShowCmd := Integer(cbRunShowCommand.Items.Objects[cbRunShowCommand.ItemIndex]);
    Usages := [];
    if cbUsageTools.Checked    then Include(Usages, ptuToolsMenu);
    if cbUsageGroups.Checked   then Include(Usages, ptuGroupPopupMenu);
    if cbUsageThViewer.Checked then Include(Usages, ptuThViewerPopupMenu);
    if FTool=nil then
      FTool := TPhoaToolSetting.Create(FPage, eName.Text, eHint.Text, eRunCommand.Text, eRunFolder.Text, eRunParams.Text, eMasks.Text, Kind, ShowCmd, Usages)
    else begin
      FTool.Kind           := Kind;
      FTool.Name           := eName.Text;
      FTool.Masks          := eMasks.Text;
      FTool.Hint           := eHint.Text;
      FTool.RunCommand     := eRunCommand.Text;
      FTool.RunParameters  := eRunParams.Text;
      FTool.RunFolder      := eRunFolder.Text;
      FTool.RunShowCommand := ShowCmd;
      FTool.Usages         := Usages;
    end;
    inherited ButtonClick_OK;
  end;

  procedure TdToolProps.InitializeDialog;
  var k: TPhoaToolKind;
  begin
    inherited InitializeDialog;
     // Заполняем cbKind
    for k := Low(k) to High(k) do cbKind.Items.Add(PhoaToolKindName(k)); 
     // Заполняем cbRunShowCommand
    with cbRunShowCommand.Items do begin
      Objects[0] := Pointer(SW_SHOWNORMAL);
      Objects[1] := Pointer(SW_SHOWMINIMIZED);
      Objects[2] := Pointer(SW_SHOWMAXIMIZED);
    end;
    if FTool=nil then
      cbKind.ItemIndex           := Byte(ptkOpen)
    else begin
      cbKind.ItemIndex           := Byte(FTool.Kind);
      eName.Text                 := ConstValEx(FTool.Name);
      eMasks.Text                := FTool.Masks;
      eHint.Text                 := ConstValEx(FTool.Hint);
      eRunCommand.Text           := FTool.RunCommand;
      eRunParams.Text            := FTool.RunParameters;
      eRunFolder.Text            := FTool.RunFolder;
      cbRunShowCommand.ItemIndex := cbRunShowCommand.Items.IndexOfObject(Pointer(FTool.RunShowCommand));
      cbUsageTools.Checked       := ptuToolsMenu         in FTool.Usages;
      cbUsageGroups.Checked      := ptuGroupPopupMenu    in FTool.Usages;
      cbUsageThViewer.Checked    := ptuThViewerPopupMenu in FTool.Usages;
    end;
  end;

end.
