unit udStringBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phDlg, StdCtrls, DTLangTools, ExtCtrls;

type
  TdStringBox = class(TPhoaDialog)
    eMain: TEdit;
    lMain: TLabel;
  end;

  function StringBox(var s: String; const sCaption, sLabel: String; iHelpContext: THelpContext): Boolean;

implementation
{$R *.dfm}

  function StringBox(var s: String; const sCaption, sLabel: String; iHelpContext: THelpContext): Boolean;
  begin
    with TdStringBox.Create(Application) do
      try
        HelpContext := iHelpContext;
        if sCaption<>'' then Caption := sCaption;
        if sLabel<>'' then begin
          lMain.Caption := sLabel;
           // Если метка не влазит, подгоняем ширину окна
          if lMain.Width>eMain.Width then ClientWidth := lMain.Width+(ClientWidth-eMain.Width)+8;
        end;
        eMain.Text := s;
        Result := Execute;
        if Result then s := eMain.Text; 
      finally
        Free;
      end;
  end;

end.
