//**********************************************************************************************************************
//  $Id: udStringBox.pas,v 1.2 2004-04-15 12:54:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
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
