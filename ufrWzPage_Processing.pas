//**********************************************************************************************************************
//  $Id: ufrWzPage_Processing.pas,v 1.5 2004-12-31 13:38:58 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPage_Processing;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars,
  phWizard, StdCtrls, ComCtrls, ExtCtrls, GR32, GR32_Image;

type
  TfrWzPage_Processing = class(TWizardPage)
    gbMain: TGroupBox;
    lInfo: TLabel;
    pbMain: TProgressBar;
    bInterrupt: TButton;
    iThumb: TImage32;
    procedure bInterruptClick(Sender: TObject);
  private
    procedure WMPageUpdate(var Msg: TMessage); message WM_PAGEUPDATE;
     // Обновляет статус
    procedure UpdateStatus;
  protected
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
  end;

implementation
{$R *.dfm}
uses phUtils;

  procedure TfrWzPage_Processing.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited BeforeDisplay(ChangeMethod);
    iThumb.Bitmap.SetSize(iThumb.Width, iThumb.Height);
    pbMain.Position := 0;
  end;

  procedure TfrWzPage_Processing.bInterruptClick(Sender: TObject);
  begin
     // Запрещаем кнопку до следующего обновления статуса
    bInterrupt.Enabled := False;
     // Начинаем или уведомляем мастер о необходимости прервать обработку
    with StorageForm as IPhoaWizardPageHost_Process do
      if ProcessingActive then StopProcessing else StartProcessing;
  end;

  procedure TfrWzPage_Processing.UpdateStatus;
  var
    IProcess: IPhoaWizardPageHost_Process;
    bActive: Boolean;
  begin
    bInterrupt.Enabled := True;
    iThumb.Bitmap.Clear(Color32(gbMain.Color));
    IProcess := StorageForm as IPhoaWizardPageHost_Process;
    bActive := IProcess.ProcessingActive;
     // Обновляем статус
    lInfo.Caption := IProcess.CurrentStatus;
     // Если процесс активен, отображаем прогресс
    if bActive then begin
      pbMain.Max      := IProcess.ProgressMax;
      pbMain.Position := IProcess.ProgressCur;
      IProcess.PaintThumbnail(iThumb.Bitmap);
    end;
    bInterrupt.Caption := ConstVal(iif(bActive, 'SBtn_Interrupt', 'SBtn_Continue'));
    bInterrupt.Cancel  := bActive;
    bInterrupt.Default := bActive;
    pbMain.Visible := bActive;
  end;

  procedure TfrWzPage_Processing.WMPageUpdate(var Msg: TMessage);
  begin
    UpdateStatus;
  end;

end.
