//**********************************************************************************************************************
//  $Id: ufrPicProps_View.pas,v 1.7 2004-05-30 18:41:18 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit ufrPicProps_View;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, GR32_Layers, TBXLists,
  phWizard, Menus, TB2Item, TBX, ActnList, GR32_Image, TB2ExtItems,
  TBXExtItems, TB2Dock, TB2Toolbar, phPicPropsDlgPage, DTLangTools;

type
  TfrPicProps_View = class(TPicPropsDialogPage)
    iMain: TImage32;
    alMain: TActionList;
    aZoomIn: TAction;
    aZoomOut: TAction;
    aZoomActual: TAction;
    aZoomFit: TAction;
    pmMain: TTBXPopupMenu;
    dtlsMain: TDTLanguageSwitcher;
    dkTop: TTBXDock;
    tbMain: TTBXToolbar;
    cbViewFile: TTBXComboBoxItem;
    bViewZoomIn: TTBXItem;
    bViewZoomOut: TTBXItem;
    bViewZoomActual: TTBXItem;
    bViewZoomFit: TTBXItem;
    dkLeft: TTBXDock;
    dkRight: TTBXDock;
    dkBottom: TTBXDock;
    tbTools: TTBXToolbar;
    TBXItem1: TTBXItem;
    TBXItem2: TTBXItem;
    TBXItem3: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    TBXItem4: TTBXItem;
    TBXItem5: TTBXItem;
    TBXItem6: TTBXItem;
    aRotate0: TAction;
    aRotate90: TAction;
    aRotate180: TAction;
    aRotate270: TAction;
    aFlipHorz: TAction;
    aFlipVert: TAction;
    procedure aaZoomIn(Sender: TObject);
    procedure aaZoomOut(Sender: TObject);
    procedure aaZoomActual(Sender: TObject);
    procedure aaZoomFit(Sender: TObject);
    procedure iMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure iMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure iMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure iMainResize(Sender: TObject);
    procedure cbViewFileChange(Sender: TObject; const Text: String);
    procedure cbViewFileAdjustImageIndex(Sender: TTBXComboBoxItem; const AText: String; AIndex: Integer; var ImageIndex: Integer);
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure aaRotate0(Sender: TObject);
    procedure aaRotate90(Sender: TObject);
    procedure aaRotate180(Sender: TObject);
    procedure aaRotate270(Sender: TObject);
    procedure aaFlipHorz(Sender: TObject);
    procedure aaFlipVert(Sender: TObject);
  private
     // Кэшированное изменение масштаба
    FZoomFactorChange: Single;
     // Размеры iMain
    FWView, FHView: Integer;
     // Размеры масштабированного изображения
    FWScaled, FHScaled: Integer;
     // Курсор для iView (crHand или crDefault)
    FImageCursor: TCursor;
     // Флаг и положение перетаскивания изображения
    FTrackDrag: Boolean;
    FTrackX: Integer;
    FTrackY: Integer;
     // Флаг того, что изображение загружено
    FImageLoaded: Boolean;
     // Возвращает коэффициент оптимального масштаба изображения
    function  BestFitZoomFactor: Single;
     // Загружает и отображает изображение
    procedure LoadViewImage;
     // Находит размеры окна и устанавливает масштаб так, чтобы изображение целиком туда вписалось
    procedure AdjustView;
     // Настраивает доступность Actions
    procedure EnableActions; 
     // Обработчик события таймера задержки
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER; 
     // Prop handlers
    function  GetViewOffset: TPoint;
    function  GetViewZoomFactor: Single;
    procedure SetViewOffset(const Value: TPoint);
    procedure SetViewZoomFactor(Value: Single);
  protected
    procedure InitializePage; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure AfterDisplay(ChangeMethod: TPageChangeMethod); override;
  public
     // Props
     // -- Масштаб просматриваемого изображения
    property ViewZoomFactor: Single read GetViewZoomFactor write SetViewZoomFactor;
     // -- Смещение левого верхнего угла просматриваемого изображения
    property ViewOffset: TPoint read GetViewOffset write SetViewOffset;
  end;

implementation
{$R *.dfm}
uses phUtils, phObj, ConsVars, Main, phSettings;

  procedure TfrPicProps_View.aaFlipHorz(Sender: TObject);
  begin
    //
  end;

  procedure TfrPicProps_View.aaFlipVert(Sender: TObject);
  begin
    //
  end;

  procedure TfrPicProps_View.aaRotate0(Sender: TObject);
  begin
    //
  end;

  procedure TfrPicProps_View.aaRotate180(Sender: TObject);
  begin
    //
  end;

  procedure TfrPicProps_View.aaRotate270(Sender: TObject);
  begin
    //
  end;

  procedure TfrPicProps_View.aaRotate90(Sender: TObject);
  begin
    //
  end;

  procedure TfrPicProps_View.aaZoomActual(Sender: TObject);
  begin
    ViewZoomFactor := 1.0;
  end;

  procedure TfrPicProps_View.aaZoomFit(Sender: TObject);
  begin
    ViewZoomFactor := BestFitZoomFactor;
  end;

  procedure TfrPicProps_View.aaZoomIn(Sender: TObject);
  begin
    ViewZoomFactor := ViewZoomFactor*FZoomFactorChange;
  end;

  procedure TfrPicProps_View.aaZoomOut(Sender: TObject);
  begin
    ViewZoomFactor := ViewZoomFactor/FZoomFactorChange;
  end;

  procedure TfrPicProps_View.AdjustView;
  begin
     // Если изображение загружено
    if FImageLoaded then begin
      FWView := iMain.Width;
      FHView := iMain.Height;
      ViewZoomFactor := BestFitZoomFactor;
    end;
  end;

  procedure TfrPicProps_View.AfterDisplay(ChangeMethod: TPageChangeMethod);
  begin
    inherited AfterDisplay(ChangeMethod);
    StorageForm.ActiveControl := iMain;
  end;

  procedure TfrPicProps_View.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var i: Integer;
  begin
    inherited BeforeDisplay(ChangeMethod);
     // Считываем имена файлов, если ещё не делали этого
    if cbViewFile.Strings.Count=0 then begin
      for i := 0 to EditedPicCount-1 do cbViewFile.Strings.Add(EditedPics[i].PicFileName);
      cbViewFile.ItemIndex := 0;
    end;
  end;

  function TfrPicProps_View.BestFitZoomFactor: Single;
  begin
    Result := MinS(FWView/iMain.Bitmap.Width, FHView/iMain.Bitmap.Height);
  end;

  procedure TfrPicProps_View.cbViewFileAdjustImageIndex(Sender: TTBXComboBoxItem; const AText: String; AIndex: Integer; var ImageIndex: Integer);
  begin
    if AIndex=-1 then AIndex := cbViewFile.Strings.IndexOf(AText);
    ImageIndex := FileImageIndex[AIndex];
  end;

  procedure TfrPicProps_View.cbViewFileChange(Sender: TObject; const Text: String);
  begin
    KillTimer(Handle, IPicPropsViewTimerID);
    SetTimer(Handle, IPicPropsViewTimerID, 500, nil);
  end;

  procedure TfrPicProps_View.EnableActions;
  begin
    aZoomIn.Enabled     := FImageLoaded and (ViewZoomFactor<SMaxPicZoom);
    aZoomOut.Enabled    := FImageLoaded and (ViewZoomFactor>SMinPicZoom);
    aZoomActual.Enabled := FImageLoaded and (ViewZoomFactor<>1);
    aZoomFit.Enabled    := FImageLoaded and (ViewZoomFactor<>BestFitZoomFactor);
  end;

  procedure TfrPicProps_View.FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    if WheelDelta<0 then aZoomOut.Execute else aZoomIn.Execute;
    Handled := True;
  end;

  function TfrPicProps_View.GetViewOffset: TPoint;
  begin
    Result := Point(Trunc(iMain.OffsetHorz), Trunc(iMain.OffsetVert));
  end;

  function TfrPicProps_View.GetViewZoomFactor: Single;
  begin
    Result := iMain.Scale;
  end;

  procedure TfrPicProps_View.iMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
    if (Button=mbLeft) and ((FWScaled>FWView) or (FHScaled>FHView)) then begin
      FTrackDrag := True;
      iMain.Cursor := crHandDrag;
      FTrackX := ViewOffset.x-x;
      FTrackY := ViewOffset.y-y;
    end;
  end;

  procedure TfrPicProps_View.iMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
    if FTrackDrag then ViewOffset := Point(x+FTrackX, y+FTrackY);
  end;

  procedure TfrPicProps_View.iMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
    if FTrackDrag then begin
      FTrackDrag := False;
       // Возвращаем прежний курсор
      iMain.Cursor := FImageCursor;
    end;
  end;

  procedure TfrPicProps_View.iMainResize(Sender: TObject);
  begin
    AdjustView;
  end;

  procedure TfrPicProps_View.InitializePage;
  begin
    inherited InitializePage;
    pmMain.LinkSubitems := tbMain.Items;
    cbViewFile.Images        := FileImages;
    cbViewFile.SubMenuImages := FileImages;
    FZoomFactorChange := adMagnifications[SettingValueInt(ISettingID_View_ZoomFactor)];
  end;

  procedure TfrPicProps_View.LoadViewImage;
  begin
    StartWait;
    try
       // Загружаем изображение
      try
        LoadGraphicFromFile(cbViewFile.Text, iMain.Bitmap);
        FImageLoaded := True;
      except
        FImageLoaded := False;
        raise;
      end;
       // Устанавливаем масштаб
      AdjustView;
    finally
      StopWait;
      EnableActions;
    end;
  end;

  procedure TfrPicProps_View.SetViewOffset(const Value: TPoint);
  var ix, iy: Integer;
  begin
    if FWScaled>FWView then ix := Min(0, Max(Value.x, FWView-FWScaled)) else ix := (FWView-FWScaled) div 2;
    if FHScaled>FHView then iy := Min(0, Max(Value.y, FHView-FHScaled)) else iy := (FHView-FHScaled) div 2;
    iMain.OffsetHorz := ix;
    iMain.OffsetVert := iy;
  end;

  procedure TfrPicProps_View.SetViewZoomFactor(Value: Single);
  begin
     // Verify zoom value
    if Value>SMaxPicZoom then Value := SMaxPicZoom
    else if Value<SMinPicZoom then Value := SMinPicZoom;
     // Apply zoom
    iMain.Scale := Value;
     // Находим размеры масштабированного изображения
    FWScaled := Round(iMain.Bitmap.Width*Value);
    FHScaled := Round(iMain.Bitmap.Height*Value);
     // Устанавливаем начальное положение изображения
    ViewOffset := Point((FWView-FWScaled) div 2, (FHView-FHScaled) div 2);
     // Настраиваем курсор
    FImageCursor := aImgViewCursors[(FWScaled>FWView) or (FHScaled>FHView)];
    iMain.Cursor := FImageCursor;
    EnableActions;
  end;

  procedure TfrPicProps_View.WMTimer(var Msg: TWMTimer);
  begin
    if Msg.TimerID=IPicPropsViewTimerID then begin
      KillTimer(Handle, IPicPropsViewTimerID);
      LoadViewImage;
    end;
  end;

end.
