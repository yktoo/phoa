//**********************************************************************************************************************
//  $Id: ufrPicProps_View.pas,v 1.20 2004-10-11 11:41:24 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrPicProps_View;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, GR32_Layers, TBXLists, ConsVars,
  phIntf, phMutableIntf, phObj, phGraphics,
  phWizard, Menus, TB2Item, TBX, ActnList, GR32_Image, TB2ExtItems,
  TBXExtItems, TB2Dock, TB2Toolbar, phPicPropsDlgPage, DKLang;

type
  TfrPicProps_View = class(TPicPropsDialogPage)
    iMain: TImage32;
    alMain: TActionList;
    aZoomIn: TAction;
    aZoomOut: TAction;
    aZoomActual: TAction;
    aZoomFit: TAction;
    pmMain: TTBXPopupMenu;
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
    bRotate180: TTBXItem;
    bRotate90: TTBXItem;
    bRotate0: TTBXItem;
    tbSepFlipHorz: TTBXSeparatorItem;
    bFlipHorz: TTBXItem;
    bRotate270: TTBXItem;
    bFlipVert: TTBXItem;
    aRotate0: TAction;
    aRotate90: TAction;
    aRotate180: TAction;
    aRotate270: TAction;
    aFlipHorz: TAction;
    aFlipVert: TAction;
    gipmToolsToolbar: TTBGroupItem;
    gipmMainToolbar: TTBGroupItem;
    ipmSep: TTBXSeparatorItem;
    dklcMain: TDKLanguageController;
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
     // Флаг того, что страница проинициализирована
    FInitialized: Boolean;
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
     // Текущее отобржаемое изображение (инициализируется в LoadViewImage)
    FPic: IPhoaPic;
     // Преобразование изображений
    FTransform: TPicTransform;
     // Флаг, свидетельствующий о том, что поворот у выделенных изображений разный, и применять поворот из Transform не
     //   нужно 
    FNoRotation: Boolean;
     // Флаги, свидетельствующий о том, что наличие отражений у выделенных изображений разное, и применять соотв.
     //   отражения из Transform не нужно
    FNoFlipHorz: Boolean;
    FNoFlipVert: Boolean;
     // True в процессе отображения изображения
    FDisplayingImage: Boolean;
     // Возвращает коэффициент оптимального масштаба изображения
    function  BestFitZoomFactor: Single;
     // Загружает и отображает изображение
    procedure LoadViewImage;
     // Находит размеры окна и устанавливает масштаб так, чтобы изображение целиком туда вписалось
    procedure AdjustView;
     // События преобразования
    procedure TransformApplied(Sender: TObject);
     // Настраивает свойство Checked для Actions преобразований
    procedure UpdateTransformActions;
     // Настраивает доступность Actions
    procedure EnableActions;
     // Обработчик события таймера задержки
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
     // Возвращает требуемые преобразования для изображения с учётом флагов FNoRotation, FNoFlipHorz, FNoFlipVert
    procedure GetRequiredTransform(Pic: IPhoaPic; out Rotation: TPicRotation; out Flips: TPicFlips);
     // Prop handlers
    function  GetViewOffset: TPoint;
    function  GetViewZoomFactor: Single;
    procedure SetViewOffset(const Value: TPoint);
    procedure SetViewZoomFactor(Value: Single);
  protected
    procedure InitializePage; override;
    procedure FinalizePage; override;
    function  GetRegistrySection: String; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    procedure AfterDisplay(ChangeMethod: TPageChangeMethod); override;
  public
    procedure Apply(FOperations: TPhoaOperations); override;
     // Props
     // -- Масштаб просматриваемого изображения
    property ViewZoomFactor: Single read GetViewZoomFactor write SetViewZoomFactor;
     // -- Смещение левого верхнего угла просматриваемого изображения
    property ViewOffset: TPoint read GetViewOffset write SetViewOffset;
  end;

implementation
{$R *.dfm}
uses phUtils, Main, phSettings;

  procedure TfrPicProps_View.aaFlipHorz(Sender: TObject);
  begin
    FNoFlipHorz := False;
    FTransform.ToggleFlip(pflHorz);
  end;

  procedure TfrPicProps_View.aaFlipVert(Sender: TObject);
  begin
    FNoFlipVert := False;
    FTransform.ToggleFlip(pflVert);
  end;

  procedure TfrPicProps_View.aaRotate0(Sender: TObject);
  begin
    FNoRotation := False;
    FTransform.Rotation := pr0;
  end;

  procedure TfrPicProps_View.aaRotate180(Sender: TObject);
  begin
    FNoRotation := False;
    FTransform.Rotation := pr180;
  end;

  procedure TfrPicProps_View.aaRotate270(Sender: TObject);
  begin
    FNoRotation := False;
    FTransform.Rotation := pr270;
  end;

  procedure TfrPicProps_View.aaRotate90(Sender: TObject);
  begin
    FNoRotation := False;
    FTransform.Rotation := pr90;
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

  procedure TfrPicProps_View.Apply(FOperations: TPhoaOperations);
  var
    i: Integer;
    Pic: IPhotoAlbumPic;
    Rotation: TPicRotation;
    Flips: TPicFlips;
  begin
    inherited Apply(FOperations);
     // Если страница инициализирована, создаём операции применения преобразований
    if FInitialized and not (FNoRotation and FNoFlipHorz and FNoFlipVert) then
      for i := 0 to EditedPics.Count-1 do begin
        Pic := EditedPics[i];
         // Определяем требуемое преобразование
        GetRequiredTransform(Pic, Rotation, Flips);
         // Если оно не совпадает - создаём операцию
        if (Rotation<>Pic.Rotation) or (Flips<>Pic.Flips) then
          TPhoaOp_StoreTransform.Create(FOperations, PhoA, Pic, Rotation, Flips);
      end;
  end;

  procedure TfrPicProps_View.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var
    i: Integer;
    Pic: IPhoaPic;
  begin
    inherited BeforeDisplay(ChangeMethod);
    if not FInitialized then begin
       // Создаём преобразование
      FTransform := TPicTransform.Create(iMain.Bitmap);
      FTransform.OnApplied := TransformApplied;
       // Считываем имена файлов и проверяем свойства преобразований
      for i := 0 to EditedPics.Count-1 do begin
        Pic := EditedPics[i];
        cbViewFile.Strings.Add(Pic.FileName);
        if i=0 then
          FTransform.InitValues(Pic.Rotation, Pic.Flips)
        else begin
          if Pic.Rotation<>FTransform.Rotation                     then FNoRotation := True;
          if (pflHorz in Pic.Flips)<>(pflHorz in FTransform.Flips) then FNoFlipHorz := True;
          if (pflVert in Pic.Flips)<>(pflVert in FTransform.Flips) then FNoFlipVert := True;
        end;
      end;
      cbViewFile.ItemIndex := 0;
      FInitialized := True;
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

  procedure TfrPicProps_View.FinalizePage;
  begin
    FTransform.Free;
    inherited FinalizePage;
  end;

  procedure TfrPicProps_View.FrameMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    if WheelDelta<0 then aZoomOut.Execute else aZoomIn.Execute;
    Handled := True;
  end;

  function TfrPicProps_View.GetRegistrySection: String;
  begin
    Result := SRegWizPage_PicProp_View;
  end;

  procedure TfrPicProps_View.GetRequiredTransform(Pic: IPhoaPic; out Rotation: TPicRotation; out Flips: TPicFlips);
  begin
    if FNoRotation then Rotation := Pic.Rotation else Rotation := FTransform.Rotation;
    Flips := [];
    if (FNoFlipHorz and (pflHorz in Pic.Flips)) or (not FNoFlipHorz and (pflHorz in FTransform.Flips)) then Include(Flips, pflHorz);
    if (FNoFlipVert and (pflVert in Pic.Flips)) or (not FNoFlipVert and (pflVert in FTransform.Flips)) then Include(Flips, pflVert);
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
    gipmMainToolbar.LinkSubitems  := tbMain.Items;
    gipmToolsToolbar.LinkSubitems := tbTools.Items;
    cbViewFile.Images        := FileImages;
    cbViewFile.SubMenuImages := FileImages;
    FZoomFactorChange := adMagnifications[SettingValueInt(ISettingID_View_ZoomFactor)];
  end;

  procedure TfrPicProps_View.LoadViewImage;
  var
    Rotation: TPicRotation;
    Flips: TPicFlips;
  begin
     // Запрещаем изменения Modified
    BeginUpdate;
    StartWait;
    FDisplayingImage := True;
    try
      FPic := EditedPics[cbViewFile.ItemIndex];
       // Загружаем изображение
      try
        LoadGraphicFromFile(FPic.FileName, iMain.Bitmap);
        FImageLoaded := True;
      except
        FImageLoaded := False;
        raise;
      end;
       // Определяем требуемое преобразование
      GetRequiredTransform(FPic, Rotation, Flips);
       // Применяем преобразование
      FTransform.InitValues(pr0, []);
      FTransform.ApplyValues(Rotation, Flips);
       // Настраиваем размеры изображения
      AdjustView;
      UpdateTransformActions;
    finally
      FDisplayingImage := False;
      StopWait;
      EndUpdate;
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

  procedure TfrPicProps_View.TransformApplied(Sender: TObject);
  begin
    if FDisplayingImage then Exit;
    AdjustView;
    UpdateTransformActions;
    Modified;
  end;

  procedure TfrPicProps_View.UpdateTransformActions;
  begin
    aRotate0.Checked   := not FNoRotation and (FTransform.Rotation=pr0);
    aRotate90.Checked  := not FNoRotation and (FTransform.Rotation=pr90);
    aRotate180.Checked := not FNoRotation and (FTransform.Rotation=pr180);
    aRotate270.Checked := not FNoRotation and (FTransform.Rotation=pr270);
    aFlipHorz.Checked  := not FNoFlipHorz and (pflHorz in FTransform.Flips);
    aFlipVert.Checked  := not FNoFlipVert and (pflVert in FTransform.Flips);
  end;

  procedure TfrPicProps_View.WMTimer(var Msg: TWMTimer);
  begin
    if Msg.TimerID=IPicPropsViewTimerID then begin
      KillTimer(Handle, IPicPropsViewTimerID);
      LoadViewImage;
    end;
  end;

end.

