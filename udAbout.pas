//**********************************************************************************************************************
//  $Id: udAbout.pas,v 1.9 2004-09-11 17:52:36 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit udAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, GR32, GR32_Layers, ConsVars, 
  StdCtrls, GR32_Image, ExtCtrls, DKLang;

type
  TShowDetail = (sdTitle, sdAuthor, sdCredits);

  TdAbout = class(TForm)
    iMain: TImage32;
    lWebsite: TLabel;
    lOK: TLabel;
    TheTimer: TTimer;
    dklcMain: TDKLanguageController;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lWebsiteClick(Sender: TObject);
    procedure lOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TheTimerTick(Sender: TObject);
    procedure iMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure iMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure iMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
     // Слой, на котором рисуется заголовок
    FTitleLayer: TPositionedLayer;
     // Слой, на котором рисуется прогресс [при DialogMode=False]
    FProgressLayer: TPositionedLayer;
     // Картинки заголовка
    FBmpTitle, FBmpAuthor, FBmpCredits: TBitmap32;
     // Строка состояния прогресса [при DialogMode=False]
    FCurProgressStage: String;
     // Текущие отображаемые данные
    FShowDetail: TShowDetail;
     // Флаг отрисовки анимации
    FFading: Boolean;
     // Флаги необходимости остановки/прерывания анимации
    FStopAnimation: Boolean;
    FAnimationTerminated: Boolean;
     // Поля для перетаскивания формы
    FTrackDrag: Boolean;
    FTrackPos: TPoint;
     // Prop handlers
    FAnimateFadeout: Boolean;
    FDialogMode: Boolean;
     // Инициализация диалога
    procedure DoInitialize;
     // Зажигает или гасит заголовок
    procedure FadeTitle(bIn: Boolean);
     // События слоёв
    procedure PaintTitleLayer(Sender: TObject; Buffer: TBitmap32);
    procedure PaintProgressLayer(Sender: TObject; Buffer: TBitmap32);
     // Создаёт TBitmap32 и загружает его из PNG-ресурса
    function  CreatePNGBitmap(const sResourceName: String; bAlpha: Byte): TBitmap32;
     // Возвращает текущий отображаемый Bitmap заголовка
    function  CurBitmap: TBitmap32;
     // Начинает закрытие окошка [в режиме DialogMode]
    procedure StartClosing;
  public
     // Создаёт диалог
    constructor Create(bDialogMode: Boolean); reintroduce;
     // Отображает заданную стадию прогресса [НЕ в режиме DialogMode]
    procedure DisplayStage(const sStage: String);
     // Скрывает окно
    procedure HideWindow;
     // Props
     // -- Если False - это диалог "О программе"; если True - индикатор прогресса загрузки
    property DialogMode: Boolean read FDialogMode;
     // -- Если True - диалог "плавно растворяется" при закрытии, иначе - закрывается сразу
    property AnimateFadeout: Boolean read FAnimateFadeout write FAnimateFadeout;
  end;

   // Создаёт, отображает и возвращает окно отображения состояния прогресса
  procedure CreateProgressWnd;
   // Отображает модальный диалог "О программе"
  procedure ShowAbout(bAnimateFadeout: Boolean);
   // Отображает прогресс загрузки, если окно прогресса отображается
  procedure ShowProgressInfo(const sConstName: String; const aParams: array of const);
   // Уничтожает окно прогресса, если оно есть
  procedure HideProgressWnd;
   // Если есть окошко прогресса, держит окно Wnd позади него
  procedure KeepBehindProgressWnd(Wnd: HWND);

const
   // Высота области для отрисовки строки прогресса (в нижней части окна)
  IProgressAreaHeight = 16;

implementation
{$R *.dfm}
uses GraphicEx, phUtils, phSettings;

var
  ProgressWnd: TdAbout;

  procedure CreateProgressWnd;
  begin
    ProgressWnd := TdAbout.Create(False);
    try
      ProgressWnd.Show;
    except
      ProgressWnd.Free;
      raise;
    end;
  end;

  procedure ShowAbout(bAnimateFadeout: Boolean);
  begin
    with TdAbout.Create(True) do
      try
        AnimateFadeout := bAnimateFadeout;
        ShowModal;
      finally
        Free;
      end;
  end;

  procedure ShowProgressInfo(const sConstName: String; const aParams: array of const);
  begin
    if ProgressWnd<>nil then ProgressWnd.DisplayStage(ConstVal(sConstName, aParams));
  end;

  procedure HideProgressWnd;
  begin
    if ProgressWnd<>nil then begin
      ProgressWnd.DisplayStage('');
      ProgressWnd.AnimateFadeout := SettingValueBool(ISettingID_Dlgs_SplashStartFade);
      ProgressWnd.HideWindow;
    end;
  end;

  procedure KeepBehindProgressWnd(Wnd: HWND);
  begin
    if ProgressWnd<>nil then SetWindowPos(Wnd, ProgressWnd.Handle, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
  end;

  constructor TdAbout.Create(bDialogMode: Boolean);
  begin
    inherited Create(Application);
    FDialogMode := bDialogMode;
    DoInitialize;
  end;

  function TdAbout.CreatePNGBitmap(const sResourceName: String; bAlpha: Byte): TBitmap32;
  var Png: TPNGGraphic;
  begin
    Png := TPNGGraphic.Create;
    try
      Png.LoadFromResourceName(HInstance, sResourceName);
      Result := TBitmap32.Create;
      Result.Assign(Png);
      Result.DrawMode := dmBlend;
      Result.MasterAlpha := bAlpha;
    finally
      Png.Free;
    end;
  end;

  function TdAbout.CurBitmap: TBitmap32;
  begin
    case FShowDetail of
      sdTitle:  Result := FBmpTitle;
      sdAuthor: Result := FBmpAuthor;
      else      Result := FBmpCredits;
    end;
  end;

  procedure TdAbout.DisplayStage(const sStage: String);
  begin
    FCurProgressStage := sStage;
    iMain.Invalidate;
    Update;
  end;

  procedure TdAbout.DoInitialize;
  var Png: TPNGGraphic;
  begin
    Png := TPNGGraphic.Create;
    try
       // Загружаем изображение фона
      Png.LoadFromResourceName(HInstance, 'PNG_BACK');
      iMain.Bitmap.Assign(Png);
    finally
      Png.Free;
    end;
     // Создаём слой заголовка
    FTitleLayer := TPositionedLayer.Create(iMain.Layers);
    FTitleLayer.OnPaint := PaintTitleLayer;
     // Создаём слой сообщений о прогрессе
    if not DialogMode then begin
      FProgressLayer := TPositionedLayer.Create(iMain.Layers);
      FProgressLayer.OnPaint := PaintProgressLayer;
    end;
     // Загружаем изображения заголовков
    FBmpTitle     := CreatePNGBitmap('PNG_TITLE',   255);
    if DialogMode then begin
      FBmpAuthor  := CreatePNGBitmap('PNG_AUTHOR',  0);
      FBmpCredits := CreatePNGBitmap('PNG_CREDITS', 0);
    end;
     // Разрешаем таймер смены заголовков для режима диалога "О программе"
    TheTimer.Enabled := DialogMode;
    lOK.Visible      := DialogMode;
  end;

  procedure TdAbout.FadeTitle(bIn: Boolean);
  var
    i, iWait: Integer;
    cStart: Cardinal;
  begin
    FFading := True;
    cStart := GetTickCount;
    for i := 0 to 255 do begin
      CurBitmap.MasterAlpha := iif(bIn, i, 255-i);
      iMain.Invalidate;
      Application.ProcessMessages;
      if FAnimationTerminated then Break;
       // Растягиваем показ на 2 секунды
      iWait := (i*2000) div 255-Integer(GetTickCount-cStart);
      if iWait>0 then Sleep(iWait);
    end;
    FFading := False;
     // Если прервали анимацию - надо закрыть окно
    if FAnimationTerminated then HideWindow;
  end;

  procedure TdAbout.FormDestroy(Sender: TObject);
  begin
    FBmpTitle.Free;
    FBmpAuthor.Free;
    FBmpCredits.Free;
    ProgressWnd := nil;
  end;

  procedure TdAbout.FormKeyPress(Sender: TObject; var Key: Char);
  begin
    if DialogMode and (Key in [#13, #27]) then StartClosing;
  end;

  procedure TdAbout.HideWindow;
  var b: Byte;
  begin
     // Enable fadeout effect on Win2K+
    if FAnimateFadeout and (Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>=5) then begin
      AlphaBlend := True;
      Update;
      for b := 12 downto 1 do begin
        AlphaBlendValue := b*20;
        Sleep(20);
      end;
    end;
    Close;
    if not DialogMode then Release;
  end;

  procedure TdAbout.iMainMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
    if DialogMode then begin
      FTrackDrag := True;
      FTrackPos := Point(x, y);
    end;
  end;

  procedure TdAbout.iMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
    if FTrackDrag then
      with iMain.ClientToScreen(Point(x-FTrackPos.x, y-FTrackPos.y)) do SetBounds(x, y, Width, Height);
  end;

  procedure TdAbout.iMainMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  begin
    FTrackDrag := False;
  end;

  procedure TdAbout.lOKClick(Sender: TObject);
  begin
    StartClosing;
  end;

  procedure TdAbout.lWebsiteClick(Sender: TObject);
  begin
    DKWeb.Open_Index;
  end;

  procedure TdAbout.PaintProgressLayer(Sender: TObject; Buffer: TBitmap32);
  begin
    Buffer.Textout(Rect(0, Height-IProgressAreaHeight, Width, Height), DT_CENTER or DT_VCENTER or DT_NOPREFIX, FCurProgressStage);
  end;

  procedure TdAbout.PaintTitleLayer(Sender: TObject; Buffer: TBitmap32);
  var bmp: TBitmap32;
  begin
    bmp := CurBitmap;
    if bmp<>nil then Buffer.Draw((Width-bmp.Width) div 2, (Height-bmp.Height) div 2, bmp);
  end;

  procedure TdAbout.StartClosing;
  begin
    if FFading then FAnimationTerminated := True else HideWindow;
  end;

  procedure TdAbout.TheTimerTick(Sender: TObject);
  begin
    TheTimer.Enabled := False;
     // Стираем надпись
    FadeTitle(False);
    if FAnimationTerminated then Exit;
     // Показываем новую надпись
    FShowDetail := Succ(FShowDetail);
    if FShowDetail>High(FShowDetail) then begin
      FShowDetail := sdTitle;
      FStopAnimation := True;
    end;
    FadeTitle(True);
    if FStopAnimation or FAnimationTerminated then Exit;
     // Запускаем таймер
    TheTimer.Enabled := True;
  end;

end.

