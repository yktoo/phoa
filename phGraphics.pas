//**********************************************************************************************************************
//  $Id: phGraphics.pas,v 1.7 2004-09-24 16:44:29 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phGraphics;

interface
uses Windows, SysUtils, Classes, Graphics, GR32, ConsVars;

type

   // Байтовая карта одного цветового канала (вход -> выход)
  TChannelByteMap = Array[Byte] of Byte;

   //===================================================================================================================
   // Карта цветов по каналам
   //===================================================================================================================

  TColor32Map = class(TObject)
  private
     // Prop storage
    FMapA: TChannelByteMap;
    FMapG: TChannelByteMap;
    FMapR: TChannelByteMap;
    FMapB: TChannelByteMap;
     // Заполняет массив линейными значениями
    procedure ChannelBuildLinear(var Map: TChannelByteMap);
     // Заполняет массив константой
    procedure ChannelBuildConstant(var Map: TChannelByteMap; bValue: Byte);
  public
    constructor Create;
     // Заполняет массив линейными значениями
    procedure BuildLinear;
     // Применяет карту к указанному цвету
    function  ApplyToColor(c: TColor32): TColor32;
     // Props
     // -- Карты каналов
    property MapR: TChannelByteMap read FMapR;
    property MapG: TChannelByteMap read FMapG;
    property MapB: TChannelByteMap read FMapB;
    property MapA: TChannelByteMap read FMapA;
  end;

   //===================================================================================================================
   // Преобразование изображения (набор параметров преобразования)
   //===================================================================================================================

  TPicTransform = class(TObject)
  private
     // Счётчик блокировки применения преобразования
    FApplyLock: Integer;
     // Последнее применённое [абсолютное] значение поворота
    FAppliedRotation: TPicRotation;
     // Последнее применённое [абсолютное] значение отражений
    FAppliedFlips: TPicFlips;
     // Prop storage
    FBitmap: TBitmap32;
    FFlips: TPicFlips;
    FRotation: TPicRotation;
    FOnApplied: TNotifyEvent;
     // Поворачивает изображение на ARotation и отражает в соответствии флагами AFlips
    procedure ApplyRelativeTransform(ARotation: TPicRotation; AFlips: TPicFlips);
     // Применяет преобразования к изображению
    procedure ApplyTransform;
     // Prop handlers
    procedure SetFlips(Value: TPicFlips);
    procedure SetRotation(Value: TPicRotation);
  public
    constructor Create(ABitmap: TBitmap32);
     // Установка/снятие блокировки применения
    procedure BeginUpdate;
    procedure EndUpdate;
     // Инициализирует свойства как исходные (т.е. соответствующие текущему состоянию Bitmap)
    procedure InitValues(ARotation: TPicRotation; AFlips: TPicFlips);
     // Применяет свойства преобразования одновременно
    procedure ApplyValues(ARotation: TPicRotation; AFlips: TPicFlips);
     // Переключает один из флагов отражения
    procedure ToggleFlip(Flip: TPicFlip);
     // Props
     // -- Изображение, к которому применяется преобразование
    property Bitmap: TBitmap32 read FBitmap;
     // -- Отражения изображения относительно исходного
    property Flips: TPicFlips read FFlips write SetFlips;
     // -- Поворот изображения относительно исходного
    property Rotation: TPicRotation read FRotation write SetRotation;
     // -- Событие применения преобразования
    property OnApplied: TNotifyEvent read FOnApplied write FOnApplied;
  end;

   // Отрисовывает на Bitmap круг-заготовку тени с alpha-каналом
  procedure RenderShadowTemplate(Bitmap: TBitmap32; iRadius: Integer; bOpacity: Byte; Color: TColor);
   // Рисует на Target тень из заготовки ShadowTemplate для прямоугольника rObject
  procedure DropShadow(Target, ShadowTemplate: TBitmap32; const rObject, rClipOuter: TRect; iOffsetX, iOffsetY: Integer; Color: TColor);

   // Открывает файл, создаёт эскиз и возвращает его данные в виде бинарной строки
  function  GetThumbnailData(const sFileName: String; iMaxWidth, iMaxHeight: Integer; StretchFilter: TStretchFilter; bJPEGQuality: Byte; out iWidth, iHeight, iThWidth, iThHeight: Integer): String;
   // Отрисовывает эскиз на битмэпе в заданном прямоугольнике. В r возвращает фактически использованный для отрисовки
   //   прямоугольник
  procedure PaintThumbnail(const sThumbnailData: String; Rotation: TPicRotation; Flips: TPicFlips; Bitmap32: TBitmap32; var r: TRect);

const
  BColor_Alpha_Transparent = $00;
  BColor_Alpha_Opaque      = $ff;

implementation
uses JPEG, phUtils;

  procedure RenderShadowTemplate(Bitmap: TBitmap32; iRadius: Integer; bOpacity: Byte; Color: TColor);
  var
    iSize, ix, iy, iR2, iy2: Integer;
    c32: TColor32;
    sAlpha: Single;
    bAlpha: Byte;

     // Отражает "четвертинку" изображения по горизонтали и/или вертикали
    procedure MirrorQuarter(ixq, iyq: Integer; bHorz, bVert: Boolean);
    var
      ixSrc, iySrc: Integer;
      pSrc, pTgt: PColor32;
    begin
       // Цикл по строкам
      for iySrc := iyq to iyq+iRadius-1 do begin
         // Определяем индекс пиксела в начале строк: исходной и целевой
        pSrc := Bitmap.PixelPtr[ixq, iySrc];
        pTgt := Bitmap.PixelPtr[iif(bHorz, iSize-ixq, ixq), iif(bVert, iSize-iySrc, iySrc)];
         // Переписываем пикселы строки в цикле
        for ixSrc := ixq to ixq+iRadius-1 do begin
          pTgt^ := pSrc^;
          Inc(pSrc);
          if bHorz then Dec(pTgt) else Inc(pTgt);
        end;
      end;
    end;

    // +---+---+
    // | 0 | 1 |
    // +---+---+
    // | 2 | 3 |
    // +---+---+

  begin
    iSize := iRadius*2;
    iR2 := iRadius*iRadius*iRadius;
    c32 := Color32(Color);
     // Устанавливаем параметры битмэпа
    Bitmap.SetSize(iSize, iSize);
    Bitmap.DrawMode    := dmBlend;
    Bitmap.MasterAlpha := bOpacity;
     // Рендерим квадрат (3)
    for iy := 0 to iRadius-1 do begin
       // Заранее считаем квадрат iy
      iy2 := iy*iy*iy;
      for ix := 0 to iRadius-1 do begin
        sAlpha := 1-(ix*ix*ix+iy2)/iR2; // Находим значение непрозрачности в диапазоне 0..1
        if sAlpha<0 then bAlpha := 0 else bAlpha := Trunc(255*sAlpha);
        Bitmap.SetPixelT(iRadius+ix, iRadius+iy, SetAlpha(c32, bAlpha));
      end;
    end;
     // Копируем угол в остальные углы
    MirrorQuarter(iRadius, iRadius, False, True);  // (1)
    MirrorQuarter(iRadius, iRadius, True,  False); // (2)
    MirrorQuarter(iRadius, iRadius, True,  True);  // (0)
  end;

  procedure DropShadow(Target, ShadowTemplate: TBitmap32; const rObject, rClipOuter: TRect; iOffsetX, iOffsetY: Integer; Color: TColor);
  var
    r, rTotal, rQuarter: TRect;
    iRadius, iHalfRad: Integer;
    BMPObject: TBitmap32;

     // Рисует полоску тени высотой в iRadius и шириной с r, начиная с вертикальной координаты iy (пикселы берёт с
     //   шаблона с вертикальной координаты iyTempl)
    procedure DrawHShadow(iy, iyTempl: Integer);
    var
      i: Integer;
      c: TColor32;
    begin
      for i := 0 to iRadius-1 do begin
        c := ShadowTemplate[iRadius, iyTempl];
        Target.HorzLineTS(r.Left, iy, r.Right-1, SetAlpha(c, Trunc(AlphaComponent(c)*Integer(ShadowTemplate.MasterAlpha)/255)));
        Inc(iy);
        Inc(iyTempl);
      end;
    end;

     // Рисует полоску тени шириной в iRadius и высотой с r, начиная с горизонтальной координаты ix (пикселы берёт с
     //   шаблона с горизонтальной координаты ixTempl)
    procedure DrawVShadow(ix, ixTempl: Integer);
    var
      i: Integer;
      c: TColor32;
    begin
      for i := 0 to iRadius-1 do begin
        c := ShadowTemplate[ixTempl, iRadius];
        Target.VertLineTS(ix, r.Top, r.Bottom-1, SetAlpha(c, Trunc(AlphaComponent(c)*Integer(ShadowTemplate.MasterAlpha)/255)));
        Inc(ix);
        Inc(ixTempl);
      end;
    end;

  begin
     // Сохраняем объект во временный битмэп
    BMPObject := TBitmap32.Create;
    try
      BMPObject.SetSize(rObject.Right-rObject.Left, rObject.Bottom-rObject.Top);
      BMPObject.Draw(0, 0, rObject, Target); 
       // Уменьшаем сплошной регион тени на 1/2 радиуса с каждого края (весьма эмпирическое правило)
      iRadius := ShadowTemplate.Width div 2;
      iHalfRad := iRadius div 2;
      r := rObject;
      with r do begin
        Inc(Left,   iOffsetX+iHalfRad);
        Inc(Top,    iOffsetY+iHalfRad);
        Inc(Right,  iOffsetX-iHalfRad);
        Inc(Bottom, iOffsetY-iHalfRad);
      end;
       // Считаем общие размеры всей тени
      rTotal := r;
      InflateRect(rTotal, iRadius, iRadius);
       // Рисуем сплошную тень объекта
      Target.FillRectTS(r, SetAlpha(Color32(Color), ShadowTemplate.MasterAlpha));
       // Рисуем углы тени
       // -- Top left
      rQuarter := Rect(0, 0, iRadius, iRadius);
      Target.Draw(rTotal.Left, rTotal.Top, rQuarter, ShadowTemplate);
       // -- Top right
      OffsetRect(rQuarter, iRadius, 0);
      Target.Draw(rTotal.Right-iRadius, rTotal.Top, rQuarter, ShadowTemplate);
       // -- Bottom right
      OffsetRect(rQuarter, 0, iRadius);
      Target.Draw(rTotal.Right-iRadius, rTotal.Bottom-iRadius, rQuarter, ShadowTemplate);
       // -- Bottom left
      OffsetRect(rQuarter, -iRadius, 0);
      Target.Draw(rTotal.Left, rTotal.Bottom-iRadius, rQuarter, ShadowTemplate);
       // Рисуем полоски тени между углами
      DrawHShadow(rTotal.Top,  0);
      DrawHShadow(r.Bottom,    iRadius);
      DrawVShadow(rTotal.Left, 0);
      DrawVShadow(r.Right,     iRadius);
       // Переносим изображение объекта обратно
      Target.Draw(rObject.Left, rObject.Top, BMPObject);
    finally
      BMPObject.Free;
    end;
  end;

  function GetThumbnailData(const sFileName: String; iMaxWidth, iMaxHeight: Integer; StretchFilter: TStretchFilter; bJPEGQuality: Byte; out iWidth, iHeight, iThWidth, iThHeight: Integer): String;
  var
    sScale: Single;
    ThumbBitmap, FullSizeBitmap: TBitmap32;
    Stream: TStringStream;
    bmp: TBitmap;
  begin
     // Масштабируем изображение
    ThumbBitmap := TBitmap32.Create;
    try
       // Создаём, загружаем картинку и превращаем её в Bitmap32
      FullSizeBitmap := LoadGraphicFromFile(sFileName);
      try
        FullSizeBitmap.StretchFilter := StretchFilter;
        iWidth  := FullSizeBitmap.Width;
        iHeight := FullSizeBitmap.Height;
         // Определяем коэффициент масштабирования
        if (iWidth>0) and (iHeight>0) then sScale := MinS(MinS(iMaxWidth/iWidth, iMaxHeight/iHeight), 1) else sScale := 1;
         // Масштабируем изображение
        iThWidth  := Max(Trunc(iWidth*sScale), 1);
        iThHeight := Max(Trunc(iHeight*sScale), 1);
        ThumbBitmap.SetSize(iThWidth, iThHeight);
        FullSizeBitmap.DrawTo(ThumbBitmap, ThumbBitmap.BoundsRect);
      finally
        FullSizeBitmap.Free;
      end;
       // Преобразуем TBitmap32 в TBitmap
      bmp := TBitmap.Create;
      try
        bmp.Assign(ThumbBitmap);
         // Преобразуем TBitmap в TJPEGImage
        with TJPEGImage.Create do
          try
             // Копируем эскиз
            Assign(bmp);
             // Сжимаем
            CompressionQuality := bJPEGQuality;
            Compress;
             // Сохраняем эскиз в поток
            Stream := TStringStream.Create('');
            try
              SaveToStream(Stream);
              Result := Stream.DataString;
            finally
              Stream.Free;
            end;
          finally
            Free;
          end;
      finally
        bmp.Free;
      end;
    finally
      ThumbBitmap.Free;
    end;
  end;

var
   // Буферные переменные для отрисовки эскизов. Not thread-safe!
  JPGThumbBuffer: TJPEGImage = nil;
  BMPThumbBuffer: TBitmap32  = nil;

  procedure PaintThumbnail(const sThumbnailData: String; Rotation: TPicRotation; Flips: TPicFlips; Bitmap32: TBitmap32; var r: TRect);
  var
    Stream: TStringStream;
    Transform: TPicTransform;
    iw, ih: Integer;
    rSrc: TRect;
  begin
    if sThumbnailData='' then
      FillChar(r, SizeOf(r), 0)
    else begin
       // Создаём буферные изображения
      if JPGThumbBuffer=nil then JPGThumbBuffer := TJPEGImage.Create;
      if BMPThumbBuffer=nil then BMPThumbBuffer := TBitmap32.Create;
       // Загружаем JPEG-изображение эскиза
      Stream := TStringStream.Create(sThumbnailData);
      try
        JPGThumbBuffer.LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
       // Декодируем эскиз
      BMPThumbBuffer.Assign(JPGThumbBuffer);
       // Применяем преобразования, если они есть
      if (Rotation<>pr0) or (Flips<>[]) then begin
        Transform := TPicTransform.Create(BMPThumbBuffer);
        try
          Transform.Rotation := Rotation;
          Transform.Flips    := Flips;
        finally
          Transform.Free;
        end;
      end;
       // Определяем размеры отрисовки
      iw := Min(BMPThumbBuffer.Width,  r.Right-r.Left);
      ih := Min(BMPThumbBuffer.Height, r.Bottom-r.Top);
       // Определяем границы для отрисовки эскиза
      rSrc := Bounds(Max(0, (BMPThumbBuffer.Width-iw) div 2),  Max(0, (BMPThumbBuffer.Height-ih) div 2), iw, ih);
      r    := Bounds(r.Left+Max(0, (r.Right-r.Left-iw) div 2), r.Top+Max(0, (r.Bottom-r.Top-ih) div 2),  iw, ih);
       // Отрисовываем изображение на битмэпе
      Bitmap32.Draw(r.Left, r.Top, rSrc, BMPThumbBuffer);
    end;
  end;

   //===================================================================================================================
   // TColor32Map
   //===================================================================================================================

  function TColor32Map.ApplyToColor(c: TColor32): TColor32;
  asm
     // EAX = Self
     // EDX = C
     // out: EAX
    mov ebx, edx
    xor edx, edx
     // Put B value
    mov dl, bl
    mov dl, byte ptr [eax+FMapB+edx]
    mov byte ptr [Result+0], dl
     // Put G value
    mov dl, bh
    mov dl, byte ptr [eax+FMapG+edx]
    mov byte ptr [Result+1], dl
     // Put R value
    bswap ebx
    mov dl, bh
    mov dl, byte ptr [eax+FMapR+edx]
    mov byte ptr [Result+2], dl
     // Put A value
    mov dl, bl
    mov dl, byte ptr [eax+FMapA+edx]
    mov byte ptr [Result+3], dl
  end;

  procedure TColor32Map.BuildLinear;
  begin
     // Заполняем цветовые массивы линейными значениями
    ChannelBuildLinear(FMapR);
    ChannelBuildLinear(FMapG);
    ChannelBuildLinear(FMapB);
     // Альфа-канал делаем непрозрачным
    ChannelBuildConstant(FMapA, BColor_Alpha_Opaque);
  end;

  procedure TColor32Map.ChannelBuildConstant(var Map: TChannelByteMap; bValue: Byte);
  begin
    FillChar(Map, SizeOf(Map), bValue);
  end;

  procedure TColor32Map.ChannelBuildLinear(var Map: TChannelByteMap);
  var b: Byte;
  begin
    for b := Low(b) to High(b) do Map[b] := b;
  end;

  constructor TColor32Map.Create;
  begin
    inherited Create;
    BuildLinear;
  end;

   //===================================================================================================================
   // TPicTransform
   //===================================================================================================================

  procedure TPicTransform.ApplyRelativeTransform(ARotation: TPicRotation; AFlips: TPicFlips);
  begin
     // Поворот на 180° и оба флипа дают исходное изображение
    if (FBitmap<>nil) and ((ARotation<>pr180) or (AFlips<>[pflHorz, pflVert])) then begin
      case ARotation of
        pr90:  FBitmap.Rotate90;
        pr180: FBitmap.Rotate180;
        pr270: FBitmap.Rotate270;
      end;
      if pflHorz in AFlips then FBitmap.FlipHorz;
      if pflVert in AFlips then FBitmap.FlipVert;
    end;
  end;

  procedure TPicTransform.ApplyTransform;
  var
    fl: TPicFlip;
    NewRotation: TPicRotation;
    NewFlips: TPicFlips;
  begin
    if FBitmap<>nil then begin
       // Вычисляем разницу в повороте и кладём её в NewRotation
      NewRotation := FRotation;
      if NewRotation<FAppliedRotation then Inc(NewRotation, Byte(Succ(High(NewRotation))));
      Dec(NewRotation, Byte(FAppliedRotation));
       // -- Если есть один из флипов, вращаем в противоположную сторону
      if (FAppliedFlips<>[]) and (FAppliedFlips<>[pflHorz, pflVert]) then Byte(NewRotation) := Byte(Succ(High(NewRotation)))-Byte(NewRotation);
       // Вычисляем разницу в отражениях и кладём её в NewFlips
      NewFlips := FFlips;
      for fl := Low(fl) to High(fl) do
        if (fl in FAppliedFlips)=(fl in NewFlips) then Exclude(NewFlips, fl) else Include(NewFlips, fl);
       // Применяем разницу в преобразованиях
      ApplyRelativeTransform(NewRotation, NewFlips);
    end;
     // Сохраняем текущие значение как последние применённые
    FAppliedRotation := FRotation;
    FAppliedFlips    := FFlips;
     // Вызываем событие OnApplied
    if Assigned(FOnApplied) then FOnApplied(Self); 
  end;

  procedure TPicTransform.ApplyValues(ARotation: TPicRotation; AFlips: TPicFlips);
  begin
    if (FRotation<>ARotation) or (FFlips<>AFlips) then begin
      FRotation := ARotation;
      FFlips    := AFlips;
      if FApplyLock=0 then ApplyTransform;
    end;
  end;

  procedure TPicTransform.BeginUpdate;
  begin
    Inc(FApplyLock);
  end;

  constructor TPicTransform.Create(ABitmap: TBitmap32);
  begin
    inherited Create;
    FBitmap   := ABitmap;
  end;

  procedure TPicTransform.EndUpdate;
  begin
    if FApplyLock>0 then begin
      Dec(FApplyLock);
      if FApplyLock=0 then ApplyTransform;
    end;
  end;

  procedure TPicTransform.InitValues(ARotation: TPicRotation; AFlips: TPicFlips);
  begin
    FRotation        := ARotation;
    FAppliedRotation := ARotation;
    FFlips           := AFlips;
    FAppliedFlips    := AFlips;
  end;

  procedure TPicTransform.SetFlips(Value: TPicFlips);
  begin
    if FFlips<>Value then begin
      FFlips := Value;
      if FApplyLock=0 then ApplyTransform;
    end;
  end;

  procedure TPicTransform.SetRotation(Value: TPicRotation);
  begin
    if FRotation<>Value then begin
      FRotation := Value;
      if FApplyLock=0 then ApplyTransform;
    end;
  end;

  procedure TPicTransform.ToggleFlip(Flip: TPicFlip);
  begin
    if Flip in FFlips then Exclude(FFlips, Flip) else Include(FFlips, Flip);
    if FApplyLock=0 then ApplyTransform;
  end;

initialization
finalization
  JPGThumbBuffer.Free;
  BMPThumbBuffer.Free;
end.
