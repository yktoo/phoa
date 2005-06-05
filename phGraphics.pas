//**********************************************************************************************************************
//  $Id: phGraphics.pas,v 1.20 2005-06-05 16:36:55 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phGraphics;

interface
uses Windows, SysUtils, Classes, Graphics, GR32, ConsVars, phIntf, phMutableIntf, phNativeIntf;

type
   // Исключение для прерывания загрузки изображения
  ELoadGraphicAborted = class(EAbort);

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

   // Открывает файл и создаёт уменьшенный эскиз на ThumbBitmap
  procedure MakeThumbnail(const sFileName: String; const MaxSize: TSize; StretchFilter: TPhoaStretchFilter; out ImageSize, ThumbSize: TSize; ThumbBitmap: TBitmap32);
   // Открывает файл, создаёт эскиз и возвращает его данные в виде бинарной строки в формате JPEG
  function  GetThumbnailData(const sFileName: String; const MaxSize: TSize; StretchFilter: TPhoaStretchFilter; bJPEGQuality: Byte; out ImageSize, ThumbSize: TSize): String;
   // То же, но в формате 32-bit bitmap
  function  GetBmp32ThumbnailData(const sFileName: String; const MaxSize: TSize; StretchFilter: TPhoaStretchFilter; out ImageSize, ThumbSize: TSize): String;
   // Отрисовывает эскиз на битмэпе в заданном прямоугольнике. В r возвращает фактически использованный для отрисовки
   //   прямоугольник
  procedure PaintThumbnail(const sThumbnailData: String; Rotation: TPicRotation; Flips: TPicFlips; Bitmap32: TBitmap32; var r: TRect); overload;
  procedure PaintThumbnail(Pic: IPhoaPic; Bitmap32: TBitmap32; var r: TRect); overload;
  procedure PaintThumbnail(Pic: IPhoaPic; Bitmap32: TBitmap32); overload;
   // Отрисовывает Bitmap32-данные на битмэпе в заданной позиции
  procedure PaintBmp32Data(const sBmpData: String; Bitmap32: TBitmap32; const p: TPoint);
   // Отрисовывает значок группы на битмэпе в заданной позиции
  procedure PaintGroupIcon(const sBmpData: String; Bitmap32: TBitmap32; const p: TPoint; bSelected: Boolean; App: IPhotoAlbumApp); overload;
  procedure PaintGroupIcon(const sBmpData: String; DC: HDC; const p: TPoint; bSelected: Boolean; App: IPhotoAlbumApp); overload;

   // Создаёт, загружает изображение, и возвращает преобразованное в TBitmap32 изображение
  procedure LoadGraphicFromFile(const sFileName: String; Bitmap32: TBitmap32; const DesiredSize: TSize; out FullSize: TSize; const OnProgress: TProgressEvent);

const
  BColor_Alpha_Transparent = $00;
  BColor_Alpha_Opaque      = $ff;

implementation
uses JPEG, Math, CommCtrl, GraphicEx, phUtils, phIJLIntf;

  procedure RenderShadowTemplate(Bitmap: TBitmap32; iRadius: Integer; bOpacity: Byte; Color: TColor);
  var
    iSize, ix, iy, iy2: Integer;
    c32: TColor32;
    sAlpha, sCoeff: Single;
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
    c32 := Color32(Color);
     // Устанавливаем параметры битмэпа
    Bitmap.SetSize(iSize, iSize);
    Bitmap.DrawMode    := dmBlend;
    Bitmap.MasterAlpha := bOpacity;
     // Рендерим квадрат (3)
    sCoeff := 1/Sqr(iRadius)*Ln(0.013);
    for iy := 0 to iRadius-1 do begin
       // Заранее считаем квадрат iy
      iy2 := Sqr(iy);
      for ix := 0 to iRadius-1 do begin
        sAlpha := Exp(sCoeff*(Sqr(ix)+iy2)); // Находим значение непрозрачности в диапазоне 0..1
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
    r, rTotal, rQuarter, rQuarterMargins: TRect;
    iRadius: Integer;
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

     // Обновляет границы "четвертинок" тени, если соотв. размер "сплошного" региона меньше 0
    procedure CalcQuarterBounds(iSolidSize: Integer; var iLowBound, iHighBound: Integer);
    var iDelta: Integer;
    begin
      if iSolidSize<0 then begin
        iDelta := iSolidSize div 2; // iDelta<=0 !
        Inc(iLowBound,  iDelta);
        Dec(iHighBound, iSolidSize-iDelta); // Это гарантирует, что в сумме границы дадут iSolidSize
      end;
    end;

  begin
     // Сохраняем объект во временный битмэп
    BMPObject := TBitmap32.Create;
    try
      BMPObject.SetSize(rObject.Right-rObject.Left, rObject.Bottom-rObject.Top);
      BMPObject.Draw(0, 0, rObject, Target);
       // Уменьшаем сплошной регион тени на 1/3 радиуса с каждого края (весьма эмпирическое правило)
      iRadius := ShadowTemplate.Width div 2;
      r := rObject;
      with r do begin
        Inc(Left,   iOffsetX+iRadius div 3);
        Inc(Top,    iOffsetY+iRadius div 3);
        Inc(Right,  iOffsetX-iRadius div 3);
        Inc(Bottom, iOffsetY-iRadius div 3);
      end;
       // Рисуем сплошную тень объекта
      Target.FillRectTS(r, SetAlpha(Color32(Color), ShadowTemplate.MasterAlpha));
       // Определяем границы "четвертинок" (они >0, если ширина/высота "сплошного" региона меньше 0)
      FillChar(rQuarterMargins, SizeOf(rQuarterMargins), 0);
      CalcQuarterBounds(r.Right-r.Left, rQuarterMargins.Left, rQuarterMargins.Right);
      CalcQuarterBounds(r.Bottom-r.Top, rQuarterMargins.Top,  rQuarterMargins.Bottom);
       // Считаем общие размеры всей тени
      rTotal := r;
      InflateRect(rTotal, iRadius, iRadius);
       // Рисуем углы тени
       // -- Top left
      rQuarter := Rect(0, 0, iRadius, iRadius);
      Dec(rQuarter.Bottom, rQuarterMargins.Bottom);
      Dec(rQuarter.Right,  rQuarterMargins.Right);
      Target.Draw(rTotal.Left, rTotal.Top, rQuarter, ShadowTemplate);
      Inc(rQuarter.Right,  rQuarterMargins.Right);
       // -- Top right
      OffsetRect(rQuarter, iRadius, 0);
      Dec(rQuarter.Left,   rQuarterMargins.Left);
      Target.Draw(rTotal.Right-(rQuarter.Right-rQuarter.Left), rTotal.Top, rQuarter, ShadowTemplate);
      Inc(rQuarter.Left,   rQuarterMargins.Left);
      Inc(rQuarter.Bottom, rQuarterMargins.Bottom);
       // -- Bottom right
      OffsetRect(rQuarter, 0, iRadius);
      Dec(rQuarter.Top,  rQuarterMargins.Top);
      Dec(rQuarter.Left, rQuarterMargins.Left);
      Target.Draw(rTotal.Right-(rQuarter.Right-rQuarter.Left), rTotal.Bottom-(rQuarter.Bottom-rQuarter.Top), rQuarter, ShadowTemplate);
      Inc(rQuarter.Left, rQuarterMargins.Left);
       // -- Bottom left
      OffsetRect(rQuarter, -iRadius, 0);
      Dec(rQuarter.Right, rQuarterMargins.Right);
      Target.Draw(rTotal.Left, rTotal.Bottom-(rQuarter.Bottom-rQuarter.Top), rQuarter, ShadowTemplate);
      Inc(rQuarter.Right, rQuarterMargins.Right);
      Inc(rQuarter.Top,   rQuarterMargins.Top);
       // Рисуем полоски тени между углами
      if r.Right>r.Left then begin
        DrawHShadow(rTotal.Top,  0);
        DrawHShadow(r.Bottom,    iRadius);
      end;
      if r.Bottom>r.Top then begin
        DrawVShadow(rTotal.Left, 0);
        DrawVShadow(r.Right,     iRadius);
      end;
       // Переносим изображение объекта обратно
      Target.Draw(rObject.Left, rObject.Top, BMPObject);
    finally
      BMPObject.Free;
    end;
  end;

  procedure MakeThumbnail(const sFileName: String; const MaxSize: TSize; StretchFilter: TPhoaStretchFilter; out ImageSize, ThumbSize: TSize; ThumbBitmap: TBitmap32);
  const
     // Таблица перекодировки TPhoaStretchFilter -> GR32.TStretchFilter
    aPhoaSFtoGR32SF: Array[TPhoaStretchFilter] of GR32.TStretchFilter = (
      GR32.sfNearest, GR32.sfDraft, GR32.sfLinear, GR32.sfCosine, GR32.sfSpline, GR32.sfLanczos, GR32.sfMitchell);
  var
    sScale: Single;
    LargeBitmap: TBitmap32;
  begin
    LargeBitmap := TBitmap32.Create;
    try
      LoadGraphicFromFile(sFileName, LargeBitmap, MaxSize, ImageSize, nil);
      LargeBitmap.StretchFilter := aPhoaSFtoGR32SF[StretchFilter];
       // Определяем коэффициент масштабирования
      if (ImageSize.cx>0) and (ImageSize.cy>0) then sScale := MinS(MinS(MaxSize.cx/ImageSize.cx, MaxSize.cy/ImageSize.cy), 1) else sScale := 1;
       // Масштабируем изображение
      ThumbSize.cx := Max(Trunc(ImageSize.cx*sScale), 1);
      ThumbSize.cy := Max(Trunc(ImageSize.cy*sScale), 1);
      ThumbBitmap.SetSize(ThumbSize.cx, ThumbSize.cy);
      LargeBitmap.DrawTo(ThumbBitmap, ThumbBitmap.BoundsRect);
    finally
      LargeBitmap.Free;
    end;
  end;

  function GetThumbnailData(const sFileName: String; const MaxSize: TSize; StretchFilter: TPhoaStretchFilter; bJPEGQuality: Byte; out ImageSize, ThumbSize: TSize): String;
  var
    ThumbBitmap: TBitmap32;
    Stream: TStringStream;
    bmp: TBitmap;
  begin
     // Масштабируем изображение
    ThumbBitmap := TBitmap32.Create;
    try
       // Загружаем уменьшенную версию картинки в ThumbBitmap
      MakeThumbnail(sFileName, MaxSize, StretchFilter, ImageSize, ThumbSize, ThumbBitmap);
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

  function GetBmp32ThumbnailData(const sFileName: String; const MaxSize: TSize; StretchFilter: TPhoaStretchFilter; out ImageSize, ThumbSize: TSize): String;
  var
    ThumbBitmap: TBitmap32;
    Stream: TStringStream;
  begin
     // Масштабируем изображение
    ThumbBitmap := TBitmap32.Create;
    try
       // Загружаем уменьшенную версию картинки в ThumbBitmap
      MakeThumbnail(sFileName, MaxSize, StretchFilter, ImageSize, ThumbSize, ThumbBitmap);
       // Сохраняем эскиз в поток
      Stream := TStringStream.Create('');
      try
        ThumbBitmap.SaveToStream(Stream);
        Result := Stream.DataString;
      finally
        Stream.Free;
      end;
    finally
      ThumbBitmap.Free;
    end;
  end;

var
   // Буферные переменные для отрисовки. Not thread-safe!
  _JPGBuffer: TJPEGImage  = nil;
  _BMPBuffer: TBitmap32   = nil;

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
      if _JPGBuffer=nil then _JPGBuffer := TJPEGImage.Create;
      if _BMPBuffer=nil then _BMPBuffer := TBitmap32.Create;
       // Загружаем JPEG-изображение эскиза
      Stream := TStringStream.Create(sThumbnailData);
      try
        _JPGBuffer.LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
       // Декодируем эскиз
      _BMPBuffer.Assign(_JPGBuffer);
       // Применяем преобразования, если они есть
      if (Rotation<>pr0) or (Flips<>[]) then begin
        Transform := TPicTransform.Create(_BMPBuffer);
        try
          Transform.Rotation := Rotation;
          Transform.Flips    := Flips;
        finally
          Transform.Free;
        end;
      end;
       // Определяем размеры отрисовки
      iw := Min(_BMPBuffer.Width,  r.Right-r.Left);
      ih := Min(_BMPBuffer.Height, r.Bottom-r.Top);
       // Определяем границы для отрисовки эскиза
      rSrc := Bounds(Max(0, (_BMPBuffer.Width-iw) div 2),  Max(0, (_BMPBuffer.Height-ih) div 2), iw, ih);
      r    := Bounds(r.Left+Max(0, (r.Right-r.Left-iw) div 2), r.Top+Max(0, (r.Bottom-r.Top-ih) div 2),  iw, ih);
       // Отрисовываем изображение на битмэпе
      Bitmap32.Draw(r.Left, r.Top, rSrc, _BMPBuffer);
    end;
  end;

  procedure PaintThumbnail(Pic: IPhoaPic; Bitmap32: TBitmap32; var r: TRect);
  begin
    PaintThumbnail(Pic.ThumbnailData, Pic.Rotation, Pic.Flips, Bitmap32, r);
  end;

  procedure PaintThumbnail(Pic: IPhoaPic; Bitmap32: TBitmap32);
  var r: TRect;
  begin
    r := Bitmap32.BoundsRect;
    PaintThumbnail(Pic, Bitmap32, r);
  end;

  procedure PaintBmp32Data(const sBmpData: String; Bitmap32: TBitmap32; const p: TPoint);
  var Stream: TStringStream;
  begin
     // Создаём буферное изображение
    if _BMPBuffer=nil then _BMPBuffer := TBitmap32.Create;
     // Загружаем данные
    Stream := TStringStream.Create(sBmpData);
    try
      _BMPBuffer.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
     // Рисуем картинку на Bitmap32
    Bitmap32.Draw(p.x, p.y, _BMPBuffer);
  end;

  procedure PaintGroupIcon(const sBmpData: String; Bitmap32: TBitmap32; const p: TPoint; bSelected: Boolean; App: IPhotoAlbumApp);
  var i: Integer;
  begin
    Bitmap32.SetSize(16, 16);
     // Если нет данных - рисуем значок папки
    if sBmpData='' then begin
       // Рисуем значок папки на фоне цвета clFuchsia
      Bitmap32.DrawMode := dmOpaque;
      ImageList_DrawEx(App.ImageList.Handle, iif(bSelected, iiFolderOpen, iiFolder), Bitmap32.Canvas.Handle, 0, 0, 0, 0, clFuchsia, CLR_NONE, ILD_NORMAL);
      Bitmap32.ResetAlpha;
       // Заменяем clFuchsia прозрачным цветом
      Bitmap32.DrawMode := dmBlend;
      for i := 0 to 16*16-1 do
        if Bitmap32.Bits[i]=clFuchsia32 then Bitmap32.Bits[i] := 0;
     // Иначе рисуем заданный значок
    end else begin
      Bitmap32.DrawMode := dmBlend;
      PaintBmp32Data(sBmpData, Bitmap32, p);
    end;
  end;

  procedure PaintGroupIcon(const sBmpData: String; DC: HDC; const p: TPoint; bSelected: Boolean; App: IPhotoAlbumApp);
  begin
     // Создаём буферное изображение
    if _BMPBuffer=nil then _BMPBuffer := TBitmap32.Create;
     // Рисуем значок на буферном битмэпе
    PaintGroupIcon(sBmpData, _BMPBuffer, p, bSelected, App);
     // Переносим буферный битмэп на DC
    _BMPBuffer.DrawTo(DC, p.x, p.y);
  end;

  procedure LoadGraphicFromFile(const sFileName: String; Bitmap32: TBitmap32; const DesiredSize: TSize; out FullSize: TSize; const OnProgress: TProgressEvent);
  var
    sExt: String;
    GClass: TGraphicClass;
    Graphic: TGraphic;
  begin
    sExt := ExtractFileExt(sFileName);
     // Если IJL доступна, JPEG грузим с её помощью (якобы 300% faster) 
    if bIJL_Available and (SameText(sExt, '.JPG') or SameText(sExt, '.JPEG')) then begin
      try
        phIJLIntf.LoadJPEGFromFile(Bitmap32, sFileName, DesiredSize, FullSize);
      except
        on e: Exception do PhoaException(ConstVal('SErrCannotLoadPicture', [sFileName, e.Message]));
      end;
     // Остальное - с помощью GraphicEx
    end else begin
      GClass := FileFormatList.GraphicFromExtension(sExt);
      if GClass=nil then PhoaException(ConstVal('SErrUnknownPicFileExtension', [sFileName]));
      Graphic := GClass.Create;
      try
        Graphic.OnProgress := OnProgress;
         // Загружаем изображение
        try
          Graphic.LoadFromFile(sFileName);
        except
          on e: Exception do begin
            FreeAndNil(Graphic);
            if not (e is ELoadGraphicAborted) then PhoaException(ConstVal('SErrCannotLoadPicture', [sFileName, e.Message]));
          end;
        end;
         // Преобразовываем в TBitmap32
        if Graphic<>nil then
          try
            Bitmap32.Assign(Graphic);
            FullSize.cx := Bitmap32.Width;
            FullSize.cy := Bitmap32.Height;
          except
            on e: Exception do
              if not (e is ELoadGraphicAborted) then PhoaException(ConstVal('SErrCannotDecodePicture', [sFileName, e.Message]));
          end;
      finally
        Graphic.Free;
      end;
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
  _JPGBuffer.Free;
  _BMPBuffer.Free;
end.
