//**********************************************************************************************************************
//  $Id: phGraphics.pas,v 1.5 2004-09-23 14:36:15 dale Exp $
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
  procedure RenderShadow(Bitmap: TBitmap32; iRadius: Integer; bOpacity: Byte; Color: TColor);

const
  BColor_Alpha_Transparent = $00;
  BColor_Alpha_Opaque      = $FF;

implementation
uses phUtils;

  procedure RenderShadow(Bitmap: TBitmap32; iRadius: Integer; bOpacity: Byte; Color: TColor);
  var
    iSize, ix, iy, i2R2, iy2: Integer;
    c32: TColor32;
    bAlpha: Byte;
  begin
    iSize := iRadius*2;
    i2R2 := 2*iRadius*iRadius;
    c32 := Color32(Color);
     // Устанавливаем параметры битмэпа
    Bitmap.SetSize(iSize, iSize);
    Bitmap.DrawMode    := dmBlend;
    Bitmap.MasterAlpha := bOpacity;
    for iy := 0 to iRadius-1 do begin
       // Заранее считаем квадрат iy
      iy2 := iy*iy;
      for ix := 0 to iRadius-1 do begin
        if (ix=0) and (iy=0) then bAlpha := $ff else bAlpha := Trunc(255*(1-i2R2/(ix*ix+iy2)));
        Bitmap.SetPixelT(iRadius+ix, iRadius+iy, SetAlpha(c32, bAlpha));
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

end.
