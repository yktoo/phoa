//**********************************************************************************************************************
//  $Id: phUtils.pas,v 1.15 2004-06-03 20:33:39 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phUtils;

interface
uses
  SysUtils, Windows, Messages, Classes, Controls, Graphics, GraphicEx, GR32, StdCtrls, ConsVars, VirtualTrees, VirtualShellUtilities;

   // Exception raising
  procedure PhoaException(const sMsg: String; const aParams: Array of const);

   // Min/max values
  function Max(i1, i2: Integer): Integer;
  function Min(i1, i2: Integer): Integer;
  function MinS(s1, s2: Single): Single;
  function MaxS(s1, s2: Single): Single;

  procedure RegLoadHistory(const sSection: String; cb: TComboBox; bSetLastItem: Boolean);
  procedure RegSaveHistory(const sSection: String; cb: TComboBox; bRegisterFirst: Boolean);
  procedure RegisterCBHistory(cb: TComboBox);

   // Преобразование TRect<->Строка вида '1,2,3,4'
  function  RectToStr(const r: TRect): String;
  function  StrToRect(const s: String; const rDefault: TRect): TRect;
  
   // Работа с описанием шрифта в виде "Name/Size/Style/Color/Charset"
  function  FontToStr(Font: TFont): String;
  procedure FontFromStr(Font: TFont; const sFont: String);

   // Транслирует Charset в кодовую страницу
  function  CharsetToCP(Charset: TFontCharset): Cardinal;
   // Преобразует Ansi-строку в Unicode-строку, используя указанную кодовую страницу и наоборот
  function  AnsiToUnicodeCP(const s: AnsiString; cCodePage: Cardinal): WideString;
  function  UnicodeToAnsiCP(const s: WideString; cCodePage: Cardinal): AnsiString;

   // Заменяет вхождения символов sReplaceChars в строке s на символ cReplaceWith и возвращает результат
  function  ReplaceChars(const s, sReplaceChars: String; cReplaceWith: Char): String;
   // Возвращает первое слово из строки s, считая за разделители слов любой из символов в sDelimiters. Если разделителя
   //   в строке нет, возвращается вся строка
  function  GetFirstWord(const s, sDelimiters: String): String;
   // Извлекает и возвращает первое слово из строки s, считая за разделители слов любой из символов в sDelimiters. Из
   //   строки s слово вместе с разделителем удаляется. Если разделителя в строке нет, возвращается вся строка
  function  ExtractFirstWord(var s: String; const sDelimiters: String): String;
   // Добавляет к строке s строку sAdd. Если обе они не пустые, между ними вставляется sSeparator
  procedure AccumulateStr(var s: String; const sSeparator, sAdd: String);
   // Функция сравнения строк с конца *без учёта регистра* (для имён файлов работает быстрее сравнения с начала)
  function  ReverseCompare(const s1, s2: String): Boolean;
   // Преобразует относительный путь к файлу в абсолютный, используя базовый каталог sBasePath
  function  ExpandRelativePath(const sBasePath, sRelFileName: String): String;
   // Возвращает строку, представляющую собой наиболее длинную совпадающую часть обоих строк. Сравнение ведётся без
   //   учёта регистра
  function  LongestCommonPart(const s1, s2: String): String;

  function  iif(b: Boolean; const sTrue, sFalse: String): String; overload;
  function  iif(b: Boolean; iTrue, iFalse: Integer): Integer;     overload;
  function  iif(b: Boolean; pTrue, pFalse: Pointer): Pointer;     overload;
  function  iif(b: Boolean; sTrue, sFalse: Single): Single; overload;

   // Преобразует размер файла в удобочитаемую форму
  function  HumanReadableSize(iSize: Integer): String; overload;
  function  HumanReadableSize(i64Size: Int64): String; overload;

   // Проверяет ввод текста, представляющего из себя дату (bTime=False) или время (bTime=True). В случае удачи результат
   //   помещает в dtResult и возвращает True, иначе отображает сообщение об ошибке и возвращает False. Если sText -
   //   пустая маска, то в dtResult возвращается -1
  function  CheckMaskedDateTime(const sText: String; bTime: Boolean; var dtResult: TDateTime): Boolean;

   // Возвращает значение константы по её наименованию из fMain.dtlsMain
  function  ConstVal(const sConstName: String): String; overload;
  function  ConstVal(const sConstName: String; const aParams: Array of const): String; overload;
   // Возвращает sText, если он начинается на символ, отличный от '@'. Иначе - трактует текст после '@' как имя
   //   константы и возвращает её значение
  function  ConstValEx(const sText: String): String;

   // Преобразование TPicProperties <-> Integer
  function  PicPropsToInt(PicProps: TPicProperties): Integer;
  function  IntToPicProps(i: Integer): TPicProperties;
   // Возвращает наименование свойства изображения
  function  PicPropName(PicProp: TPicProperty): String;
   // Возвращает текст отражения изображения
  function  PicFlipText(PicFlip: TPicFlip): String;
   // Возвращает текст отражений изображения
  function  PicFlipsText(PicFlips: TPicFlips): String;
   // Возвращает наименование свойства изображения для группировки
  function  GroupByPropName(GBProp: TGroupByProperty): String;
   // Возвращает наименование пиксельного формата изображения
  function  PixelFormatName(PFmt: TPixelFormat): String;
   // Возвращает наименование свойства дискового файла
  function  DiskFilePropName(DFProp: TDiskFileProp): String;
   // Возвращает значение свойства дискового файла по объекту TNamespace файла. Если Namespace=nil, возвращает пустую строку
  function  DiskFilePropValue(DFProp: TDiskFileProp; Namespace: TNamespace): String;
   // Возвращает наименование свойства для автозаполнения даты/времени изображения
  function  DateTimeAutofillPropName(DTAProp: TDateTimeAutofillProp): String;
   // Возвращает текст результата автозаполнения даты/времени изображения
  function  DateTimeFillResultName(DTFResult: TDateTimeFillResult): String;

   // Разрешает или запрещает контрол, перекрашивая его в clWindow или clBtnFace соответственно
  procedure EnableWndCtl(Ctl: TWinControl; bEnable: Boolean);

   // Создаёт, загружает изображение, и возвращает преобразованное в TBitmap32 изображение
   // -- Версия для существующего экземпляра TBitmap32
  procedure LoadGraphicFromFile(const sFileName: String; Bitmap: TBitmap32); overload;
   // -- Версия, создающая новый экземпляр TBitmap32
  function  LoadGraphicFromFile(const sFileName: String): TBitmap32; overload;

   // Фокусирует и выделяет узел и проматывает дерево так, чтобы он был виден. Возвращает True, если Node<>nil
  function  ActivateVTNode(Tree: TBaseVirtualTree; Node: PVirtualNode): Boolean;
   // Сохранение/загрузка настроек столбцов VirtualTree
  procedure RegSaveVTColumns(const sSection: String; Tree: TVirtualStringTree);
  procedure RegLoadVTColumns(const sSection: String; Tree: TVirtualStringTree);
   // Устанавливает опцию coVisible столбца согласно bVisible
  procedure SetVTColumnVisible(Column: TVirtualTreeColumn; bVisible: Boolean);

   // Возвращает True, если iID содержится в массиве aIDs
  function  IDInArray(iID: Integer; const aIDs: TIDArray): Boolean;

   // Укорачивает строку как имя файла
  function  ShortenFileName(Canvas: TCanvas; iWidth: Integer; const s: String): String;

   // Отображает системное контекстное меню для заданного файла. Возвращает True, если удалось
  function  ShowFileShellContextMenu(const sFileName: String): Boolean;

   // Показ/скрытие курсора HourGlass
  procedure StartWait;
  procedure StopWait;

   // Запускает браузер с домашней страницей PhoA
  procedure OpenWebsite;

implementation
uses Forms, Main, TypInfo, Registry, ShellAPI, phSettings, udMsgBox;

  procedure PhoaException(const sMsg: String; const aParams: Array of const);

     function RetAddr: Pointer;
     asm
       mov eax, [ebp+4]
     end;

  begin
    raise EPhoaException.CreateFmt(sMsg, aParams) at RetAddr;
  end;

  function Max(i1, i2: Integer): Integer;
  begin
    if i1>i2 then Result := i1 else Result := i2;
  end;

  function Min(i1, i2: Integer): Integer;
  begin
    if i1<i2 then Result := i1 else Result := i2;
  end;

  function MinS(s1, s2: Single): Single;
  begin
    if s1<s2 then Result := s1 else Result := s2;
  end;

  function MaxS(s1, s2: Single): Single;
  begin
    if s1>s2 then Result := s1 else Result := s2;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // History
   //-------------------------------------------------------------------------------------------------------------------

  procedure RegLoadHistory(const sSection: String; cb: TComboBox; bSetLastItem: Boolean);
  var
    sl: TStringList;
    i: Integer;
  begin
    sl := TStringList.Create;
    try
      with TRegIniFile.Create(SRegRoot) do
        try
          ReadSectionValues(sSection, sl);
        finally
          Free;
        end;
      cb.Clear;
      for i := 0 to sl.Count-1 do cb.Items.Add(sl.ValueFromIndex[i]);
    finally
      sl.Free;
    end;
    with cb do
      if bSetLastItem and (Items.Count>0) then ItemIndex := 0;
  end;

  procedure RegSaveHistory(const sSection: String; cb: TComboBox; bRegisterFirst: Boolean);
  var i: Integer;
  begin
    if bRegisterFirst then RegisterCBHistory(cb);
    with TRegIniFile.Create(SRegRoot) do
      try
        EraseSection(sSection);
        for i := 0 to cb.Items.Count-1 do WriteString(sSection, 'Item'+IntToStr(i), cb.Items[i]);
      finally
        Free;
      end;
  end;

  procedure RegisterCBHistory(cb: TComboBox);
  var
    idx: Integer;
    s: String;
  begin
    s := Trim(cb.Text);
    if s<>'' then
      with cb.Items do begin
        idx := IndexOf(s);
        if idx>=0 then Delete(idx);
        Insert(0, s);
        while Count>IMaxHistoryEntries do Delete(IMaxHistoryEntries);
      end;
    cb.Text := s;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Rectangles
   //-------------------------------------------------------------------------------------------------------------------

  function  RectToStr(const r: TRect): String;
  begin
    Result := Format('%d,%d,%d,%d', [r.Left, r.Top, r.Right, r.Bottom]);
  end;

  function  StrToRect(const s: String; const rDefault: TRect): TRect;
  var ss: String;
  begin
    ss := s;
    try
      Result.Left   := StrToInt(ExtractFirstWord(ss, ','));
      Result.Top    := StrToInt(ExtractFirstWord(ss, ','));
      Result.Right  := StrToInt(ExtractFirstWord(ss, ','));
      Result.Bottom := StrToInt(ExtractFirstWord(ss, ','));
    except
      on EConvertError do Result := rDefault;
    end;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Fonts / chars
   //-------------------------------------------------------------------------------------------------------------------

  function FontToStr(Font: TFont): String;
  begin
    with Font do Result := Format('%s/%d/%d/%d/%d', [Name, Size, Byte(Style), Color, Charset]);
  end;

  procedure FontFromStr(Font: TFont; const sFont: String);
  var s: String;
  begin
    s := sFont;
    with Font do begin
      Name    := ExtractFirstWord(s, '/');
      Size    := StrToIntDef(ExtractFirstWord(s, '/'), 10);
      Style   := TFontStyles(Byte(StrToIntDef(ExtractFirstWord(s, '/'), 0)));
      Color   := StrToIntDef(ExtractFirstWord(s, '/'), 0);
      Charset := StrToIntDef(ExtractFirstWord(s, '/'), DEFAULT_CHARSET);
    end;
  end;

  function CharsetToCP(Charset: TFontCharset): Cardinal;
  begin
    case Charset of
      VIETNAMESE_CHARSET,
        ANSI_CHARSET:      Result := 1252; // Windows 3.1 Latin 1 (US, Western Europe) or Vietnam
      SHIFTJIS_CHARSET:    Result := 932;  // Japan
      HANGEUL_CHARSET,
        JOHAB_CHARSET:     Result := 949;  // Korean
      GB2312_CHARSET:      Result := 936;  // Chinese (PRC, Singapore)
      CHINESEBIG5_CHARSET: Result := 950;  // Chinese (Taiwan, Hong Kong)
      HEBREW_CHARSET:      Result := 1255; // Hebrew
      ARABIC_CHARSET:      Result := 1256; // Arabic
      GREEK_CHARSET:       Result := 1253; // Windows 3.1 Greek
      TURKISH_CHARSET:     Result := 1254; // Windows 3.1 Turkish
      THAI_CHARSET:        Result := 874;  // Thai
      EASTEUROPE_CHARSET:  Result := 1250; // Windows 3.1 Eastern European
      RUSSIAN_CHARSET:     Result := 1251; // Windows 3.1 Cyrillic
      BALTIC_CHARSET:      Result := 1257; // Baltic
      SYMBOL_CHARSET:      Result := CP_SYMBOL;
      MAC_CHARSET:         Result := CP_MACCP;
      OEM_CHARSET:         Result := CP_OEMCP;
      else                 Result := CP_ACP;
    end;
  end;

  function AnsiToUnicodeCP(const s: AnsiString; cCodePage: Cardinal): WideString;
  var iLen: Integer;
  begin
    iLen := Length(s);
    SetLength(Result, iLen);
    MultiByteToWideChar(cCodePage, 0, @s[1], iLen, @Result[1], iLen);
  end;

  function UnicodeToAnsiCP(const s: WideString; cCodePage: Cardinal): AnsiString;
  var iLen: Integer;
  begin
    iLen := Length(s);
    SetLength(Result, iLen);
    WideCharToMultiByte(cCodePage, 0, @s[1], iLen, @Result[1], iLen, nil, nil);
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Misc
   //-------------------------------------------------------------------------------------------------------------------

  function ReplaceChars(const s, sReplaceChars: String; cReplaceWith: Char): String;
  var i: Integer;
  begin
    Result := s;
    for i := 1 to Length(Result) do
      if StrScan(PChar(sReplaceChars), Result[i])<>nil then Result[i] := cReplaceWith;
  end;

  function GetFirstWord(const s, sDelimiters: String): String;
  var i: Integer;
  begin
    i := 1;
    while (i<=Length(s)) and (Pos(s[i], sDelimiters)=0) do Inc(i);
    Result := Copy(s, 1, i-1);
  end;

  function ExtractFirstWord(var s: String; const sDelimiters: String): String;
  begin
    Result := GetFirstWord(s, sDelimiters);
    Delete(s, 1, Length(Result)+1);
  end;

  procedure AccumulateStr(var s: String; const sSeparator, sAdd: String);
  begin
    s := s+iif((s='') or (sAdd=''), '', sSeparator)+sAdd;
  end;

  function ReverseCompare(const s1, s2: String): Boolean;
  const acUpTable: Array[Char] of Char =
    #0#1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16#17#18#19#20#21#22#23#24#25#26#27#28#29#30#31+
    ' !"#$%&''()*+,-./0123456789:;<=>?'+
    '@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'+
    '`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~'#127+
    'ЂЃ‚ѓ„…†‡€‰Љ‹ЊЌЋЏђ‘’“”•–—™љ›њќћџ'+
    ' ЎўЈ¤Ґ¦§Ё©Є«¬­®Ї°±Ііґµ¶·Ё№є»јЅѕї'+
    'АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЯЭЮЯ'+
    'АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЯЭЮЯ';
  var i: Integer;
  begin
    Result := False;
    i := Length(s1);
    if i<>Length(s2) then Exit;
    for i := i downto 1 do
      if acUpTable[s1[i]]<>acUpTable[s2[i]] then Exit;
    Result := True;
  end;

  function ExpandRelativePath(const sBasePath, sRelFileName: String): String;
  var
    i: Integer;
    sOneDir, sRelName: String;
  begin
    if sRelFileName='' then
      Result := ''
    else begin
      sRelName := sRelFileName;
       // Если путь содержит в себе диск (абсолютный путь)
      if (Pos(':', sRelName)>0) or (Pos('\\', sRelName)>0) then
        Result := sRelName
       // Иначе - относительный путь
      else begin
         // Если начинается на '\' - идём с корня
        Result := sBasePath;
        if (Result<>'') and (Result[Length(Result)]='\') then Delete(Result, Length(Result), 1);
        if sRelName[1]='\' then
          Result := Copy(Result, 1, 3)+Copy(sRelName, 2, MaxInt)
         // Иначе - с каталога файла
        else begin
          repeat
            i := Pos('\', sRelName);
            if i=0 then Break;
            sOneDir := Copy(sRelName, 1, i);
            Delete(sRelName, 1, i);
            if sOneDir='..\' then begin
              i := LastDelimiter('\', Result);
              if i=0 then PhoaException(ConstVal('SErrInvalidPicFileName'), [sRelFileName]);
              Delete(Result, i, MaxInt);
            end else
              Result := Result+'\'+Copy(sOneDir, 1, Length(sOneDir)-1);
          until False;
          Result := Result+'\'+sRelName;
        end;
      end;
    end;
  end;

  function LongestCommonPart(const s1, s2: String): String;
  var
    sUp1, sUp2: String;
    i: Integer;
  begin
    sUp1 := AnsiUpperCase(s1);
    sUp2 := AnsiUpperCase(s2);
    Result := '';
    i := 1;
    while (i<=Length(sUp1)) and (i<=Length(sUp2)) and (sUp1[i]=sUp2[i]) do begin
      Result := Result+s1[i];
      Inc(i);
    end;
  end;

  function iif(b: Boolean; const sTrue, sFalse: String): String;
  begin
    if b then Result := sTrue else Result := sFalse;
  end;

  function iif(b: Boolean; iTrue, iFalse: Integer): Integer;
  begin
    if b then Result := iTrue else Result := iFalse;
  end;

  function iif(b: Boolean; pTrue, pFalse: Pointer): Pointer;
  begin
    if b then Result := pTrue else Result := pFalse;
  end;

  function iif(b: Boolean; sTrue, sFalse: Single): Single;
  begin
    if b then Result := sTrue else Result := sFalse;
  end;

  function  HumanReadableSize(iSize: Integer): String;
  begin
    case iSize of
      0..1023:             Result := ConstVal('SSizeBytes',  [iSize]);
      1024..1048575:       Result := ConstVal('SSizeKBytes', [iSize/1024]);
      1048576..1073741823: Result := ConstVal('SSizeMBytes', [iSize/1048576]);
      else                 Result := ConstVal('SSizeGBytes', [iSize/1073741824]);
    end;
  end;

  function HumanReadableSize(i64Size: Int64): String;
  begin
    case i64Size of
      0..1023:             Result := ConstVal('SSizeBytes',  [i64Size]);
      1024..1048575:       Result := ConstVal('SSizeKBytes', [i64Size/1024]);
      1048576..1073741823: Result := ConstVal('SSizeMBytes', [i64Size/1048576]);
      else                 Result := ConstVal('SSizeGBytes', [i64Size/1073741824]);
    end;
  end;

  function CheckMaskedDateTime(const sText: String; bTime: Boolean; var dtResult: TDateTime): Boolean;
  var dt: TDateTime;
  begin
    Result := True;
     // Если не введено ни одной цифры - значит, ввода не было
    if LastDelimiter('0123456789', sText)=0 then
      dtResult := -1
    else begin
      if bTime then dt := StrToTimeDef(sText, -1) else dt := StrToDateDef(sText, -1);
      Result := dt>=0;
      if Result then dtResult := dt else PhoaError(iif(bTime, 'SNotAValidTime', 'SNotAValidDate'), [sText]);
    end;
  end;

  function ConstVal(const sConstName: String): String;
  begin
    Result := fMain.dtlsMain.Consts[sConstName];
  end;

  function ConstVal(const sConstName: String; const aParams: Array of const): String;
  begin
    Result := Format(ConstVal(sConstName), aParams);
  end;

  function ConstValEx(const sText: String): String;
  begin
    Result := sText;
     // Если наименование начинается на '@' - это константа
    if (Result<>'') and (Result[1]='@') then Result := ConstVal(Copy(Result, 2, MaxInt));
  end;

  function PicPropsToInt(PicProps: TPicProperties): Integer;
  begin
    Result := 0;
    Move(PicProps, Result, SizeOf(PicProps));
  end;

  function IntToPicProps(i: Integer): TPicProperties;
  begin
    Move(i, Result, SizeOf(Result));
  end;

  function PicPropName(PicProp: TPicProperty): String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TPicProperty), Byte(PicProp)));
  end;

  function PicFlipText(PicFlip: TPicFlip): String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TPicFlip), Byte(PicFlip)));
  end;

  function PicFlipsText(PicFlips: TPicFlips): String;
  var fl: TPicFlip;
  begin
    Result := '';
    for fl := Low(fl) to High(fl) do AccumulateStr(Result, ', ', PicFlipText(fl));
    Result := '['+Result+']';
  end;

  function GroupByPropName(GBProp: TGroupByProperty): String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TGroupByProperty), Byte(GBProp)));
  end;

  function PixelFormatName(PFmt: TPixelFormat): String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TPixelFormat), Byte(PFmt)));
  end;

  function DiskFilePropName(DFProp: TDiskFileProp): String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TDiskFileProp), Byte(DFProp)));
  end;

  function DiskFilePropValue(DFProp: TDiskFileProp; Namespace: TNamespace): String;
  const asBoolPropVals: Array[Boolean] of String = (' ', '•');
  begin
    Result := '';
    if Namespace<>nil then
      with Namespace do
        case DFProp of
          dfpFileName:            Result := FileName;
          dfpFileType:            Result := FileType;
          dfpSizeOfFile:          Result := Format('%s (%s)', [SizeOfFile, SizeOfFileKB]);
          dfpSizeOfFileDiskUsage: Result := SizeOfFileDiskUsage;
          dfpCreationTime:        Result := CreationTime;
          dfpLastWriteTime:       Result := LastWriteTime;
          dfpLastAccessTime:      Result := LastAccessTime;
          dfpReadOnlyFile:        Result := asBoolPropVals[ReadOnlyFile];
          dfpHidden:              Result := asBoolPropVals[Hidden];
          dfpArchive:             Result := asBoolPropVals[Archive];
          dfpCompressed:          Result := asBoolPropVals[Compressed];
          dfpSystemFile:          Result := asBoolPropVals[SystemFile];
          dfpTemporary:           Result := asBoolPropVals[Temporary];
        end;
  end;

  function DateTimeAutofillPropName(DTAProp: TDateTimeAutofillProp): String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TDateTimeAutofillProp), Byte(DTAProp)));
  end;

  function DateTimeFillResultName(DTFResult: TDateTimeFillResult): String;
  begin
    Result := ConstVal(GetEnumName(TypeInfo(TDateTimeFillResult), Byte(DTFResult)));
  end;

  procedure EnableWndCtl(Ctl: TWinControl; bEnable: Boolean);
  var pi: PPropInfo;
  begin
    if not (csDestroying in Ctl.ComponentState) then Ctl.HandleNeeded; // Workaround ComboBoxEx's repaint bug
    Ctl.Enabled := bEnable;
    pi := GetPropInfo(Ctl, 'Color', [tkInteger]);
    if pi<>nil then SetOrdProp(Ctl, pi, iif(bEnable, clWindow, clBtnFace));
  end;

  procedure LoadGraphicFromFile(const sFileName: String; Bitmap: TBitmap32);
  var
    GClass: TGraphicClass;
    Graphic: TGraphic;
  begin
    GClass := FileFormatList.GraphicFromExtension(ExtractFileExt(sFileName));
    if GClass=nil then raise Exception.Create(ConstVal('SErrUnknownPicFileExtension', [sFileName]));
    Graphic := GClass.Create;
    try
       // Загружаем изображение
      try
        Graphic.LoadFromFile(sFileName);
      except
        on e: Exception do begin
          FreeAndNil(Graphic);
          raise Exception.Create(ConstVal('SErrCannotLoadPicture', [sFileName, e.Message]));
        end;
      end;
       // Преобразовываем в TBitmap32
      try
        Bitmap.Assign(Graphic);
      except
        on e: Exception do raise Exception.Create(ConstVal('SErrCannotDecodePicture', [sFileName, e.Message]));
      end;
    finally
      Graphic.Free;
    end;
  end;

  function LoadGraphicFromFile(const sFileName: String): TBitmap32;
  begin
    Result := TBitmap32.Create;
    try
      LoadGraphicFromFile(sFileName, Result);
    except
      on e: Exception do begin
        FreeAndNil(Result); // set to nil to satisfy the compiler
        raise;
      end;
    end;
  end;

  function ActivateVTNode(Tree: TBaseVirtualTree; Node: PVirtualNode): Boolean;
  begin
    Result := Node<>nil;
    Tree.FocusedNode := Node;
    if Result then begin
      Tree.Selected[Node] := True;
      Tree.ScrollIntoView(Node, False, False);
    end;
  end;

  procedure RegSaveVTColumns(const sSection: String; Tree: TVirtualStringTree);
  var
    i: Integer;
    c: TVirtualTreeColumn;
  begin
    with TRegIniFile.Create(SRegRoot) do
      try
        for i := 0 to Tree.Header.Columns.Count-1 do begin
          c := Tree.Header.Columns[i];
          WriteString(sSection, 'Column'+IntToStr(i), Format('%d,%d,%d', [c.Position, c.Width, Byte(coVisible in c.Options)]));
        end;
      finally
        Free;
      end;
  end;

type
  TVTColumnsCast = class(TVirtualTreeColumns);

  procedure RegLoadVTColumns(const sSection: String; Tree: TVirtualStringTree);
  var
    i: Integer;
    c: TVirtualTreeColumn;
    s: String;
  begin
    with TRegIniFile.Create(SRegRoot) do
      try
        for i := 0 to Tree.Header.Columns.Count-1 do begin
          c := Tree.Header.Columns[i];
          s := ReadString(sSection, 'Column'+IntToStr(i), '');
          c.Position := StrToIntDef(ExtractFirstWord(s, ','), c.Position);
          c.Width    := StrToIntDef(ExtractFirstWord(s, ','), c.Width);
          SetVTColumnVisible(c, StrToIntDef(ExtractFirstWord(s, ','), Byte(coVisible in c.Options))<>0);
        end;
    finally
      Free;
    end;
  end;

  procedure SetVTColumnVisible(Column: TVirtualTreeColumn; bVisible: Boolean);
  begin
    with Column do
      if bVisible then Options := Options+[coVisible] else Options := Options-[coVisible];
  end;

  function IDInArray(iID: Integer; const aIDs: TIDArray): Boolean;
  var i: Integer;
  begin
    for i := 0 to High(aIDs) do
      if aIDs[i]=iID then begin
        Result := True;
        Exit;
      end;
    Result:= False;
  end;

  function ShortenFileName(Canvas: TCanvas; iWidth: Integer; const s: String): String;
  var
    aBuf: Array[0..1024] of Char;
    r: TRect;
  begin
    StrPCopy(aBuf, s);
    r := Rect(0, 0, iWidth, 0);
    DrawText(Canvas.Handle, aBuf, -1, r, DT_LEFT or DT_PATH_ELLIPSIS or DT_MODIFYSTRING);
    Result := aBuf;
  end;

  function ShowFileShellContextMenu(const sFileName: String): Boolean;
  var NS: TNamespace;
  begin
    Result := False;
    NS := nil;
    try
      try
        NS := TNamespace.CreateFromFileName(sFileName);
      except
        PhoaError('SErrFileNotFoundFmt', [sFileName]);
      end;
      if NS<>nil then Result := NS.ShowContextMenu(fMain, nil, nil, nil);
    finally
      NS.Free;
    end;
  end;

var
  iWaitCount: Integer = 0;
  SaveCursor: TCursor = crDefault;

  procedure StartWait;
  begin
    if iWaitCount=0 then begin
      SaveCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
    end;
    Inc(iWaitCount);
  end;

  procedure StopWait;
  begin
    if iWaitCount>0 then begin
      Dec(iWaitCount);
      if iWaitCount=0 then Screen.Cursor := SaveCursor;
    end;
  end;

  procedure OpenWebsite;
  begin
    ShellExecute(Application.Handle, 'open', PChar(ConstVal('SWebsite')), nil, nil, SW_SHOWNORMAL);
  end;

end.
