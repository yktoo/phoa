//**********************************************************************************************************************
//  $Id: HTMLRender.pas,v 1.2 2004-10-21 07:10:42 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit HTMLRender;

interface

uses
  Windows, SysUtils, Classes, Graphics;

  function HTMLColorToRGB(cValue: TColor): String;
  function HTMLDecodeSpecialChars(const s: String): String;
  function HTMLDrawText(Canvas: TCanvas; const szStr: String; Rect: TRect; uFlags: TOwnerDrawState; bCalcOnly: Boolean = False): Integer;
  function HTMLGetTag(const szTmp: String; var Index: Integer; var szTag: String): Integer;

implementation

const
  ERR_PARAM = $123456;

  aSpecialChars: Array[0..97] of
    record
      sName: String;
      cChar: Char;
    end = (
      (sName: 'nbsp';    cChar: ' '),
      (sName: 'quot';    cChar: '"'),
      (sName: 'amp';     cChar: '&'),
      (sName: 'lt';      cChar: '<'),
      (sName: 'gt';      cChar: '>'),
      (sName: 'ndash';   cChar: #150),
      (sName: 'mdash';   cChar: #151),
      (sName: 'iexcl';   cChar: #161),
      (sName: 'cent';    cChar: #162),
      (sName: 'pound';   cChar: #163),
      (sName: 'curren';  cChar: #164),
      (sName: 'yen';     cChar: #165),
      (sName: 'brvbar';  cChar: #166),
      (sName: 'sect';    cChar: #167),
      (sName: 'uml';     cChar: #168),
      (sName: 'copy';    cChar: #169),
      (sName: 'ordf';    cChar: #170),
      (sName: 'laquo';   cChar: #171),
      (sName: 'not';     cChar: #172),
      (sName: 'shy';     cChar: #173),
      (sName: 'reg';     cChar: #174),
      (sName: 'macr';    cChar: #175),
      (sName: 'deg';     cChar: #176),
      (sName: 'plusmn';  cChar: #177),
      (sName: 'sup2';    cChar: #178),
      (sName: 'sup3';    cChar: #179),
      (sName: 'acute';   cChar: #180),
      (sName: 'micro';   cChar: #181),
      (sName: 'para';    cChar: #182),
      (sName: 'uml';     cChar: #183),
      (sName: 'middot';  cChar: #184),
      (sName: 'cedil';   cChar: #185),
      (sName: 'sup1';    cChar: #186),
      (sName: 'ordm';    cChar: #187),
      (sName: 'raquo';   cChar: #188),
      (sName: 'frac14';  cChar: #189),
      (sName: 'frac12';  cChar: #190),
      (sName: 'frac34';  cChar: #191),
      (sName: 'iquest';  cChar: #192),
      (sName: 'Agrave';  cChar: #193),
      (sName: 'Aacute';  cChar: #194),
      (sName: 'Acirc';   cChar: #195),
      (sName: 'Atilde';  cChar: #196),
      (sName: 'Auml';    cChar: #197),
      (sName: 'Aring';   cChar: #198),
      (sName: 'AElig';   cChar: #199),
      (sName: 'Ccedil';  cChar: #200),
      (sName: 'Egrave';  cChar: #201),
      (sName: 'Eacute';  cChar: #202),
      (sName: 'Ecirc';   cChar: #203),
      (sName: 'Euml';    cChar: #204),
      (sName: 'Igrave';  cChar: #205),
      (sName: 'Iacute';  cChar: #206),
      (sName: 'Icirc';   cChar: #207),
      (sName: 'Iuml';    cChar: #208),
      (sName: 'ETH';     cChar: #209),
      (sName: 'Ntilde';  cChar: #210),
      (sName: 'Ograve';  cChar: #211),
      (sName: 'Oacute';  cChar: #212),
      (sName: 'Ocirc';   cChar: #213),
      (sName: 'Otilde';  cChar: #214),
      (sName: 'Ouml';    cChar: #215),
      (sName: 'times';   cChar: #216),
      (sName: 'Oslash';  cChar: #217),
      (sName: 'Ugrave';  cChar: #218),
      (sName: 'Uacute';  cChar: #219),
      (sName: 'Ucirc';   cChar: #220),
      (sName: 'Uuml';    cChar: #221),
      (sName: 'Yacute';  cChar: #222),
      (sName: 'THORN';   cChar: #223),
      (sName: 'szlig';   cChar: #224),
      (sName: 'agrave';  cChar: #225),
      (sName: 'aacute';  cChar: #226),
      (sName: 'acirc';   cChar: #227),
      (sName: 'atilde';  cChar: #228),
      (sName: 'auml';    cChar: #229),
      (sName: 'aring';   cChar: #230),
      (sName: 'aelig';   cChar: #231),
      (sName: 'ccedil';  cChar: #232),
      (sName: 'egrave';  cChar: #233),
      (sName: 'eacute';  cChar: #234),
      (sName: 'ecirc';   cChar: #235),
      (sName: 'euml';    cChar: #236),
      (sName: 'igrave';  cChar: #237),
      (sName: 'iacute';  cChar: #238),
      (sName: 'icirc';   cChar: #239),
      (sName: 'iuml';    cChar: #240),
      (sName: 'eth';     cChar: #241),
      (sName: 'ntilde';  cChar: #242),
      (sName: 'ograve';  cChar: #243),
      (sName: 'oacute';  cChar: #244),
      (sName: 'ocirc';   cChar: #245),
      (sName: 'otilde';  cChar: #246),
      (sName: 'ouml';    cChar: #247),
      (sName: 'divide';  cChar: #248),
      (sName: 'oslash';  cChar: #249),
      (sName: 'ugrave';  cChar: #250),
      (sName: 'uacute';  cChar: #251));

   // Возвращает HTML special char по имени, или #0, если имя не опознано
  function DecodeSpecialChar(const sName: String): Char;
  var i: Integer;
  begin
    for i := 0 to High(aSpecialChars) do
      if aSpecialChars[i].sName=sName then begin
        Result := aSpecialChars[i].cChar;
        Exit;
      end;
    Result := #0;
  end;

  function HTMLColorToRGB(cValue: TColor): String;
  var c: Integer;
  begin
    c := ColorToRGB(cValue);
    Result := IntToHex(GetRValue(c), 2)+IntToHex(GetGValue(c), 2)+IntToHex(GetBValue(c), 2);
  end;

  function HTMLDecodeSpecialChars(const s: String): String;
  var
    i, iLen, iEscStart: Integer;
    c: Char;
  begin
    Result := '';
     // Перебираем символы s
    i := 1;
    iLen := Length(s);
    while i<=iLen do begin
      c := s[i];
       // Escape char
      if c='&' then begin
        iEscStart := i;
         // Ищем окончание Escape-последовательности
        Inc(i); 
        while (i<=iLen) and (s[i] in ['0'..'9', 'A'..'Z', 'a'..'z']) do Inc(i);
         // Если нашли ';' - считаем его частью имени и пытаемся декодировать имя
        if (i<=iLen) and (s[i]=';') then
          c := DecodeSpecialChar(Copy(s, iEscStart+1, i-iEscStart-1))
         // Иначе откатываемся на предыдущий символ и считаем, что не опознали
        else begin
          c := #0;
          Dec(i);
        end;
         // Если не опознали как special char - добавляем, как есть
        if c=#0 then Result := Result+Copy(s, iEscStart, i-iEscStart+1);
      end;
      if c<>#0 then Result := Result+c;
      Inc(i);
    end;
  end;

  function HTMLDrawText(Canvas: TCanvas; const szStr: String; Rect: TRect; uFlags: TOwnerDrawState; bCalcOnly:Boolean=False):Integer;
  var i, iCount, XOrg, FLineSizeY: Integer;

    function ProcessTag(Canvas: TCanvas; szTag: String): Integer;
    var
      iTmp: Integer;
      tmp, szTmp: String;
      Back: Word;
      Front: Word;

      function GetMaxColor(Color: TColor): Word;
      var R, G, B: Word;
      begin
        B := (Color and $FF);
        G := ((Color shr 16) and $FF);
        R := ((Color shr 32) and $FF);
        Result := (R+G+B) div 3;
      end;

      function GetHexParamBase(szTag, szParam: String): String;
      var
        i: Integer;
        szTmp: String;
      begin
        Result := '';
        i := Pos(szParam, szTag);
        if i > 0 then begin
          i := i + Length(szParam);
          szTmp := '';
          while i<Length(szTag) do begin
            Inc(i);
            if szTag[i-1] in ['#','=',':','''','"'] then Continue;
            if szTag[i-1] in ['0'..'9','A'..'F','a'..'f','-','+'] then szTmp := szTmp+szTag[i-1] else Break;
          end;
          Result := szTmp;
        end;
      end;

      function GetHexParam(szTag, szParam: String): Integer;
      var szTmp: String;
      begin
        Result := ERR_PARAM;
        szTmp := GetHexParamBase(szTag, szParam);
        if szTmp<>'' then Result := StrToIntDef('$'+szTmp, 0);
      end;

      function GetHexColor(szTag, szParam: String): TColor;
      var
        szTmp: String;
        r, g, b:Byte;
      begin
        Result := ERR_PARAM;
        szTmp:=GetHexParamBase(szTag, szParam);
        if szTmp<>'' then begin
          r := StrToIntDef('$'+Copy(szTmp, 1, 2), 0);
          g := StrToIntDef('$'+Copy(szTmp, 3, 2), 0);
          b := StrToIntDef('$'+Copy(szTmp, 5, 2), 0);
          Result := TColor(RGB(r, g, b));
        end;
        Exit; //???
        i := Pos(szParam, szTag);
        if i > 0 then begin
          i := i + Length(szParam);
          szTmp := '';
          while i<Length(szTag) do begin
            Inc(i);
            if szTag[i-1] in ['#','=',':','''','"'] then Continue;
            if szTag[i-1] in ['0'..'9','A'..'F','a'..'f','-','+'] then szTmp := szTmp+szTag[i-1] else Break;
          end;
          try
            r := StrToIntDef('$'+Copy(szTmp, 1, 2), 0);
            g := StrToIntDef('$'+Copy(szTmp, 3, 2), 0);
            b := StrToIntDef('$'+Copy(szTmp, 5, 2), 0);
            Result := TColor(RgB(r,g,b));
          except
          end;
        end;
      end;

      function GetStringParam(szTag, szParam: String): String;
      var i: Integer;
      begin
        Result := '';
        i := Pos(szParam, szTag);
        if i>0 then begin
          i := i + Length(szParam);
          while i<Length(szTag) do begin
            Inc(i);
            if szTag[i-1] in ['#','=',':','''','"'] then Continue;
            if not (szTag[i-1] in [' ','>','<','.',',']) then Result := Result+szTag[i-1] else Break;
          end;
        end;
      end;

    begin
      szTag := UpperCase(szTag);
      Result := -100;
      if Length(szTag)<2 then Exit;
      if szTag[1]='<' then begin
        tmp := Copy(szTag,2,100);
        if tmp='B>' then begin
          Result := 1;
          if not bCalcOnly then Canvas.Font.Style := Canvas.Font.Style + [fsBold];
          iTmp := GetHexParam(szTag, 'BACKGROUND');
          if not bCalcOnly and (iTmp<>ERR_PARAM) then
            if odSelected in uFlags then Canvas.Font.Color := iTmp else Canvas.Brush.Color := iTmp;
        end else if (tmp='/B>') or (tmp='/U>') or (tmp='/I>') or (tmp='/S>') then
          Result := -1
        else if tmp='I>' then begin
          Result := 1;
          if not bCalcOnly then Canvas.Font.Style := Canvas.Font.Style + [fsItalic];
        end else if tmp='S>' then begin
          Result := 1;
          if not bCalcOnly then Canvas.Font.Style := Canvas.Font.Style + [fsStrikeOut];
        end else if tmp='U>' then begin
          Result := 1;
          if not bCalcOnly then Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
        end else if tmp='/I>' then begin
          Result := -1;
        end else if (tmp='BR>') or (tmp='P>') then begin
          Result := 3;
        end else if Copy(szTag,2,4)='FONT' then begin
          Result := 2;
          iTmp := GetHexColor(szTag, 'COLOR');
          if not bCalcOnly and (iTmp<>ERR_PARAM) then Canvas.Font.Color := iTmp;
          iTmp := GetHexParam(szTag, 'SIZE');
          if iTmp<>ERR_PARAM then Canvas.Font.Size := 5+iTmp*5;
          szTmp := GetStringParam(szTag,'FACE');
          if szTmp<>'' then Canvas.Font.Name := szTmp;
          iTmp := GetHexParam(szTag, 'BACKGROUND');
          Front := GetMaxColor(iTmp);
          Back  := GetMaxColor(ColorToRGB(Canvas.Brush.Color));
          if not bCalcOnly then begin
            if iTmp<>ERR_PARAM then
              if (odSelected in uFlags) and (((Back > 127) and (Front <= 127)) or ((Back <= 127) and (Front > 127))) then
                Canvas.Font.Color  := iTmp
              else if not (odGrayed in uFlags) then
                Canvas.Brush.Color := iTmp;
            if (odGrayed in uFlags) and (Canvas.Brush.Color<>clHighlight) then Canvas.Font.Color := clBlue;
          end;
        end else if Copy(szTag, 2, 5)='/FONT' then
          Result := -2
        else
          Result := -100;
      end;
    end;

    function ProcessString(var X, Y, XMax, YMax, LineSizeY, Index: Integer; szTmp: String; iStartTag: Integer = 0):Boolean;
    var
      FFont: TFont;
      BColor: TColor;
      BStyle: TBrushStyle;
      szTag: String;
      iWidth, iTag, i, iMax: Integer;
      Rect: TRect;
      szWord, szOval, szOldWords: String;

      function DrawWords: Boolean;
      var iOval: Integer;
      begin
        Result := False;
        if Canvas.TextHeight('0')>LineSizeY then LineSizeY := Canvas.TextHeight('0');
        if szOldWords<>'' then begin
          Rect.Left := X;
          Rect.Top  := Y + 1;
          if not bCalcOnly then DrawText(Canvas.Handle, PChar(szOldWords), Length(szOldWords), Rect, DT_LEFT or DT_SINGLELINE or DT_NOPREFIX);
          Inc(X, Canvas.TextWidth(szOldWords));
          szOldWords := '';
        end;
        if szWord<>'' then begin
          iWidth := Canvas.TextWidth(szWord);
          if (X + iWidth > XMax) or (iWidth > XMax) then begin
            if ((Y + Canvas.TextHeight(szWord) * 2 + 2) > YMax) or (iWidth > XMax) then begin
              for iOval := Length(szWord) downto 1 do begin
                szOval := Copy(szWord, 1, iOval)+'...';
                if X + Canvas.TextWidth(szOval) <= XMax then begin
                  Rect.Left := X;
                  Rect.Top  := Y + 1;
                  if not bCalcOnly then DrawText(Canvas.Handle, PChar(szOval), Length(szOval), Rect, DT_LEFT or DT_SINGLELINE or DT_NOPREFIX);
                  Result := True;
                  Inc(X, Canvas.TextWidth(szOval));
                  Break;
                end;
              end;
            end;
            X := XOrg;
            Inc(Y, LineSizeY);
            LineSizeY := 0;
            if not Result then Inc(iCount);
          end;
          if not Result then begin
            Rect.Left := X;
            Rect.Top  := Y + 1;
            if not bCalcOnly and (YMax-Y>=Canvas.TextHeight(szWord)) then DrawText(Canvas.Handle, PChar(szWord), Length(szWord), Rect, DT_LEFT or DT_SINGLELINE or DT_NOPREFIX);
            Inc(X, iWidth);
          end;
          szWord := '';
        end;
      end;

    begin
      FFont := TFont.Create;
      try
        Result := False;
        with Canvas do begin
          FFont.Assign(Font);
          BColor := Brush.Color;
          BStyle := Brush.Style;
          iTag := HTMLGetTag(szTmp, Index, szTag);
          while iTag>=0 do begin
            case iTag of
              0: begin
                Rect.Right  := XMax;
                Rect.Bottom := YMax;
                szWord := '';
                szOldWords := '';
                iMax := Length(szTag);
                for i := 1 to iMax do
                  if szTag[i]=' ' then begin
                    szWord := szWord+' ';
                    iWidth := TextWidth(szOldWords+szWord);
                    if X+iWidth>XMax then begin
                      if DrawWords then begin
                        Result := True;
                        Break;
                      end;
                    end else begin
                      szOldWords := szOldWords+szWord;
                      szWord := '';
                    end;
                  end else
                    szWord := szWord+szTag[i];
                DrawWords;
              end;
              1: begin
                iTag := ProcessTag(Canvas, szTag);
                if iTag=3 then begin
                  X := XOrg;
                  if LineSizeY=0 then Inc(Y, Canvas.TextHeight('0')) else Inc(Y, LineSizeY);
                  LineSizeY := 0;
                end else begin
                  if (iTag = -iStartTag) then Break;
                  Result := ProcessString(X, Y, XMax, YMax, LineSizeY, Index, szTmp, iTag);
                  if Result then Break;
                  Brush.Color := BColor;
                  Brush.Style := BStyle;
                  Font.Assign(FFont);
                end;
                if (X>XMax) or (Y>YMax) then Break;
              end;
            end;
            iTag := HTMLGetTag(szTmp, Index, szTag);
          end;
          Brush.Color := BColor;
          Brush.Style := BStyle;
          Font.Assign(FFont);
        end;
      finally
        FFont.Free;
      end;
    end;

  begin
    iCount := 1;
    i := 1;
    XOrg := Rect.Left;
    FLineSizeY :=0;
    ProcessString(
      Rect.Left,
      Rect.Top,
      Rect.Right,
      Rect.Bottom,
      FLineSizeY,
      i,
      StringReplace(HTMLDecodeSpecialChars(szStr), #13#10, ' ', [rfReplaceAll]));
    Result := iCount;
  end;

  function HTMLGetTag(const szTmp: String; var Index: Integer; var szTag: String): Integer;
  var
    i: Integer;
    iCount: Integer;
    OldIndex: Integer;
    bTag: Boolean;
  begin
    iCount:= Length(szTmp);
    Result:= -1;
    if Index > iCount then Exit;
    bTag := (szTmp[Index]='<');
    szTag := szTmp[Index];
    OldIndex := Index;
    Index := iCount+1;
    for i := OldIndex+1 to iCount do begin
      if bTag then begin
        if szTmp[i] = '>' then begin
          szTag := szTag + szTmp[i];
          Index := i+1;
          Break;
        end;
      end else if szTmp[i] = '<' then begin
        Index:= i;
        Break;
      end;
      szTag:= szTag + szTmp[i];
    end;
    if not bTag then Result:= 0 else Result:= 1;
  end;


end.
