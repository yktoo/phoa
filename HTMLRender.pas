//**********************************************************************************************************************
//  $Id: HTMLRender.pas,v 1.1 2004-10-14 12:21:02 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit HTMLRender;

interface

uses
  Windows, SysUtils, Classes, Graphics;

  function HTMLColorToRGB(cValue: TColor): String;
  function HTMLDrawText(Canvas: TCanvas; szStr: String; Rect: TRect; uFlags: TOwnerDrawState; bCalcOnly: Boolean = False): Integer;
  function HTMLGetTag(szTmp: String; var Index: Integer; var szTag: String): Integer;

implementation

const
  ERR_PARAM = $123456;

  function HTMLColorToRGB(cValue: TColor): String;
  var c: Integer;
  begin
    c := ColorToRGB(cValue);
    Result := IntToHex(GetRValue(c), 2)+IntToHex(GetGValue(c), 2)+IntToHex(GetBValue(c), 2);
  end;

  function ReplaceSpecialCharsFunc(aText: String): String;
  const
    spCharsCount = 96;
    spChars: Array[0..spCharsCount-1] of
      record
        spChar: String;
        spRepl: String;
      end = (
        (spChar: 'nbsp';    spRepl: ' '),
        (spChar: 'quot';    spRepl: '"'),
        (spChar: 'amp';     spRepl: '&'),
        (spChar: 'lt';      spRepl: '<'),
        (spChar: 'gt';      spRepl: '<'),
        (spChar: 'iexcl';   spRepl: #161),
        (spChar: 'cent';    spRepl: #162),
        (spChar: 'pound';   spRepl: #163),
        (spChar: 'curren';  spRepl: #164),
        (spChar: 'yen';     spRepl: #165),
        (spChar: 'brvbar';  spRepl: #166),
        (spChar: 'sect';    spRepl: #167),
        (spChar: 'uml';     spRepl: #168),
        (spChar: 'copy';    spRepl: #169),
        (spChar: 'ordf';    spRepl: #170),
        (spChar: 'laquo';   spRepl: #171),
        (spChar: 'not';     spRepl: #172),
        (spChar: 'shy';     spRepl: #173),
        (spChar: 'reg';     spRepl: #174),
        (spChar: 'macr';    spRepl: #175),
        (spChar: 'deg';     spRepl: #176),
        (spChar: 'plusmn';  spRepl: #177),
        (spChar: 'sup2';    spRepl: #178),
        (spChar: 'sup3';    spRepl: #179),
        (spChar: 'acute';   spRepl: #180),
        (spChar: 'micro';   spRepl: #181),
        (spChar: 'para';    spRepl: #182),
        (spChar: 'uml';     spRepl: #183),
        (spChar: 'middot';  spRepl: #184),
        (spChar: 'cedil';   spRepl: #185),
        (spChar: 'sup1';    spRepl: #186),
        (spChar: 'ordm';    spRepl: #187),
        (spChar: 'raquo';   spRepl: #188),
        (spChar: 'frac14';  spRepl: #189),
        (spChar: 'frac12';  spRepl: #190),
        (spChar: 'frac34';  spRepl: #191),
        (spChar: 'iquest';  spRepl: #192),
        (spChar: 'Agrave';  spRepl: #193),
        (spChar: 'Aacute';  spRepl: #194),
        (spChar: 'Acirc';   spRepl: #195),
        (spChar: 'Atilde';  spRepl: #196),
        (spChar: 'Auml';    spRepl: #197),
        (spChar: 'Aring';   spRepl: #198),
        (spChar: 'AElig';   spRepl: #199),
        (spChar: 'Ccedil';  spRepl: #200),
        (spChar: 'Egrave';  spRepl: #201),
        (spChar: 'Eacute';  spRepl: #202),
        (spChar: 'Ecirc';   spRepl: #203),
        (spChar: 'Euml';    spRepl: #204),
        (spChar: 'Igrave';  spRepl: #205),
        (spChar: 'Iacute';  spRepl: #206),
        (spChar: 'Icirc';   spRepl: #207),
        (spChar: 'Iuml';    spRepl: #208),
        (spChar: 'ETH';     spRepl: #209),
        (spChar: 'Ntilde';  spRepl: #210),
        (spChar: 'Ograve';  spRepl: #211),
        (spChar: 'Oacute';  spRepl: #212),
        (spChar: 'Ocirc';   spRepl: #213),
        (spChar: 'Otilde';  spRepl: #214),
        (spChar: 'Ouml';    spRepl: #215),
        (spChar: 'times';   spRepl: #216),
        (spChar: 'Oslash';  spRepl: #217),
        (spChar: 'Ugrave';  spRepl: #218),
        (spChar: 'Uacute';  spRepl: #219),
        (spChar: 'Ucirc';   spRepl: #220),
        (spChar: 'Uuml';    spRepl: #221),
        (spChar: 'Yacute';  spRepl: #222),
        (spChar: 'THORN';   spRepl: #223),
        (spChar: 'szlig';   spRepl: #224),
        (spChar: 'agrave';  spRepl: #225),
        (spChar: 'aacute';  spRepl: #226),
        (spChar: 'acirc';   spRepl: #227),
        (spChar: 'atilde';  spRepl: #228),
        (spChar: 'auml';    spRepl: #229),
        (spChar: 'aring';   spRepl: #230),
        (spChar: 'aelig';   spRepl: #231),
        (spChar: 'ccedil';  spRepl: #232),
        (spChar: 'egrave';  spRepl: #233),
        (spChar: 'eacute';  spRepl: #234),
        (spChar: 'ecirc';   spRepl: #235),
        (spChar: 'euml';    spRepl: #236),
        (spChar: 'igrave';  spRepl: #237),
        (spChar: 'iacute';  spRepl: #238),
        (spChar: 'icirc';   spRepl: #239),
        (spChar: 'iuml';    spRepl: #240),
        (spChar: 'eth';     spRepl: #241),
        (spChar: 'ntilde';  spRepl: #242),
        (spChar: 'ograve';  spRepl: #243),
        (spChar: 'oacute';  spRepl: #244),
        (spChar: 'ocirc';   spRepl: #245),
        (spChar: 'otilde';  spRepl: #246),
        (spChar: 'ouml';    spRepl: #247),
        (spChar: 'divide';  spRepl: #248),
        (spChar: 'oslash';  spRepl: #249),
        (spChar: 'ugrave';  spRepl: #250),
        (spChar: 'uacute';  spRepl: #251));
  var
    I: longint;
    P: longint;
    S: String;
  begin
    Result := aText;
    for I := 0 to spCharsCount - 1 do 
      repeat
        P := Pos('&' + spChars[I].spChar + ';', Result);
        if P <> 0 then begin
          S := Copy(Result, 1, P-1)+
            spChars[I].spRepl+
            Copy(Result, P+Length(spChars[I].spChar) + 2,
            Length(Result) - (P + Length(spChars[I].spChar) + 2) + 1);
          Result := S;
        end;
      until P = 0;
  end;

type
  StrRec = record
    allocSiz: longint;
    refCnt: longint;
    Length: longint;
  end;

const
  skew = sizeof(StrRec);

  function PosEx(substr: AnsiString; S: AnsiString; Index: Integer): Integer;register;
  asm
    TEST    EAX,EAX
    JE      @@noWork
    TEST    EDX,EDX
    JE      @@stringEmpty
    PUSH    EBX
    PUSH    ESI
    PUSH    EDI
    MOV     ESI,substr
    MOV     EDI,s
    MOV     EBX,Index
    DEC     EBX
    MOV     ECX,[EDI-skew].StrRec.Length
    SUB     ECX,EBX
    ADD     EDI,EBX
    PUSH    EBX
    PUSH    EDI
    MOV     EDX,[ESI-skew].StrRec.Length
    DEC     EDX
    JS      @@fail
    MOV     AL,[ESI]
    INC     ESI
    SUB     ECX,EDX
    JLE     @@fail
  @@loop:
    REPNE   SCASB
    JNE     @@fail
    MOV     EBX,ECX
    PUSH    ESI
    PUSH    EDI
    MOV     ECX,EDX
    REPE    CMPSB
    POP     EDI
    POP     ESI
    JE      @@found
    MOV     ECX,EBX
    JMP     @@loop
  @@fail:
    POP     EDX
    POP     EBX
    XOR     EAX,EAX
    JMP     @@exit
  @@stringEmpty:
    XOR     EAX,EAX
    JMP     @@noWork
  @@found:
    POP     EDX
    POP     EBX
    MOV     EAX,EDI
    SUB     EAX,EDX
    ADD     EAX,EBX
  @@exit:
    POP     EDI
    POP     ESI
    POP     EBX
  @@noWork:
  end;


  function ReplaceStr(const S, Srch, Replace: String): String;
  var
    I,iPos,iInc: Integer;
    Source: String;
  begin
    Result:= '';
    if S='' then exit;
    Source:= UpperCase(S);
    iPos  := 1;
    iInc  := Length(Srch);
    repeat
      I:= PosEx(Srch, Source,iPos);
      if I > 0 then
        begin
          Result:= Result + Copy(S, iPos, I-iPos) + Replace;
        end
      else
        Result:= Result + Copy(S, iPos, Length(S)-iPos+1);
      iPos := I+iInc;
    until I <= 0;
  end;


  function HTMLDrawText(Canvas: TCanvas; szStr: String; Rect: TRect; uFlags: TOwnerDrawState; bCalcOnly:Boolean=False):Integer;
  var
    i: Integer;
    tmp:String;
    iCount : Integer;
    XOrg   : Integer;
    FLineSizeY  : Integer;

    function ProcessTag(Canvas: TCanvas; szTag: String): Integer;
    var
      iTmp: Integer;
      szTmp: String;
      Back: Word;
      Front: Word;

      function GetMaxColor(Color: TColor): Word;
      var R, G, B: Word;
      begin
        B := (Color and $FF);
        G := ((Color shr 16) and $FF);
        R := ((Color shr 32) and $FF);
        Result := (R + G + B) div 3;
      end;

      function GetHexParamBase(szTag, szParam: String): String;
      var
        i    : Integer;
        szTmp: String;
      begin
        Result := '';
        i := Pos(szParam, szTag);
        if i > 0 then
          begin
            i := i + Length(szParam);
            szTmp := '';
            while i<Length(szTag) do
              begin
                Inc(i);
                if szTag[i-1] in ['#','=',':','''','"'] then
                  continue;
                if szTag[i-1] in ['0'..'9','A'..'F','a'..'f','-','+'] then
                  szTmp := szTmp+szTag[i-1]
                else
                  Break;
            end;
            try
              Result := szTmp;
            except
            end;
          end;
      end;

      function GetHexParam(szTag, szParam: String): Integer;
      var
        szTmp: String;
      begin
        Result := ERR_PARAM;
        szTmp:=GetHexParamBase(szTag, szParam);
        if szTmp<>'' then
          try
            Result := StrToIntDef('$' + szTmp, 0);
          except
          end;
      end;

      function GetHexColor(szTag, szParam: String): TColor;
      var
        szTmp: String;
        r,g,b:Byte;

      begin
        Result := ERR_PARAM;
        szTmp:=GetHexParamBase(szTag, szParam);
        if szTmp<>'' then
          try
            r:=StrToIntDef('$'+Copy(szTmp,1,2),0);
            g:=StrToIntDef('$'+Copy(szTmp,3,2),0);
            b:=StrToIntDef('$'+Copy(szTmp,5,2),0);
            Result :=TColor(RgB(r,g,b));
          except
          end;

        exit;
        i := Pos(szParam, szTag);
        if i > 0 then
          begin
            i := i + Length(szParam);
            szTmp := '';
            while i<Length(szTag) do
              begin
                Inc(i);
                if szTag[i-1] in ['#','=',':','''','"'] then
                  continue;
                if szTag[i-1] in ['0'..'9','A'..'F','a'..'f','-','+'] then
                  szTmp := szTmp+szTag[i-1]
                else
                  Break;
            end;
            try
              r:=StrToIntDef('$'+Copy(szTmp,1,2),0);
              g:=StrToIntDef('$'+Copy(szTmp,3,2),0);
              b:=StrToIntDef('$'+Copy(szTmp,5,2),0);
              Result :=TColor(RgB(r,g,b));
            except
            end;
          end;
      end;

      function GetStringParam(szTag, szParam: String): String;
      var
        i    : Integer;
      begin
        Result := '';
        i := Pos(szParam, szTag);
        if i > 0 then
          begin
            i := i + Length(szParam);
            while i<Length(szTag) do
              begin
                Inc(i);
                if szTag[i-1] in ['#','=',':','''','"'] then
                  continue;
                if not (szTag[i-1] in [' ','>','<','.',',']) then
                  Result := Result+szTag[i-1]
                else
                  Break;
              end;
          end;
      end;


    begin
      szTag := UpperCase(szTag);
      Result := -100;
      if Length(szTag)<2 then exit;
      if szTag[1]='<' then
        begin
          tmp:=Copy(szTag,2,100);
          if tmp='B>' then
            begin
              Result := 1;
              if not bCalcOnly then
                Canvas.Font.Style := Canvas.Font.Style + [fsBold];
              iTmp := GetHexParam(szTag, 'BACKGROUND');
              if not bCalcOnly then
                if iTmp <> ERR_PARAM then
                  if odSelected in uFlags then
                    Canvas.Font.Color  := iTmp
                  else
                    Canvas.Brush.Color := iTmp;
            end
          else
            if (tmp='/B>') or (tmp='/U>') or (tmp='/I>') or (tmp='/S>') then
              Result := -1
            else
              if tmp='I>' then
                 begin
                   result:=1;
                   if not bCalcOnly then
                    Canvas.Font.Style := Canvas.Font.Style + [fsItalic];
                 end
              else
                if tmp='S>' then
                  begin
                    result:=1;
                    if not bCalcOnly then
                      Canvas.Font.Style := Canvas.Font.Style + [fsStrikeOut];
                  end
                else
                  if tmp='U>' then
                    begin
                      result:=1;
                      if not bCalcOnly then
                        Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
                    end
                     else
                       if tmp='/I>' then
                         begin
                           Result := -1;
                         end
                          else
                            if (tmp='BR>') or (tmp='P>') then
                              begin
                                Result := 3;
                              end
                            else
                              if Copy(szTag,2,4)='FONT' then
                                begin
                                  Result := 2;
                                  iTmp := GetHexColor(szTag, 'COLOR');
                                  if not bCalcOnly then
                                    if iTmp <> ERR_PARAM then
                                      Canvas.Font.Color := iTmp;

                                  iTmp := GetHexParam(szTag, 'SIZE');
                                  if iTmp <> ERR_PARAM then
                                    Canvas.Font.Size := 5+iTmp*5;

                                  szTmp := GetStringParam(szTag,'FACE');
                                  if szTmp<>'' then
                                    Canvas.Font.Name := szTmp;

                                  iTmp := GetHexParam(szTag, 'BACKGROUND');
                                  Front:= GetMaxColor(iTmp);
                                  Back := GetMaxColor(ColorToRGB(Canvas.Brush.Color));
                                  if not bCalcOnly then
                                    begin
                                     if iTmp <> ERR_PARAM then
                                       if (odSelected in uFlags) and (((Back > 127) and (Front <= 127)) or
                                          ((Back <= 127) and (Front > 127))) then
                                          Canvas.Font.Color  := iTmp
                                        else if not (odGrayed in uFlags) then
                                          Canvas.Brush.Color := iTmp;
                                       if (odGrayed in uFlags) and (Canvas.Brush.Color <> clHighlight) // 28.03.00
                                        then Canvas.Font.Color := clBlue;
                                    end;
                                end
                            else
                              if Copy(szTag,2,5)='/FONT' then
                                begin
                                  Result := -2;
                                end
                              else
                                begin
                                  Result := -100;
                                end;
        end;
    end;


    function ProcessString(var X, Y, XMax, YMax,LineSizeY, Index: Integer; szTmp: String; iStartTag: Integer = 0):Boolean;
    var
      FFont : TFont;
      BColor: TColor;
      BStyle        : TBrushStyle;
      szTag         : String;
      iWidth        : Integer;
      iTag          : Integer;
      Rect          : TRect;
      i,iMax        : Integer;
      szWord        : String;
      szOval        : String;
      szOldWords    : String;

      function DrawWords: Boolean;
      var
        iOval : Integer;
      begin
        Result := False;
        if Canvas.TextHeight('0')>LineSizeY then
          LineSizeY := Canvas.TextHeight('0');
        if szOldWords <> '' then
        begin
          Rect.Left   := X;
          Rect.Top    := Y + 1;
          if (not bCalcOnly) then
            DrawText(Canvas.Handle, Pchar(szOldWords), Length(szOldWords),
             Rect,DT_LEFT or DT_SINGLELINE);
          Inc(X, Canvas.TextWidth(szOldWords));
          szOldWords := '';
        end;
        if szWord <> '' then
        begin
          iWidth := Canvas.TextWidth(szWord);
          if (X + iWidth > XMax) or (iWidth > XMax) then
          begin
            if ((Y + Canvas.TextHeight(szWord) * 2 + 2) > YMax) or (iWidth > XMax) then
            begin
              for iOval := Length(szWord) downto 1 do
              begin
                szOval := Copy(szWord, 1, iOval) + '...';
                if X + Canvas.TextWidth(szOval) <= XMax then
                 begin
                   Rect.Left   := X;
                   Rect.Top    := Y + 1;
                   if not bCalcOnly then
                     DrawText(Canvas.Handle, Pchar(szOval), Length(szOval),
                      Rect,DT_LEFT or DT_SINGLELINE);
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
          if not Result then
            begin
              Rect.Left   := X;
              Rect.Top    := Y + 1;
              if (not bCalcOnly) and (YMax - Y >= Canvas.TextHeight(szWord)) then
                DrawText(Canvas.Handle, Pchar(szWord), Length(szWord),
                 Rect,DT_LEFT or DT_SINGLELINE);
              Inc(X, iWidth);
            end;
          szWord := '';
        end;
      end;

    begin
      FFont := TFont.Create;
      try
        Result := False;
        with Canvas do
          begin
            FFont.Assign(Font);
            BColor := Brush.Color;
            BStyle := Brush.Style;

            iTag := HTMLGetTag(szTmp, Index, szTag);

            while (iTag >= 0)  do
              begin
                case iTag of
                  0:
                    begin
                      Rect.Right  := XMax;
                      Rect.Bottom := YMax;
                      szWord := '';
                      szOldWords := '';
                      iMax := Length(szTag);
                      for i:=1 to iMax do
                        begin
                          if szTag[i]=' ' then
                            begin
                              szWord := szWord+' ';
                              iWidth := TextWidth(szOldWords+szWord);
                              if (X + iWidth  > XMax) then
                                begin
                                  if DrawWords then
                                    begin
                                      Result := True;
                                      Break;
                                    end;
                                end
                              else
                                begin
                                  szOldWords := szOldWords+szWord;
                                  szWord := '';
                                end;
                            end
                          else
                            szWord := szWord+szTag[i];
                        end;
                      DrawWords;
                    end;

                  1:
                    begin
                      iTag := ProcessTag(Canvas, szTag);
                      if iTag=3 then
                        begin
                           X := XOrg;
                           if LineSizeY=0 then
                             Inc(Y, Canvas.TextHeight('0'))
                           else
                             Inc(Y, LineSizeY);
                           LineSizeY := 0;
                        end
                      else
                        begin
                          if (iTag = -iStartTag)  then
                            Break;
                          Result := ProcessString(X, Y, XMax, YMax, LineSizeY,Index, szTmp, iTag);
                          if Result then
                            Break;
                          Brush.Color := BColor;
                          Brush.Style := BStyle;
                          Font.Assign(FFont);
                        end;
                      if (X > XMax) or (Y > YMax) then
                        Break;
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
    szStr := ReplaceStr(ReplaceSpecialCharsFunc(szStr), #13#10, '');
    FLineSizeY :=0;
    ProcessString(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, FLineSizeY,i, szStr);
    Result := iCount;
  end;

  function HTMLGetTag(szTmp: String; var Index: Integer; var szTag: String): Integer;
  var
    i: Integer;
    iCount: Integer;
    OldIndex: Integer;
    bTag: Boolean;
  begin
    iCount:= Length(szTmp);
    Result:= -1;
    if Index > iCount then Exit;
    bTag:= (szTmp[Index] = '<');
    szTag:= szTmp[Index];
    OldIndex:= Index;
    Index:= iCount + 1;
    for i:= OldIndex + 1 to iCount do
      begin
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
    if not bTag then
      Result:= 0
    else
      Result:= 1;
  end;


end.
