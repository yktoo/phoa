//**********************************************************************************************************************
//  $Id: phPicFilterHighlighter.pas,v 1.2 2004-11-16 17:49:05 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPicFilterHighlighter;

interface

uses
  Graphics,
  Registry,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkKey, tkProperty, tkNull, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TRangeState = (rsUnknown, rsComment, rsString);

  TProcTableProc = procedure of object;

  TIdentifierTable = array[Char] of ByteBool;
  THashTable       = array[Char] of Integer;

  TSynPicFilterSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fKeywords: TSynHashEntryList;
     // Prop storage
    FCommentAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FPropertyAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function  IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function  GetEol: Boolean; override;
    function  GetRange: Pointer; override;
    function  GetToken: string; override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  GetTokenID: TtkTokenKind;
    function  GetTokenKind: integer; override;
    function  GetTokenPos: Integer; override;
    function  IsKeyword(const AKeyword: string): boolean; override;              // DJLP 2000-08-09
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri:  TSynHighlighterAttributes read FCommentAttri  write FCommentAttri;
    property KeyAttri:      TSynHighlighterAttributes read FKeyAttri      write FKeyAttri;
    property PropertyAttri: TSynHighlighterAttributes read FPropertyAttri write FPropertyAttri;
    property NumberAttri:   TSynHighlighterAttributes read FNumberAttri   write FNumberAttri;
    property SpaceAttri:    TSynHighlighterAttributes read FSpaceAttri    write FSpaceAttri;
    property StringAttri:   TSynHighlighterAttributes read FStringAttri   write FStringAttri;
    property SymbolAttri:   TSynHighlighterAttributes read FSymbolAttri   write FSymbolAttri;
  end;

implementation
uses SynEditStrConst;

var
  Identifiers: TIdentifierTable;
  mHashTable: THashTable;

const
  SFilterKeywords: String = 'and,or,not,in,startswith,endswith,contains,isempty';
  SFilterPicProps: String =
   'id,author,date,time,description,filename,filesize,filmnumber,flips,framenumber,imageformat,imagewidth,imageheight,'+
   'keywords,media,notes,place,rotation,thumbnailwidth,thumbnailheight';

  procedure MakeIdentTable;
  var c: char;
  begin
    FillChar(Identifiers, SizeOf(Identifiers), 0);
    for c := 'a' to 'z' do Identifiers[c] := True;
    for c := 'A' to 'Z' do Identifiers[c] := True;
    for c := '0' to '9' do Identifiers[c] := True;
    Identifiers['_'] := True;
    Identifiers['#'] := True;
    Identifiers['$'] := True;
    FillChar(mHashTable, SizeOf(mHashTable), 0);
    mHashTable['_'] := 1;
    for c := 'a' to 'z' do mHashTable[c] := 2 + Ord(c) - Ord('a');
    for c := 'A' to 'Z' do mHashTable[c] := 2 + Ord(c) - Ord('A');
  end;

  function TSynPicFilterSyn.KeyHash(ToHash: PChar): Integer;
  begin
    Result := 0;
    while Identifiers[ToHash^] do begin
      Result := 2*Result+mHashTable[ToHash^];
      Inc(ToHash);
    end;
    Result := Result and $FF; // 255
    fStringLen := ToHash - fToIdent;
  end;

  function TSynPicFilterSyn.KeyComp(const aKey: string): Boolean;
  var
    c: integer;
    pKey1, pKey2: PChar;
  begin
    pKey1 := fToIdent;
    // Note: fStringLen is always > 0 !
    pKey2 := pointer(aKey);
    for c := 1 to fStringLen do begin
      if mHashTable[pKey1^]<>mHashTable[pKey2^] then begin
        Result := FALSE;
        Exit;
      end;
      Inc(pKey1);
      Inc(pKey2);
    end;
    Result := True;
  end;

  function TSynPicFilterSyn.IdentKind(MayBe: PChar): TtkTokenKind;
  var Entry: TSynHashEntry;
  begin
    fToIdent := MayBe;
    Entry := fKeywords[KeyHash(MayBe)];
    while Assigned(Entry) do begin
      if Entry.KeywordLen > fStringLen then
        break
      else if Entry.KeywordLen = fStringLen then
        if KeyComp(Entry.Keyword) then begin
          Result := TtkTokenKind(Entry.Kind);
          exit;
        end;
      Entry := Entry.Next;
    end;
    Result := tkUnknown;
  end;

  procedure TSynPicFilterSyn.MakeMethodTables;
  var c: Char;
  begin
    for c := #0 to #255 do
      case c of
         #0:        fProcTable[c] := NullProc;
        #10:        fProcTable[c] := LFProc;
        #13:        fProcTable[c] := CRProc;
        #39:        fProcTable[c] := AsciiCharProc;
        #34:        fProcTable[c] := StringProc;
        '$', 'A'..'Z', 'a'..'z', '_':
                    fProcTable[c] := IdentProc;
        '0'..'9':   fProcTable[c] := NumberProc;
        #1..#9, #11, #12, #14..#32:
                    fProcTable[c] := SpaceProc;
        '-':        fProcTable[c] := MinusProc;
        '=', '>', '<', '|', '+', '/', '&', '^', '%', '*', '!', '{', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~',
          ':', '@': fProcTable[c] := SymbolProc;
        else        fProcTable[c] := UnknownProc;
      end;
  end;

  constructor TSynPicFilterSyn.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    fKeywords := TSynHashEntryList.Create;
    EnumerateKeywords(Ord(tkKey),      SFilterKeywords, IdentChars, DoAddKeyword);
    EnumerateKeywords(Ord(tkProperty), SFilterPicProps, IdentChars, DoAddKeyword);
    DefHighlightChange(Self);

    FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
    FCommentAttri.Style := [fsItalic];
    AddAttribute(FCommentAttri);

    FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
    FKeyAttri.Style := [fsBold];
    AddAttribute(FKeyAttri);

    FPropertyAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
    FPropertyAttri.Foreground := $763c28;
    FPropertyAttri.Style      := [fsBold];
    AddAttribute(FPropertyAttri);

    FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
    FNumberAttri.Foreground := clGreen;
    FNumberAttri.Style      := [fsBold];
    AddAttribute(FNumberAttri);

    FStringAttri := TSynHighlighterAttributes.Create(SYNS_Attrstring);
    FStringAttri.Foreground := clRed;
    FStringAttri.Style      := [fsBold];
    AddAttribute(FStringAttri);

    FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
    FSymbolAttri.Foreground := clBlue;
    FSymbolAttri.Style      := [fsBold];
    AddAttribute(FSymbolAttri);

    FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
    AddAttribute(FSpaceAttri);

    SetAttributesOnChange(DefHighlightChange);
    MakeMethodTables;
    fRange := rsUnknown;
  end;

  destructor TSynPicFilterSyn.Destroy;
  begin
    fKeywords.Free;
    inherited Destroy;
  end;

  procedure TSynPicFilterSyn.SetLine(NewValue: string; LineNumber: Integer);
  begin
    fLine := PChar(NewValue);
    Run := 0;
    fLineNumber := LineNumber;
    Next;
  end;

  procedure TSynPicFilterSyn.AsciiCharProc;
  begin
    // Oracle SQL allows strings to go over multiple lines
    if fLine[Run] = #0 then
      NullProc
    else begin
      fTokenID := tkString;
       // else it's end of multiline string
      if (Run > 0) or (fRange <> rsString) or (fLine[Run] <> #39) then begin
        fRange := rsString;
        repeat
          Inc(Run);
        until fLine[Run] in [#0, #10, #13, #39];
      end;
      if fLine[Run] = #39 then begin
        Inc(Run);
        fRange := rsUnknown;
      end;
    end;
  end;

  procedure TSynPicFilterSyn.CRProc;
  begin
    fTokenID := tkSpace;
    Inc(Run);
    if fLine[Run] = #10 then Inc(Run);
  end;

  procedure TSynPicFilterSyn.IdentProc;
  begin
    fTokenID := IdentKind(fLine+Run);
    Inc(Run, fStringLen);
    if fTokenID = tkComment then begin
      while not (fLine[Run] in [#0, #10, #13]) do Inc(Run);
    end else
      while Identifiers[fLine[Run]] do Inc(Run);
  end;

  procedure TSynPicFilterSyn.LFProc;
  begin
    fTokenID := tkSpace;
    Inc(Run);
  end;

  procedure TSynPicFilterSyn.MinusProc;
  begin
    Inc(Run);
    if fLine[Run]='-' then begin
      fTokenID := tkComment;
      repeat
        Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end else
      fTokenID := tkSymbol;
  end;

  procedure TSynPicFilterSyn.NullProc;
  begin
    fTokenID := tkNull;
  end;

  procedure TSynPicFilterSyn.NumberProc;
  begin
    Inc(Run);
    fTokenID := tkNumber;
    while FLine[Run] in ['0'..'9', '.', '-'] do begin
      case FLine[Run] of
        '.': if FLine[Run+1]='.' then break;
      end;
      Inc(Run);
    end;
  end;

  procedure TSynPicFilterSyn.SpaceProc;
  begin
    fTokenID := tkSpace;
    repeat
      Inc(Run);
    until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
  end;

  procedure TSynPicFilterSyn.StringProc;
  begin
    fTokenID := tkString;
    Inc(Run);
    while not (fLine[Run] in [#0, #10, #13]) do begin
      case fLine[Run] of
        '\': if fLine[Run+1]=#34 then Inc(Run);
        #34: if fLine[Run+1]<>#34 then begin
          Inc(Run);
          break;
        end;
      end;
      Inc(Run);
    end;
  end;

  procedure TSynPicFilterSyn.SymbolProc;
  begin
    Inc(Run);
    fTokenID := tkSymbol;
  end;

  procedure TSynPicFilterSyn.UnknownProc;
  begin
    Inc(Run);
    fTokenID := tkUnknown;
  end;

  procedure TSynPicFilterSyn.AnsiCProc;
  begin
    case fLine[Run] of
       #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      else begin
        fTokenID := tkComment;
        repeat
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
            fRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
          Inc(Run);
        until fLine[Run] in [#0, #10, #13];
      end;
    end;
  end;

  function TSynPicFilterSyn.IsKeyword(const AKeyword: string): boolean;
  var tk: TtkTokenKind;
  begin
    tk := IdentKind(PChar(AKeyword));
    Result := tk in [tkKey, tkProperty];
  end;

  procedure TSynPicFilterSyn.Next;
  begin
    fTokenPos := Run;
    case fRange of
      rsComment: AnsiCProc;
      rsString:  AsciiCharProc;
      else       fProcTable[fLine[Run]];
    end;
  end;

  function TSynPicFilterSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
  begin
    case Index of
      SYN_ATTR_COMMENT:    Result := FCommentAttri;
      SYN_ATTR_KEYWORD:    Result := FKeyAttri;
      SYN_ATTR_STRING:     Result := FStringAttri;
      SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
      SYN_ATTR_SYMBOL:     Result := FSymbolAttri;
      else                 Result := nil;
    end;
  end;

  function TSynPicFilterSyn.GetEol: Boolean;
  begin
    Result := fTokenID = tkNull;
  end;

  function TSynPicFilterSyn.GetRange: Pointer;
  begin
    Result := Pointer(fRange);
  end;

  function TSynPicFilterSyn.GetToken: string;
  var
    Len: LongInt;
  begin
    Len := Run - fTokenPos;
    Setstring(Result, (FLine + fTokenPos), Len);
  end;

  function TSynPicFilterSyn.GetTokenID: TtkTokenKind;
  begin
    Result := fTokenId;
  end;

  function TSynPicFilterSyn.GetTokenAttribute: TSynHighlighterAttributes;
  begin
    case GetTokenID of
      tkComment:   Result := FCommentAttri;
      tkKey:       Result := FKeyAttri;
      tkProperty:  Result := FPropertyAttri;
      tkNumber:    Result := FNumberAttri;
      tkSpace:     Result := FSpaceAttri;
      tkString:    Result := FStringAttri;
      tkSymbol:    Result := FSymbolAttri;
      tkUnknown:   Result := FSpaceAttri;
      else         Result := nil;
    end;
  end;

  function TSynPicFilterSyn.GetTokenKind: integer;
  begin
    Result := Ord(fTokenId);
  end;

  function TSynPicFilterSyn.GetTokenPos: Integer;
  begin
    Result := fTokenPos;
  end;

  procedure TSynPicFilterSyn.ResetRange;
  begin
    fRange := rsUnknown;
  end;

  procedure TSynPicFilterSyn.SetRange(Value: Pointer);
  begin
    fRange := TRangeState(Value);
  end;

  function TSynPicFilterSyn.GetIdentChars: TSynIdentChars;
  begin
    Result := TSynValidStringChars+['$'];
  end;

  class function TSynPicFilterSyn.GetLanguageName: string;
  begin
    Result := 'PhoA Parsing PicFilter Expression';
  end;

  procedure TSynPicFilterSyn.DoAddKeyword(AKeyword: string; AKind: integer);
  var HashValue: integer;
  begin
    HashValue := KeyHash(PChar(AKeyword));
    fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
  end;

initialization
  MakeIdentTable;
end.
