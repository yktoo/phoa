//**********************************************************************************************************************
//  $Id: phPicFilterHighlighter.pas,v 1.1 2004-11-16 14:37:55 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPicFilterHighlighter;

{$I SynEdit.inc}

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
    fCommentAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fPropertyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
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
    property CommentAttri:  TSynHighlighterAttributes read fCommentAttri  write fCommentAttri;
    property KeyAttri:      TSynHighlighterAttributes read fKeyAttri      write fKeyAttri;
    property PropertyAttri: TSynHighlighterAttributes read fPropertyAttri write fPropertyAttri;
    property NumberAttri:   TSynHighlighterAttributes read fNumberAttri   write fNumberAttri;
    property SpaceAttri:    TSynHighlighterAttributes read fSpaceAttri    write fSpaceAttri;
    property StringAttri:   TSynHighlighterAttributes read fStringAttri   write fStringAttri;
    property SymbolAttri:   TSynHighlighterAttributes read fSymbolAttri   write fSymbolAttri;
  end;

implementation

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
      inc(ToHash);
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
    for c := 1 to fStringLen do
    begin
      if mHashTable[pKey1^] <> mHashTable[pKey2^] then
      begin
        Result := FALSE;
        exit;
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
         #0:                        fProcTable[c] := NullProc;
        #10:                        fProcTable[c] := LFProc;
        #13:                        fProcTable[c] := CRProc;
        #39:                        fProcTable[c] := AsciiCharProc;
        '=':                        fProcTable[c] := EqualProc;
        '>':                        fProcTable[c] := GreaterProc;
        '<':                        fProcTable[c] := LowerProc;
        '-':                        fProcTable[c] := MinusProc;
        '|':                        fProcTable[c] := OrSymbolProc;
        '+':                        fProcTable[c] := PlusProc;
        '/':                        fProcTable[c] := SlashProc;
        '&':                        fProcTable[c] := AndSymbolProc;
        #34:                        fProcTable[c] := StringProc;
        'A'..'Z', 'a'..'z', '_':    fProcTable[c] := IdentProc;
        '0'..'9':                   fProcTable[c] := NumberProc;
        #1..#9, #11, #12, #14..#32: fProcTable[c] := SpaceProc;
        '^', '%', '*', '!':         fProcTable[c] := SymbolAssignProc;
        '{', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~', ':', '@':
                                    fProcTable[c] := SymbolProc;
          else                      fProcTable[c] := UnknownProc;
      end;
  end;

  constructor TSynPicFilterSyn.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    fKeywords := TSynHashEntryList.Create;
    EnumerateKeywords(Ord(tkKey),      SFilterKeywords, IdentChars, DoAddKeyword);
    EnumerateKeywords(Ord(tkProperty), SFilterPicProps, IdentChars, DoAddKeyword);
    DefHighlightChange(Self);

    fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
    fCommentAttri.Style := [fsItalic];
    AddAttribute(fCommentAttri);

    fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
    fKeyAttri.Style := [fsBold];
    AddAttribute(fKeyAttri);

    fPropertyAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
    fPropertyAttri.Style := [fsBold];
    AddAttribute(fPropertyAttri);
    
    fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
    fNumberAttri.Foreground := clGreen;
    fNumberAttri.Style      := [fsBold];
    AddAttribute(fNumberAttri);

    fStringAttri := TSynHighlighterAttributes.Create(SYNS_Attrstring);
    fStringAttri.Foreground := clRed;
    fStringAttri.Style      := [fsBold];
    AddAttribute(fStringAttri);

    fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
    fSymbolAttri.Foreground := clBlue;
    fSymbolAttri.Style      := [fsBold];
    AddAttribute(fSymbolAttri);

    fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
    AddAttribute(fSpaceAttri);

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

  procedure TSynPicFilterSyn.AndSymbolProc;
  begin
    fTokenID := tkSymbol;
    Inc(Run);
    if fLine[Run] in ['=', '&'] then Inc(Run);
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

  procedure TSynPicFilterSyn.EqualProc;
  begin
    fTokenID := tkSymbol;
    Inc(Run);
    if fLine[Run] in ['=', '>'] then Inc(Run);
  end;

  procedure TSynPicFilterSyn.GreaterProc;
  begin
    fTokenID := tkSymbol;
    Inc(Run);
    if fLine[Run] in ['=', '>'] then Inc(Run);
  end;

  procedure TSynPicFilterSyn.IdentProc;
  begin
    fTokenID := IdentKind((fLine + Run));
    inc(Run, fStringLen);
  {begin}                                                                         // DJLP 2000-08-11
    if fTokenID = tkComment then begin
      while not (fLine[Run] in [#0, #10, #13]) do
        Inc(Run);
    end else
  {end}                                                                           // DJLP 2000-08-11
      while Identifiers[fLine[Run]] do inc(Run);
  end;

  procedure TSynPicFilterSyn.LFProc;
  begin
    fTokenID := tkSpace;
    inc(Run);
  end;

  procedure TSynPicFilterSyn.LowerProc;
  begin
    fTokenID := tkSymbol;
    Inc(Run);
    case fLine[Run] of
      '=': Inc(Run);
      '<': begin
             Inc(Run);
             if fLine[Run] = '=' then Inc(Run);
           end;
    end;
  end;

  procedure TSynPicFilterSyn.MinusProc;
  begin
    Inc(Run);
    if fLine[Run] = '-' then begin
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
    inc(Run);
    fTokenID := tkNumber;
    while FLine[Run] in ['0'..'9', '.', '-'] do begin
      case FLine[Run] of
        '.':
          if FLine[Run + 1] = '.' then break;
      end;
      inc(Run);
    end;
  end;

  procedure TSynPicFilterSyn.OrSymbolProc;
  begin
    fTokenID := tkSymbol;
    Inc(Run);
    if fLine[Run] in ['=', '|'] then Inc(Run);
  end;

  procedure TSynPicFilterSyn.PlusProc;
  begin
    fTokenID := tkSymbol;
    Inc(Run);
    if fLine[Run] in ['=', '+'] then Inc(Run);
  end;

  procedure TSynPicFilterSyn.SlashProc;
  begin
    Inc(Run);
    case fLine[Run] of
      '*':
        begin
          fRange := rsComment;
          fTokenID := tkComment;
          repeat
            Inc(Run);
            if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
              fRange := rsUnknown;
              Inc(Run, 2);
              break;
            end;
          until fLine[Run] in [#0, #10, #13];
        end;
      '=':
        begin
          Inc(Run);
          fTokenID := tkSymbol;
        end;
      else
        fTokenID := tkSymbol;
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
        '\': if fLine[Run + 1] = #34 then
               Inc(Run);
        #34: if fLine[Run + 1] <> #34 then
             begin
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

  procedure TSynPicFilterSyn.SymbolAssignProc;
  begin
    fTokenID := tkSymbol;
    Inc(Run);
    if fLine[Run] = '=' then Inc(Run);
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
      rsComment:
        AnsiCProc;
      rsString:
        AsciiCharProc;
    else
      fProcTable[fLine[Run]];
    end;
  end;

  function TSynPicFilterSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
  begin
    case Index of
      SYN_ATTR_COMMENT:    Result := fCommentAttri;
      SYN_ATTR_KEYWORD:    Result := fKeyAttri;
      SYN_ATTR_STRING:     Result := fStringAttri;
      SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
      SYN_ATTR_SYMBOL:     Result := fSymbolAttri;
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
      tkComment:   Result := fCommentAttri;
      tkKey:      Result := fKeyAttri;
      tkProperty: Result := fPropertyAttri;
      tkNumber:   Result := fNumberAttri;
      tkSpace:    Result := fSpaceAttri;
      tkString:   Result := fStringAttri;
      tkSymbol:   Result := fSymbolAttri;
      tkUnknown:  Result := fSpaceAttri;
      else        Result := nil;
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
