//**********************************************************************************************************************
//  $Id: phoaXMLGen.pas,v 1.2 2004-04-15 12:54:11 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phoaXMLGen;

interface
uses XMLIntf, XMLDoc, Contnrs, SysUtils, phPhoa;

type
   // Wrapper to IXMLNode, which itself is a TObject
  TNodeWrapper = class(TObject)
  public
    NodeIntf: IXMLNode;
  end;

   // PhoA XML generator class
  TPhoaXMLGen = class(TObject)
  private
    FStack: TObjectStack;
    FXMLDoc: IXMLDocument;
    FPicNode: IXMLNode;
    FGroupNode: IXMLNode;
     // Opening/closing groups
    procedure groupOpen;
    procedure groupClose;
     // Opening/closing pictures
    procedure picOpen;
    procedure picClose;
     // Adds an attribute to the root XML document node
    procedure SetDocAttr(const sName: String; const v: Variant);
     // Adds an attribute to the current group node
    procedure SetGroupAttr(const sName: String; const v: Variant);
     // Adds an attribute to the current picture node
    procedure SetPicAttr(const sName: String; const v: Variant);
     // Adds a child picture node to the current group
    procedure AddPictureToGroup(const vID: Variant);
  public
    constructor Create;
    destructor Destroy; override;
     // Tries to recognize a chunk by its code, then converts its value to an XML node
    procedure ParseChunk(Code: TPhChunkCode; Datatype: TPhChunkDatatype; const vValue: Variant);
     // Stores the XML document into the given file
    procedure SaveToFile(const sFileName: String);
  end;

implementation

  function PhoaDateToXML(iDate: Integer): String;
  begin
    Result := FormatDateTime('yyyy-mm-dd', PhoaDateToDate(iDate));
  end;

  function PhoaTimeToXML(iTime: Integer): String;
  begin
    Result := FormatDateTime('hh:nn:ss', PhoaTimeToTime(iTime));
  end;

   //===================================================================================================================
   // TPhoaXMLGen
   //===================================================================================================================

  procedure TPhoaXMLGen.AddPictureToGroup(const vID: Variant);
  begin
    FGroupNode.AddChild('pictureRef').SetAttributeNS('id', '', vID);
  end;

  constructor TPhoaXMLGen.Create;
  begin
    FXMLDoc := LoadXMLData(
      '<?xml version="1.0" encoding=''windows-1251''?>'+
        '<album/>');
    FGroupNode := FXMLDoc.DocumentElement;
    FXMLDoc.Options := [doNodeAutoIndent];
    FStack := TObjectStack.Create;
  end;

  destructor TPhoaXMLGen.Destroy;
  begin
    FStack.Free;
    inherited Destroy;
  end;

  procedure TPhoaXMLGen.groupClose;
  var w: TNodeWrapper;
  begin
    w := FStack.Pop as TNodeWrapper;
    FGroupNode := w.NodeIntf;
    w.Free;
  end;

  procedure TPhoaXMLGen.groupOpen;
  var w: TNodeWrapper;
  begin
    w := TNodeWrapper(FStack.Push(TNodeWrapper.Create));
    w.NodeIntf := FGroupNode;
    FGroupNode := FGroupNode.AddChild('group');
  end;

  procedure TPhoaXMLGen.ParseChunk(Code: TPhChunkCode; Datatype: TPhChunkDatatype; const vValue: Variant);
  begin
    case Code of
       // Photo album data chunks
      IPhChunk_PhoaGenerator:   SetDocAttr('generator', vValue);
      IPhChunk_PhoaDescription: SetDocAttr('desc',      vValue);
       // Group data chunks
      IPhChunk_Group_Open:      groupOpen;
      IPhChunk_Group_Close:     groupClose;
      IPhChunk_GroupPic_ID:     AddPictureToGroup(vValue);
      IPhChunk_Group_Text:      SetGroupAttr('desc',    vValue);
       // Picture data chunks
      IPhChunk_Pic_Open:        picOpen;
      IPhChunk_Pic_Close:       picClose;
      IPhChunk_Pic_ID:          SetPicAttr('id',        vValue);
      IPhChunk_Pic_PicFileName: SetPicAttr('fileName',  vValue);
      IPhChunk_Pic_PicFileSize: SetPicAttr('fileSize',  vValue);
      IPhChunk_Pic_PicWidth:    SetPicAttr('width',     vValue);
      IPhChunk_Pic_PicHeight:   SetPicAttr('height',    vValue);
      IPhChunk_Pic_Date:        SetPicAttr('date',      PhoaDateToXML(vValue));
      IPhChunk_Pic_Time:        SetPicAttr('time',      PhoaTimeToXML(vValue));
      IPhChunk_Pic_Place:       SetPicAttr('place',     vValue);
      IPhChunk_Pic_FilmNumber:  SetPicAttr('filmNo',    vValue);
      IPhChunk_Pic_FrameNumber: SetPicAttr('frameNo',   vValue);
      IPhChunk_Pic_Author:      SetPicAttr('author',    vValue);
      IPhChunk_Pic_Media:       SetPicAttr('media',     vValue);
      IPhChunk_Pic_Desc:        SetPicAttr('desc',      vValue);
      IPhChunk_Pic_Notes:       SetPicAttr('notes',     vValue);
      IPhChunk_Pic_Keywords:    SetPicAttr('keywords',  vValue);
    end;
  end;

  procedure TPhoaXMLGen.picClose;
  begin
    FPicNode := nil;
  end;

  procedure TPhoaXMLGen.picOpen;
  begin
    FPicNode := FXMLDoc.DocumentElement.AddChild('picture');
  end;

  procedure TPhoaXMLGen.SaveToFile(const sFileName: String);
  begin
    FXMLDoc.SaveToFile(sFileName);
  end;

  procedure TPhoaXMLGen.SetDocAttr(const sName: String; const v: Variant);
  begin
    FXMLDoc.DocumentElement.SetAttributeNS(sName, '', v);
  end;

  procedure TPhoaXMLGen.SetGroupAttr(const sName: String; const v: Variant);
  begin
    FGroupNode.SetAttributeNS(sName, '', v);
  end;

  procedure TPhoaXMLGen.SetPicAttr(const sName: String; const v: Variant);
  begin
    FPicNode.SetAttributeNS(sName, '', v);
  end;

end.
