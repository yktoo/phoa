//**********************************************************************************************************************
//  $Id: phIntf.pas,v 1.4 2004-09-27 17:07:22 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phIntf;

interface

uses Windows;

type
   // A picture rotation angle (measured clockwise)
  TPicRotation = (pr0, pr90, pr180, pr270);

   // Picture flip flags (pfl prefix used for avoiding confusing with TPixelFormat)
  TPicFlip = (pflHorz, pflVert);
  TPicFlips = set of TPicFlip;

   //-------------------------------------------------------------------------------------------------------------------
   // PhoA picture interface
   //-------------------------------------------------------------------------------------------------------------------

  IPhoaPic = interface(IInterface)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68EE}']
     // Prop handlers
    function  GetID: Integer; stdcall;
    function  GetAuthor: PAnsiChar; stdcall;
    function  GetDate: Integer; stdcall;
    function  GetTime: Integer; stdcall;
    function  GetDescription: PAnsiChar; stdcall;
    function  GetFileName: PAnsiChar; stdcall;
    function  GetFileSize: Integer; stdcall;
    function  GetFilmNumber: PAnsiChar; stdcall;
    function  GetFlips: TPicFlips; stdcall;
    function  GetFrameNumber: PAnsiChar; stdcall;
    function  GetImageSize: TSize; stdcall;
    function  GetKeywords: PAnsiChar; stdcall;
    function  GetMedia: PAnsiChar; stdcall;
    function  GetNotes: PAnsiChar; stdcall;
    function  GetPlace: PAnsiChar; stdcall;
    function  GetPropertyValue(pcPropName: PAnsiChar): PAnsiChar; stdcall;
    function  GetRotation: TPicRotation; stdcall;
    function  GetThumbnailSize: TSize; stdcall;
    function  GetThumbnailData: Pointer; stdcall;
    function  GetThumbnailDataSize: Integer; stdcall;
     // Props
     // -- Unique ID
    property ID: Integer read GetID;
     // -- Author
    property Author: PAnsiChar read GetAuthor;
     // -- Picture date: number of days since Jan 01, 0001
    property Date: Integer read GetDate;
     // -- Picture date: number of seconds since midnight
    property Time: Integer read GetTime;
     // -- Description (for display)
    property Description: PAnsiChar read GetDescription;
     // -- Full picture file name
    property FileName: PAnsiChar read GetFileName;
     // -- Picture file size in bytes
    property FileSize: Integer read GetFileSize;
     // -- Film number or name
    property FilmNumber: PAnsiChar read GetFilmNumber;
     // -- Picture flip flags
    property Flips: TPicFlips read GetFlips;
     // -- Frame number
    property FrameNumber: PAnsiChar read GetFrameNumber;
     // -- Image dimensions in pixels
    property ImageSize: TSize read GetImageSize;
     // -- Comma-delimited list of keywords. Ones having commas or spaces within are enclosed in double quotes
    property Keywords: PAnsiChar read GetKeywords;
     // -- Picture file media name or code
    property Media: PAnsiChar read GetMedia;
     // -- Notes
    property Notes: PAnsiChar read GetNotes;
     // -- Place
    property Place: PAnsiChar read GetPlace;
     // -- Picture property value by name (name is on of the SPhoaPicProp_XXX constants), represented as string. Invalid
     //    values of pcPropName cause nil to be returned
    property PropertyValue[pcPropName: PAnsiChar]: PAnsiChar read GetPropertyValue;
     // -- Picture rotation
    property Rotation: TPicRotation read GetRotation;
     // -- Thumbnail dimensions in pixels
    property ThumbnailSize: TSize read GetThumbnailSize;
     // -- Thumbnail raw binary JPEG data 
    property ThumbnailData: Pointer read GetThumbnailData;
     // -- The size of thumbnail data 
    property ThumbnailDataSize: Integer read GetThumbnailDataSize;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // A list of images
   //-------------------------------------------------------------------------------------------------------------------

  IPhoaPicList = interface(IInterface)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68EF}']
     // Returns index of a picture by its ID, or -1 if no such item found
    function  IndexOfID(iID: Integer): Integer; stdcall;
     // Returns index of a picture by its full file name (case-insensitive search), or -1 if no such item found
    function  IndexOfFileName(pcFileName: PAnsiChar): Integer; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall;
    function  GetItemsByID(iID: Integer): IPhoaPic; stdcall;
    function  GetItemsByFileName(pcFileName: PAnsiChar): IPhoaPic; stdcall;
    function  GetItems(Index: Integer): IPhoaPic; stdcall;
     // Props
     // -- Count of pictures in the list
    property Count: Integer read GetCount;
     // -- Pictures by ID; returns nil if no such item found
    property ItemsByID[iID: Integer]: IPhoaPic read GetItemsByID;
     // -- Pictures by full file name (case-insensitive search); returns nil if no such item found
    property ItemsByFileName[pcFileName: PAnsiChar]: IPhoaPic read GetItemsByFileName;
     // -- Pictures by index; Index in range 0..Count-1
    property Items[Index: Integer]: IPhoaPic read GetItems; default;
  end;

const
   // Picture property names, case-insensitive. DO NOT LOCALIZE!
  SPhoaPicProp_ID            = 'ID';
  SPhoaPicProp_FileName      = 'FileName';
  SPhoaPicProp_FullFileName  = 'FullFileName';
  SPhoaPicProp_FilePath      = 'FilePath';
  SPhoaPicProp_FileSize      = 'FileSize';
  SPhoaPicProp_FileSizeBytes = 'FileSizeBytes';
  SPhoaPicProp_PicWidth      = 'PicWidth';
  SPhoaPicProp_PicHeight     = 'PicHeight';
  SPhoaPicProp_PicDims       = 'PicDims';
  SPhoaPicProp_Format        = 'Format';
  SPhoaPicProp_Date          = 'Date';
  SPhoaPicProp_Time          = 'Time';
  SPhoaPicProp_Place         = 'Place';
  SPhoaPicProp_FilmNumber    = 'FilmNumber';
  SPhoaPicProp_FrameNumber   = 'FrameNumber';
  SPhoaPicProp_Author        = 'Author';
  SPhoaPicProp_Description   = 'Description';
  SPhoaPicProp_Notes         = 'Notes';
  SPhoaPicProp_Media         = 'Media';
  SPhoaPicProp_Keywords      = 'Keywords';
  SPhoaPicProp_Rotation      = 'Rotation';
  SPhoaPicProp_Flips         = 'Flips';

implementation

end.
