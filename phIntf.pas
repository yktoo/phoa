//**********************************************************************************************************************
//  $Id: phIntf.pas,v 1.10 2004-10-11 11:41:24 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phIntf;

interface

uses Windows;

type
   // Picture rotation angle (measured clockwise)
  TPicRotation = (pr0, pr90, pr180, pr270);

   // Picture flip flags 
  TPicFlip = (pflHorz, pflVert);
  TPicFlips = set of TPicFlip;

   // Pixel image format
  TPhoaPixelFormat = (ppfDevice, ppf1bit, ppf4bit, ppf8bit, ppf15bit, ppf16bit, ppf24bit, ppf32bit, ppfCustom); 

  TPhoaHandle = Cardinal;

   //===================================================================================================================
   // PhoA picture interface
   //===================================================================================================================

  IPhoaKeywordList = interface; 

  IPhoaPic = interface(IInterface)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68EE}']
     // Prop handlers
    function  GetID: Integer; stdcall;
    function  GetAuthor: String; stdcall;
    function  GetDate: Integer; stdcall;
    function  GetTime: Integer; stdcall;
    function  GetDescription: String; stdcall;
    function  GetFileName: String; stdcall;
    function  GetFileSize: Integer; stdcall;
    function  GetFilmNumber: String; stdcall;
    function  GetFlips: TPicFlips; stdcall;
    function  GetFrameNumber: String; stdcall;
    function  GetImageFormat: TPhoaPixelFormat;
    function  GetImageSize: TSize; stdcall;
    function  GetKeywords: IPhoaKeywordList; stdcall;
    function  GetMedia: String; stdcall;
    function  GetNotes: String; stdcall;
    function  GetPlace: String; stdcall;
    function  GetPropertyValue(const sPropName: String): String; stdcall;
    function  GetRotation: TPicRotation; stdcall;
    function  GetThumbnailSize: TSize; stdcall;
    function  GetThumbnailData: String; stdcall;
     // Props
     // -- Unique ID
    property ID: Integer read GetID;
     // -- Author
    property Author: String read GetAuthor;
     // -- Picture date: number of days since Jan 01, 0001
    property Date: Integer read GetDate;
     // -- Picture date: number of seconds since midnight
    property Time: Integer read GetTime;
     // -- Description (for display)
    property Description: String read GetDescription;
     // -- Full picture file name
    property FileName: String read GetFileName;
     // -- Picture file size in bytes
    property FileSize: Integer read GetFileSize;
     // -- Film number or name
    property FilmNumber: String read GetFilmNumber;
     // -- Picture flip flags
    property Flips: TPicFlips read GetFlips;
     // -- Frame number
    property FrameNumber: String read GetFrameNumber;
     // -- Pixel image format
    property ImageFormat: TPhoaPixelFormat read GetImageFormat;
     // -- Image dimensions in pixels
    property ImageSize: TSize read GetImageSize;
     // -- Keyword list
    property Keywords: IPhoaKeywordList read GetKeywords;
     // -- Picture file media name or code
    property Media: String read GetMedia;
     // -- Notes
    property Notes: String read GetNotes;
     // -- Place
    property Place: String read GetPlace;
     // -- Picture property value by name (name is on of the SPhoaPicProp_XXX constants), represented as string. Invalid
     //    values of pcPropName cause nil to be returned
    property PropertyValue[const sPropName: String]: String read GetPropertyValue;
     // -- Picture rotation
    property Rotation: TPicRotation read GetRotation;
     // -- Thumbnail dimensions in pixels
    property ThumbnailSize: TSize read GetThumbnailSize;
     // -- Thumbnail raw binary JPEG data
    property ThumbnailData: String read GetThumbnailData;
  end;

   //===================================================================================================================
   // Sorted list of picture keywords
   //===================================================================================================================

  IPhoaKeywordList = interface(IInterface)
    ['{C000BFAE-CE13-4DF3-A774-8A21E1727D53}']
     // Returns index of a keyword, or -1 if no such item found
    function  IndexOf(const sKeyword: String): Integer; stdcall;
     // Prop handlers
    function  GetCommaText: String; stdcall;
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): String; stdcall;
     // Props
     // -- Comma-separated keyword list; keywords containing commas and/or spaces are double-quoted
    property CommaText: String read GetCommaText;
     // -- Number of keywords in the list
    property Count: Integer read GetCount;
     // -- Keywords by index; Index in range 0..Count-1
    property Items[Index: Integer]: String read GetItems; default;
  end;

   //===================================================================================================================
   // A list of pictures
   //===================================================================================================================

  IPhoaPicList = interface(IInterface)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68EF}']
     // Returns index of a picture by its ID, or -1 if no such item found
    function  IndexOfID(iID: Integer): Integer; stdcall;
     // Returns index of a picture by its full file name (case-insensitive search), or -1 if no such item found
    function  IndexOfFileName(const sFileName: String): Integer; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall;
    function  GetItemsByID(iID: Integer): IPhoaPic; stdcall;
    function  GetItemsByFileName(const sFileName: String): IPhoaPic; stdcall;
    function  GetItems(Index: Integer): IPhoaPic; stdcall;
    function  GetMaxPicID: Integer; stdcall;
     // Props
     // -- Number of pictures in the list
    property Count: Integer read GetCount;
     // -- Pictures by ID; returns nil if no such item found
    property ItemsByID[iID: Integer]: IPhoaPic read GetItemsByID;
     // -- Pictures by full file name (case-insensitive search); returns nil if no such item found
    property ItemsByFileName[const sFileName: String]: IPhoaPic read GetItemsByFileName;
     // -- Pictures by index; Index in range 0..Count-1
    property Items[Index: Integer]: IPhoaPic read GetItems; default;
     // -- Maximum picture ID of the list
    property MaxPicID: Integer read GetMaxPicID;
  end;

   //===================================================================================================================
   // Picture group
   //===================================================================================================================

  IPhoaPicGroupList = interface;

  IPhoaPicGroup = interface(IInterface)
    ['{9C951B51-2C66-4C35-B61B-8EDCBEAD8ABE}']
     // Returns True if a picture ID is in the group's list (when bRecursive=True also in any of its subgroups)
    function  IsPicLinked(iID: Integer; bRecursive: Boolean): Boolean; stdcall;
     // Prop handlers
    function  GetDescription: String; stdcall;
    function  GetExpanded: Boolean; stdcall;
    function  GetMaxGroupID: Integer; stdcall;
    function  GetGroups: IPhoaPicGroupList; stdcall;
    function  GetGroupByID(iID: Integer): IPhoaPicGroup; stdcall;
    function  GetGroupByPath(const sPath: String): IPhoaPicGroup; stdcall;
    function  GetID: Integer; stdcall;
    function  GetIndex: Integer; stdcall;
    function  GetNestedGroupCount: Integer; stdcall;
    function  GetOwner: IPhoaPicGroup; stdcall;
    function  GetPath(const sRootName: String): String; stdcall;
    function  GetPics: IPhoaPicList; stdcall;
    function  GetRoot: IPhoaPicGroup; stdcall;
    function  GetText: String; stdcall;
     // Props
     // -- Group description
    property Description: String read GetDescription;
     // -- True if a group node is expanded
    property Expanded: Boolean read GetExpanded;
     // -- Owned group list
    property Groups: IPhoaPicGroupList read GetGroups;
     // -- Finds and returns the group by its ID (recursively); nil if no such group
    property GroupByID[iID: Integer]: IPhoaPicGroup read GetGroupByID;
     // -- Returns the group by its path; nil if no such group. The search is case-insensitive; starts searching from
     //    the first owned group. If sPath starts with '/', ignores this char
    property GroupByPath[const sPath: String]: IPhoaPicGroup read GetGroupByPath;
     // -- An unique group ID
    property ID: Integer read GetID;
     // -- Group index in its Owner's list
    property Index: Integer read GetIndex;
     // -- Maximum ID of the group and all owned groups
    property MaxGroupID: Integer read GetMaxGroupID;
     // -- Recursive number of owned groups
    property NestedGroupCount: Integer read GetNestedGroupCount;
     // -- Group owner
    property Owner: IPhoaPicGroup read GetOwner;
     // -- Group path in form '<sRootName>/Group1/Group2/.../CurrentGroup'
    property Path[const sRootName: String]: String read GetPath;
     // -- Pictures in the group
    property Pics: IPhoaPicList read GetPics;
     // -- Returns the ultimate owner of the group (all the groups)
    property Root: IPhoaPicGroup read GetRoot;
     // -- Group text (name)
    property Text: String read GetText;
  end;

   //===================================================================================================================
   // Picture group's group list
   //===================================================================================================================

  IPhoaPicGroupList = interface(IInterface)
    ['{5B299022-5911-4154-8307-37170FDD7950}']
     // Returns index of a group by its ID; -1 if no such item found
    function  IndexOfID(iID: Integer): Integer; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall; 
    function  GetItems(Index: Integer): IPhoaPicGroup; stdcall;
    function  GetOwner: IPhoaPicGroup; stdcall;
     // Props
     // -- Number of items
    property Count: Integer read GetCount;
     // -- Items by index
    property Items[Index: Integer]: IPhoaPicGroup read GetItems; default;
     // -- List owner group, if any (nil otherwise)
    property Owner: IPhoaPicGroup read GetOwner;
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
