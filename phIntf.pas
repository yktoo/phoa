//**********************************************************************************************************************
//  $Id: phIntf.pas,v 1.12 2004-10-13 11:03:33 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phIntf;

interface

uses Windows;

type
   // Picture property
  TPicProperty = (
    ppID,
    ppFileName,
    ppFullFileName,
    ppFilePath,
    ppFileSize,
    ppFileSizeBytes,
    ppPicWidth,
    ppPicHeight,
    ppPicDims,
    ppFormat,
    ppDate,
    ppTime,
    ppPlace,
    ppFilmNumber,
    ppFrameNumber,
    ppAuthor,
    ppDescription,
    ppNotes,
    ppMedia,
    ppKeywords,
    ppRotation,
    ppFlips);
  TPicProperties = set of TPicProperty;
const
   // All picture properties
  PPAllProps = [Low(TPicProperty)..High(TPicProperty)];

type
   // Picture rotation angle (measured clockwise)
  TPicRotation = (pr0, pr90, pr180, pr270);

   // Picture flip flags
  TPicFlip = (pflHorz, pflVert);
  TPicFlips = set of TPicFlip;

   // Pixel image format
  TPhoaPixelFormat = (ppfDevice, ppf1bit, ppf4bit, ppf8bit, ppf15bit, ppf16bit, ppf24bit, ppf32bit, ppfCustom);

   // Stretch filter
  TPhoaStretchFilter = (psfNearest, psfDraft, psfLinear, psfCosine, psfSpline, psfLanczos, psfMitchell);

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
    function  GetImageFormat: TPhoaPixelFormat; stdcall;
    function  GetImageSize: TSize; stdcall;
    function  GetKeywords: IPhoaKeywordList; stdcall;
    function  GetMedia: String; stdcall;
    function  GetNotes: String; stdcall;
    function  GetPlace: String; stdcall;
    function  GetPropertyValue(const sPropName: String): String; stdcall;
    function  GetProps(PicProp: TPicProperty): String; stdcall;
    function  GetRawData(PProps: TPicProperties): String; stdcall;
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
     //    values of sPropName cause nil to be returned
    property PropertyValue[const sPropName: String]: String read GetPropertyValue;
     // -- Properties by index
    property Props[PicProp: TPicProperty]: String read GetProps;
     // -- Picture raw binary data (for PProps properties)
    property RawData[PProps: TPicProperties]: String read GetRawData;
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
     // Searches a picture by ID and returns True if found, and its position in Index. Otherwise returns False, and
     //   position of the nearest greater ID in Index
    function  FindID(iID: Integer; var Index: Integer): Boolean; stdcall;
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

   // Group properties
  TGroupProperty = (gpID, gpText, gpDescription, gpPicCount, gpGroupCount);
  TGroupProperties = set of TGroupProperty;

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
    function  GetProps(GroupProp: TGroupProperty): String; stdcall;
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
     // -- Group properties by index
    property Props[GroupProp: TGroupProperty]: String read GetProps;
     // -- Returns the ultimate owner of the group (all the groups)
    property Root: IPhoaPicGroup read GetRoot;
     // -- Group text (name)
    property Text: String read GetText;
  end;

   //===================================================================================================================
   // Picture group list
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

   //===================================================================================================================
   // Picture sorting
   //===================================================================================================================

  TPhoaSortDirection = (psdAsc, psdDesc);

  IPhoaPicSorting = interface(IInterface)
    ['{9ECF6FE6-0E9A-40B4-8F01-E3C8A01688EC}']
     // Prop handlers
    function  GetProp: TPicProperty; stdcall;
    function  GetDirection: TPhoaSortDirection; stdcall;
     // Props
     // -- Sort property
    property Prop: TPicProperty read GetProp;
     // -- Sort direction
    property Direction: TPhoaSortDirection read GetDirection;
  end;

   //===================================================================================================================
   // Picture sorting list
   //===================================================================================================================

  IPhoaPicSortingList = interface(IInterface)
    ['{9ECF6FE6-0E9A-40B4-8F01-E3C8A01688ED}']
     // Returns index of a sorting by property, or -1 if no such item found
    function  IndexOfProp(Prop: TPicProperty): Integer; stdcall;
     // Sort compare routine
    function  SortComparePics(Pic1, Pic2: IPhoaPic): Integer; stdcall;
     // Returns True if list content is identical with Sortings content
    function  IdenticalWith(Sortings: IPhoaPicSortingList): Boolean; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaPicSorting; stdcall;
     // Props
     // -- Item count
    property Count: Integer read GetCount;
     // -- Sortings by index
    property Items[Index: Integer]: IPhoaPicSorting read GetItems; default;
  end;

   //===================================================================================================================
   // Picture grouping
   //===================================================================================================================

   // PhoA view group-by picture properties
  TPicGroupByProperty = (
    gbpFilePath,
    gbpDateByYear,
    gbpDateByMonth,
    gbpDateByDay,
    gbpTimeHour,
    gbpTimeMinute,
    gbpPlace,
    gbpFilmNumber,
    gbpAuthor,
    gbpMedia,
    gbpKeywords);
  TPicGroupByProperties = set of TPicGroupByProperty;

  IPhoaPicGrouping = interface(IInterface)
    ['{C75C3934-8BC4-4172-9B48-47E55DEF0F46}']
     // Prop handlers
    function  GetProp: TPicGroupByProperty; stdcall;
    function  GetUnclassifiedInOwnFolder: Boolean; stdcall;
     // Props
     // -- Group-by property
    property Prop: TPicGroupByProperty read GetProp;
     // -- If True, the unclassified pictures are to be placed in separate folder
    property UnclassifiedInOwnFolder: Boolean read GetUnclassifiedInOwnFolder;
  end;

   //===================================================================================================================
   // Picture grouping list
   //===================================================================================================================

  IPhoaPicGroupingList = interface(IInterface)
    ['{C75C3934-8BC4-4172-9B48-47E55DEF0F47}']
     // Returns index of a grouping by property, or -1 if no such item found
    function  IndexOfProp(Prop: TPicGroupByProperty): Integer; stdcall;
     // Sort compare routine
    function  SortComparePics(Pic1, Pic2: IPhoaPic): Integer; stdcall;
     // Returns True if list content is identical with Groupings content
    function  IdenticalWith(Groupings: IPhoaPicGroupingList): Boolean; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaPicGrouping; stdcall;
     // Props
     // -- Item count
    property Count: Integer read GetCount;
     // -- Groupings by index
    property Items[Index: Integer]: IPhoaPicGrouping read GetItems; default;
  end;

   //===================================================================================================================
   // PhoA view
   //===================================================================================================================

  IPhoaViewList = interface;

  IPhoaView = interface(IInterface)
    ['{5ADA8486-1E50-44BA-A9F7-2AB729234A21}']
     // Invalidates the built group hierarchy
    procedure Invalidate; stdcall;
     // Prop handlers
    function  GetGroupings: IPhoaPicGroupingList; stdcall;
    function  GetIndex: Integer; stdcall;
    function  GetList: IPhoaViewList; stdcall;
    function  GetName: String; stdcall;
    function  GetRootGroup: IPhoaPicGroup; stdcall;
    function  GetSortings: IPhoaPicSortingList; stdcall;
     // Props
     // -- Picture grouping list
    property Groupings: IPhoaPicGroupingList read GetGroupings;
     // -- View index in its Owner's list
    property Index: Integer read GetIndex;
     // -- Owner list 
    property List: IPhoaViewList read GetList;
     // -- View name
    property Name: String read GetName;
     // -- View root group. The group hierarchy is created automatically when the property is requested
    property RootGroup: IPhoaPicGroup read GetRootGroup;
     // -- Picture sorting list
    property Sortings: IPhoaPicSortingList read GetSortings;
  end;

   //===================================================================================================================
   // PhoA view list, always sorted by view name
   //===================================================================================================================

  IPhoaViewList = interface(IInterface)
    ['{5ADA8486-1E50-44BA-A9F7-2AB729234A22}']
     // Returns index of a view by its name (case-insensitive search); -1 if no such view
    function  IndexOfName(const sName: String): Integer; stdcall;
     // Invalidates all the views
    procedure Invalidate; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaView; stdcall;
    function  GetPics: IPhoaPicList; stdcall;
     // Searches a view by name (case-insensitively) and returns True if found, and its position in Index. Otherwise
     //   returns False, and position of the nearest greater name in Index
    function  FindName(const sName: String; var Index: Integer): Boolean; stdcall;
     // Props
     // -- Item count
    property Count: Integer read GetCount;
     // -- Views by index
    property Items[Index: Integer]: IPhoaView read GetItems; default;
     // -- List of pictures the views based on
    property Pics: IPhoaPicList read GetPics;
  end;

   //===================================================================================================================
   // PhoA photo album project
   //===================================================================================================================

  IPhoaProject = interface(IInterface)
    ['{769DBE0B-D86B-4F89-A557-9A8DA083E506}']
     // Prop handlers
    function  GetCurrentView: IPhoaView; stdcall;
    function  GetDescription: String; stdcall;
    function  GetFileName: String; stdcall;
    function  GetFileRevision: Integer; stdcall;
    function  GetPics: IPhoaPicList; stdcall;
    function  GetRootGroup: IPhoaPicGroup; stdcall;
    function  GetThumbnailSize: TSize; stdcall;
    function  GetThumbnailQuality: Byte; stdcall;
    function  GetViewIndex: Integer; stdcall;
    function  GetViewRootGroup: IPhoaPicGroup; stdcall;
    function  GetViews: IPhoaViewList; stdcall;
     // Props
     // -- Current view, if ViewIndex>=0; nil otherwise
    property CurrentView: IPhoaView read GetCurrentView;
     // -- Photo album description
    property Description: String read GetDescription;
     // -- Project file name
    property FileName: String read GetFileName;
     // -- Current file revision
    property FileRevision: Integer read GetFileRevision;
     // -- Photo album picture list
    property Pics: IPhoaPicList read GetPics;
     // -- Root group of group hierarchy
    property RootGroup: IPhoaPicGroup read GetRootGroup;
     // -- Maximum thumbnail dimensions
    property ThumbnailSize: TSize read GetThumbnailSize;
     // -- JPEG-thumbnail quality (0..100)
    property ThumbnailQuality: Byte read GetThumbnailQuality;
     // -- Current active view index in range -1..Views.Count-1 (-1 means no view is active but picture groups)
    property ViewIndex: Integer read GetViewIndex;
     // -- Current root group considering ViewIndex value (when ViewIndex=-1 this is the same as RootGroup)
    property ViewRootGroup: IPhoaPicGroup read GetViewRootGroup;
     // -- Photo album views
    property Views: IPhoaViewList read GetViews;
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
