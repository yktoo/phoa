//**********************************************************************************************************************
//  $Id: phIntf.pas,v 1.29 2007-06-30 10:36:20 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
// This unit contains fundamental PhoA entity interface declarations and API, such as picture, picture group, view and
// project. Those interfaces only allow read access to PhoA objects.

unit phIntf;

interface

uses Windows, SysUtils;

type
   // Base Unicode-enabled Exception class
  EPhoaWideException = class(Exception)
  private
     // Prop storage
    FWideMessage: WideString;
  public
    constructor Create(const wsMessage: WideString);
    constructor CreateFmt(const wsMessage: WideString; const Args: Array of const);
     // Props
     // -- Unicode message
    property WideMessage: WideString read FWideMessage;
  end;

  EPhoaWideExceptionClass = class of EPhoaWideException;

   // Exception class
  EPhIntfException = class(EPhoaWideException);

   // Raw binary data of an arbitrary size (up to 2 GB)
  TPhoaRawData = type AnsiString;

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
    ppThumbWidth,
    ppThumbHeight,
    ppThumbDims,
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
   // Picture property data types
  TPicPropDatatype = (
    ppdtString, ppdtInteger, ppdtFloat, ppdtDate, ppdtTime, ppdtBoolean, ppdtList, ppdtSize, ppdtPixelFormat,
    ppdtRotation, ppdtFlips);

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
    function  GetAuthor: WideString; stdcall;
    function  GetDate: Integer; stdcall;
    function  GetTime: Integer; stdcall;
    function  GetDescription: WideString; stdcall;
    function  GetFileName: WideString; stdcall;
    function  GetFileSize: Integer; stdcall;
    function  GetFilmNumber: WideString; stdcall;
    function  GetFlips: TPicFlips; stdcall;
    function  GetFrameNumber: WideString; stdcall;
    function  GetImageFormat: TPhoaPixelFormat; stdcall;
    function  GetImageSize: TSize; stdcall;
    function  GetKeywords: IPhoaKeywordList; stdcall;
    function  GetMedia: WideString; stdcall;
    function  GetNotes: WideString; stdcall;
    function  GetPlace: WideString; stdcall;
    function  GetPropStrValues(PicProp: TPicProperty): WideString; stdcall;
    function  GetPropValues(PicProp: TPicProperty): Variant; stdcall;
    function  GetRawData(PProps: TPicProperties): TPhoaRawData; stdcall;
    function  GetRotation: TPicRotation; stdcall;
    function  GetThumbnailSize: TSize; stdcall;
    function  GetThumbnailData: TPhoaRawData; stdcall;
     // Props
     // -- Unique ID
    property ID: Integer read GetID;
     // -- Author
    property Author: WideString read GetAuthor;
     // -- Picture date: number of days since Jan 01, 0001
    property Date: Integer read GetDate;
     // -- Picture date: number of seconds since midnight
    property Time: Integer read GetTime;
     // -- Description (for display)
    property Description: WideString read GetDescription;
     // -- Full picture file name
    property FileName: WideString read GetFileName;
     // -- Picture file size in bytes
    property FileSize: Integer read GetFileSize;
     // -- Film number or name
    property FilmNumber: WideString read GetFilmNumber;
     // -- Picture flip flags
    property Flips: TPicFlips read GetFlips;
     // -- Frame number
    property FrameNumber: WideString read GetFrameNumber;
     // -- Pixel image format
    property ImageFormat: TPhoaPixelFormat read GetImageFormat;
     // -- Image dimensions in pixels
    property ImageSize: TSize read GetImageSize;
     // -- Keyword list
    property Keywords: IPhoaKeywordList read GetKeywords;
     // -- Picture file media name or code
    property Media: WideString read GetMedia;
     // -- Notes
    property Notes: WideString read GetNotes;
     // -- Place
    property Place: WideString read GetPlace;
     // -- Property values of type WideString, by index
    property PropStrValues[PicProp: TPicProperty]: WideString read GetPropStrValues;
     // -- Property values of type Variant, by index
    property PropValues[PicProp: TPicProperty]: Variant read GetPropValues;
     // -- Picture raw binary data (for PProps properties). No WideString version as it's meaningless
    property RawData[PProps: TPicProperties]: TPhoaRawData read GetRawData;
     // -- Picture rotation
    property Rotation: TPicRotation read GetRotation;
     // -- Thumbnail dimensions in pixels
    property ThumbnailSize: TSize read GetThumbnailSize;
     // -- Thumbnail raw binary JPEG data
    property ThumbnailData: TPhoaRawData read GetThumbnailData;
  end;

   //===================================================================================================================
   // Sorted list of picture keywords
   //===================================================================================================================

  IPhoaKeywordList = interface(IInterface)
    ['{C000BFAE-CE13-4DF3-A774-8A21E1727D53}']
     // Returns index of a keyword, or -1 if no such item found
    function  IndexOf(const wsKeyword: WideString): Integer; stdcall;
     // Prop handlers
    function  GetCommaText: WideString; stdcall;
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): WideString; stdcall;
     // Props
     // -- Comma-separated keyword list; keywords containing commas and/or spaces are double-quoted
    property CommaText: WideString read GetCommaText;
     // -- Number of keywords in the list
    property Count: Integer read GetCount;
     // -- Keywords by index; Index in range 0..Count-1
    property Items[Index: Integer]: WideString read GetItems; default;
  end;

   //===================================================================================================================
   // A list of pictures
   //===================================================================================================================

  IPhoaPicList = interface(IInterface)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68EF}']
     // Returns index of a picture by its ID, or -1 if no such item found
    function  IndexOfID(iID: Integer): Integer; stdcall;
     // Returns index of a picture by its full file name (case-insensitive search), or -1 if no such item found
    function  IndexOfFileName(const wsFileName: WideString): Integer; stdcall;
     // Searches a picture by ID and returns True if found, and its position in Index. Otherwise returns False, and
     //   position of the nearest greater ID in Index
    function  FindID(iID: Integer; var Index: Integer): Boolean; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall;
    function  GetItemsByID(iID: Integer): IPhoaPic; stdcall;
    function  GetItemsByFileName(const wsFileName: WideString): IPhoaPic; stdcall;
    function  GetItems(Index: Integer): IPhoaPic; stdcall;
    function  GetMaxPicID: Integer; stdcall;
     // Props
     // -- Number of pictures in the list
    property Count: Integer read GetCount;
     // -- Pictures by ID; returns nil if no such item found
    property ItemsByID[iID: Integer]: IPhoaPic read GetItemsByID;
     // -- Pictures by full file name (case-insensitive search); returns nil if no such item found
    property ItemsByFileName[const wsFileName: WideString]: IPhoaPic read GetItemsByFileName;
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
    function  GetDescription: WideString; stdcall;
    function  GetExpanded: Boolean; stdcall;
    function  GetMaxGroupID: Integer; stdcall;
    function  GetGroups: IPhoaPicGroupList; stdcall;
    function  GetGroupByID(iID: Integer): IPhoaPicGroup; stdcall;
    function  GetGroupByPath(const wsPath: WideString): IPhoaPicGroup; stdcall;
    function  GetIconData: TPhoaRawData; stdcall;
    function  GetID: Integer; stdcall;
    function  GetIndex: Integer; stdcall;
    function  GetNestedGroupCount: Integer; stdcall;
    function  GetOwner: IPhoaPicGroup; stdcall;
    function  GetPath(const wsRootName: WideString): WideString; stdcall;
    function  GetPics: IPhoaPicList; stdcall;
    function  GetProps(GroupProp: TGroupProperty): WideString; stdcall;
    function  GetRoot: IPhoaPicGroup; stdcall;
    function  GetText: WideString; stdcall;
     // Props
     // -- Group description
    property Description: WideString read GetDescription;
     // -- True if a group node is expanded
    property Expanded: Boolean read GetExpanded;
     // -- Owned group list
    property Groups: IPhoaPicGroupList read GetGroups;
     // -- Finds and returns the group by its ID (recursively); nil if no such group
    property GroupByID[iID: Integer]: IPhoaPicGroup read GetGroupByID;
     // -- Returns the group by its path; nil if no such group. The search is case-insensitive; starts searching from
     //    the first owned group. If wsPath starts with '/', ignores this character
    property GroupByPath[const wsPath: WideString]: IPhoaPicGroup read GetGroupByPath;
     // -- Icon raw binary PNG data
    property IconData: TPhoaRawData read GetIconData;
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
    property Path[const wsRootName: WideString]: WideString read GetPath;
     // -- Pictures in the group
    property Pics: IPhoaPicList read GetPics;
     // -- Group properties by index
    property Props[GroupProp: TGroupProperty]: WideString read GetProps;
     // -- Returns the ultimate owner of the group (all the groups)
    property Root: IPhoaPicGroup read GetRoot;
     // -- Group text (name)
    property Text: WideString read GetText;
  end;

   //===================================================================================================================
   // Picture group list
   //===================================================================================================================

  IPhoaPicGroupList = interface(IInterface)
    ['{5B299022-5911-4154-8307-37170FDD7950}']
     // Returns index of a group; -1 if no such item found
    function  IndexOf(Group: IPhoaPicGroup): Integer; stdcall;
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
    function  GetFilterExpression: WideString; stdcall;
    function  GetGroupings: IPhoaPicGroupingList; stdcall;
    function  GetIndex: Integer; stdcall;
    function  GetList: IPhoaViewList; stdcall;
    function  GetName: WideString; stdcall;
    function  GetRootGroup: IPhoaPicGroup; stdcall;
    function  GetSortings: IPhoaPicSortingList; stdcall;
     // Props
     // -- Picture filter expression
    property FilterExpression: WideString read GetFilterExpression;
     // -- Picture grouping list
    property Groupings: IPhoaPicGroupingList read GetGroupings;
     // -- View index in its Owner's list
    property Index: Integer read GetIndex;
     // -- Owner list
    property List: IPhoaViewList read GetList;
     // -- View name
    property Name: WideString read GetName;
     // -- View root group. The group hierarchy is created automatically when the property value is requested
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
    function  IndexOfName(const wsName: WideString): Integer; stdcall;
     // Invalidates all the views
    procedure Invalidate; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaView; stdcall;
    function  GetPics: IPhoaPicList; stdcall;
     // Searches a view by name (case-insensitively) and returns True if found, and its position in Index. Otherwise
     //   returns False, and position of the nearest greater name in Index
    function  FindName(const wsName: WideString; var Index: Integer): Boolean; stdcall;
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
    function  GetDescription: WideString; stdcall;
    function  GetFileName: WideString; stdcall;
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
    property Description: WideString read GetDescription;
     // -- Project file name
    property FileName: WideString read GetFileName;
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
   // Picture property names. DO NOT LOCALIZE!
  SPicPropName_ID            = 'ID';
  SPicPropName_FileName      = 'FileName';
  SPicPropName_FullFileName  = 'FullFileName';
  SPicPropName_FilePath      = 'FilePath';
  SPicPropName_FileSize      = 'FileSize';
  SPicPropName_FileSizeBytes = 'FileSizeBytes';
  SPicPropName_PicWidth      = 'PicWidth';
  SPicPropName_PicHeight     = 'PicHeight';
  SPicPropName_PicDims       = 'PicDims';
  SPicPropName_ThumbWidth    = 'ThumbWidth';
  SPicPropName_ThumbHeight   = 'ThumbHeight';
  SPicPropName_ThumbDims     = 'ThumbDims';
  SPicPropName_Format        = 'Format';
  SPicPropName_Date          = 'Date';
  SPicPropName_Time          = 'Time';
  SPicPropName_Place         = 'Place';
  SPicPropName_FilmNumber    = 'FilmNumber';
  SPicPropName_FrameNumber   = 'FrameNumber';
  SPicPropName_Author        = 'Author';
  SPicPropName_Description   = 'Description';
  SPicPropName_Notes         = 'Notes';
  SPicPropName_Media         = 'Media';
  SPicPropName_Keywords      = 'Keywords';
  SPicPropName_Rotation      = 'Rotation';
  SPicPropName_Flips         = 'Flips';

  awsPicPropNames: Array[TPicProperty] of WideString = (
    SPicPropName_ID,            // ppID
    SPicPropName_FileName,      // ppFileName
    SPicPropName_FullFileName,  // ppFullFileName
    SPicPropName_FilePath,      // ppFilePath
    SPicPropName_FileSize,      // ppFileSize
    SPicPropName_FileSizeBytes, // ppFileSizeBytes
    SPicPropName_PicWidth,      // ppPicWidth
    SPicPropName_PicHeight,     // ppPicHeight
    SPicPropName_PicDims,       // ppPicDims
    SPicPropName_ThumbWidth,    // ppThumbWidth
    SPicPropName_ThumbHeight,   // ppThumbHeight
    SPicPropName_ThumbDims,     // ppThumbDims
    SPicPropName_Format,        // ppFormat
    SPicPropName_Date,          // ppDate
    SPicPropName_Time,          // ppTime
    SPicPropName_Place,         // ppPlace
    SPicPropName_FilmNumber,    // ppFilmNumber
    SPicPropName_FrameNumber,   // ppFrameNumber
    SPicPropName_Author,        // ppAuthor
    SPicPropName_Description,   // ppDescription
    SPicPropName_Notes,         // ppNotes
    SPicPropName_Media,         // ppMedia
    SPicPropName_Keywords,      // ppKeywords
    SPicPropName_Rotation,      // ppRotation
    SPicPropName_Flips);        // ppFlips

   // Picture property data types
  aPicPropDatatype: Array[TPicProperty] of TPicPropDatatype = (
    ppdtInteger,     // ppID
    ppdtString,      // ppFileName
    ppdtString,      // ppFullFileName
    ppdtString,      // ppFilePath
    ppdtInteger,     // ppFileSize
    ppdtInteger,     // ppFileSizeBytes
    ppdtInteger,     // ppPicWidth
    ppdtInteger,     // ppPicHeight
    ppdtSize,        // ppPicDims
    ppdtInteger,     // ppThumbWidth
    ppdtInteger,     // ppThumbHeight
    ppdtSize,        // ppThumbDims
    ppdtPixelFormat, // ppFormat
    ppdtDate,        // ppDate
    ppdtTime,        // ppTime
    ppdtString,      // ppPlace
    ppdtString,      // ppFilmNumber
    ppdtString,      // ppFrameNumber
    ppdtString,      // ppAuthor
    ppdtString,      // ppDescription
    ppdtString,      // ppNotes
    ppdtString,      // ppMedia
    ppdtList,        // ppKeywords
    ppdtRotation,    // ppRotation
    ppdtFlips);      // ppFlips

   // Translation TPicProperty<->Property name
  function  PicPropToStr(Prop: TPicProperty; bStrict: Boolean): WideString;
  function  StrToPicProp(const wsPropName: WideString; bStrict: Boolean): TPicProperty;

implementation

   //===================================================================================================================
   // EPhoaWideException
   //===================================================================================================================

  constructor EPhoaWideException.Create(const wsMessage: WideString);
  begin
    inherited Create(wsMessage); // Implicit conversion to Ansi is done here
    FWideMessage := wsMessage;
  end;

  constructor EPhoaWideException.CreateFmt(const wsMessage: WideString; const Args: array of const);
  begin
    inherited CreateFmt(wsMessage, Args); // Implicit conversion to Ansi is done here
    FWideMessage := WideFormat(wsMessage, Args);
  end;

   //===================================================================================================================

  function PicPropToStr(Prop: TPicProperty; bStrict: Boolean): WideString;
  begin
    if Prop in [Low(Prop)..High(Prop)] then Result := awsPicPropNames[Prop]
    else if not bStrict then Result := ''
    else raise EPhIntfException.CreateFmt('Invalid picture property value (%d)', [Byte(Prop)]);
  end;

  function StrToPicProp(const wsPropName: WideString; bStrict: Boolean): TPicProperty;
  begin
    for Result := Low(Result) to High(Result) do
      if WideSameText(wsPropName, awsPicPropNames[Result]) then Exit;
    if bStrict then raise EPhIntfException.CreateFmt('Invalid picture property name ("%s")', [wsPropName]);
    Result := TPicProperty(-1);
  end;

end.
