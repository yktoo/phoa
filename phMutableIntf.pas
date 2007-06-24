//**********************************************************************************************************************
//  $Id: phMutableIntf.pas,v 1.18 2007-06-24 17:47:59 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phMutableIntf;

interface

uses Windows, phIntf, phAppIntf;

type
  IPhoaMutableKeywordList = interface;

   //===================================================================================================================
   // An alterable PhoA picture interface
   //===================================================================================================================

  IPhoaMutablePic = interface(IPhoaPic)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68F0}']
     // Creates an exact copy of picture properties
    procedure Assign(SrcPic: IPhoaPic); stdcall;
     // Rebuilds thumbnail and updates thumbnail, image and file parameters
    procedure ReloadPicFileData(const AThumbnailSize: TSize; StretchFilter: TPhoaStretchFilter; AThumbnailQuality: Byte); stdcall;
     // Reverts props from Props into their defaults
    procedure CleanupProps(Props: TPicProperties); stdcall;
     // Prop handlers
    function  GetKeywordsM: IPhoaMutableKeywordList; stdcall;
    procedure SetDate(Value: Integer); stdcall;
    procedure SetFileName(const Value: WideString); stdcall;
    procedure SetFlips(Value: TPicFlips); stdcall;
    procedure SetPropValues(PicProp: TPicProperty; const Value: Variant); stdcall;
    procedure SetRawData(PProps: TPicProperties; const Value: TPhoaRawData); stdcall;
    procedure SetRotation(Value: TPicRotation); stdcall;
    procedure SetTime(Value: Integer); stdcall;
     // Props
     // -- Writable Date
    property Date: Integer read GetDate write SetDate;
     // -- Writable FileName
    property FileName: WideString read GetFileName write SetFileName;
     // -- Writable Flips
    property Flips: TPicFlips read GetFlips write SetFlips;
     // -- 'Mutable' version of Keywords
    property KeywordsM: IPhoaMutableKeywordList read GetKeywordsM;
     // -- Writable PropValues
    property PropValues[PicProp: TPicProperty]: Variant read GetPropValues write SetPropValues;
     // -- Writable RawData[]
    property RawData[PProps: TPicProperties]: TPhoaRawData read GetRawData write SetRawData;
     // -- Writable Rotation
    property Rotation: TPicRotation read GetRotation write SetRotation;
     // -- Writable Time
    property Time: Integer read GetTime write SetTime;
  end;

   //===================================================================================================================
   // An alterable list of picture keywords
   //===================================================================================================================

  IPhoaMutableKeywordList = interface(IPhoaKeywordList)
    ['{C000BFAE-CE13-4DF3-A774-8A21E1727D54}']
     // Creates an exact copy of list content
    procedure Assign(Source: IPhoaKeywordList); stdcall;
     // Adds an item; returns the index of the newly-added item
    function  Add(const wsKeyword: WideString): Integer; stdcall;
     // Deletes an item
    procedure Delete(Index: Integer); stdcall;
     // Removes a keyword; returns the index of the removed item, or -1 if no such item found
    function  Remove(const wsKeyword: WideString): Integer; stdcall;
     // Clears the list
    procedure Clear; stdcall;
     // Replaces a keyword for another one; checks that the new keyword is not in list yet, raises an exception
     //   otherwise. After replace moves the word into new position according to sort order, and return the new index of
     //   the keyword
    function  Rename(Index: Integer; const wsNewKeyword: WideString): Integer; stdcall;
     // Prop handlers
    procedure SetCommaText(const Value: WideString); stdcall;
     // Props
     // -- Writable CommaText property
    property CommaText: WideString read GetCommaText write SetCommaText;
  end;

   //===================================================================================================================
   // An alterable list of pictures
   //===================================================================================================================

   // Callback function for comparing the pictures. Must return:
   //   a positive value if Pic1 < Pic2
   //   0                if Pic1 = Pic2
   //   a negative value if Pic1 > Pic2
  TPhoaPicListSortCompareFunc = function(Pic1, Pic2: IPhoaPic; dwData: Cardinal): Integer; stdcall;

  IPhoaMutablePicList = interface(IPhoaPicList)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68F1}']
     // Creates an exact copy of list content
    procedure Assign(Source: IPhoaPicList); stdcall;
     // Add a picture and return its index. bSkipDuplicates controls whether duplicates should be checked and ignored
     //   (a sorted list always checks and ignores duplicates). Version with out bAdded also returns True if the picture
     //   was actually added, or False if it was just a duplicate
    function  Add(Pic: IPhoaPic; bSkipDuplicates: Boolean): Integer; overload; stdcall;
    function  Add(Pic: IPhoaPic; bSkipDuplicates: Boolean; out bAdded: Boolean): Integer; overload; stdcall;
     // Adds all pictures from PicList. Returns number of pictures actually added
    function  Add(PicList: IPhoaPicList; bSkipDuplicates: Boolean): Integer; overload; stdcall;
     // Inserts a picture at given index. Returns True if Pic was actually inserted. Only allowed when Sorted=False
    function  Insert(Index: Integer; Pic: IPhoaPic; bSkipDuplicates: Boolean): Boolean; stdcall;
     // Clears the list
    procedure Clear; stdcall;
     // Deletes the picture by Index
    procedure Delete(Index: Integer); stdcall;
     // Deletes the picture by its ID. Returns the index of item just deleted, or -1 if no such ID found in the list
    function  Remove(iID: Integer): Integer; stdcall;
     // Sorts the list using CompareFunc. dwData is an arbitrary value passed to CompareFunc. Only allowed when
     //   Sorted=False
    procedure CustomSort(CompareFunc: TPhoaPicListSortCompareFunc; dwData: Cardinal); stdcall;
     // Sorts the list using Sortings. Only allowed when Sorted=False
    procedure SortingsSort(Sortings: IPhoaPicSortingList); stdcall;
     // Moves an item into new position. Only allowed when Sorted=False
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
     // Prop handlers
    function  GetItemsByFileNameM(const wsFileName: WideString): IPhoaMutablePic; stdcall;
    function  GetItemsByIDM(iID: Integer): IPhoaMutablePic; stdcall;
    function  GetItemsM(Index: Integer): IPhoaMutablePic; stdcall;
    function  GetSorted: Boolean; stdcall;
     // Props
     // -- 'Mutable' version of ItemsByID[]
    property ItemsByIDM[iID: Integer]: IPhoaMutablePic read GetItemsByIDM;
     // -- 'Mutable' version of ItemsByFileName[]
    property ItemsByFileNameM[const wsFileName: WideString]: IPhoaMutablePic read GetItemsByFileNameM;
     // -- 'Mutable' version of Items[]
    property ItemsM[Index: Integer]: IPhoaMutablePic read GetItemsM; default;
     // -- True if the list is sorted by picture ID and allows no ID duplicates or resorting
    property Sorted: Boolean read GetSorted;
  end;

   //===================================================================================================================
   // An alterable picture group
   //===================================================================================================================

  IPhoaMutablePicGroupList = interface;

  IPhoaMutablePicGroup = interface(IPhoaPicGroup)
    ['{9C951B51-2C66-4C35-B61B-8EDCBEAD8ABF}']
     // Copies the group properties: Text, Description, Expanded, Icon;
     //   when bCopyIDs=True       - also IDs
     //   when bCopyPics=True      - also picture list
     //   when bCopySubgroups=True - recurses owned groups
    procedure Assign(Source: IPhoaPicGroup; bCopyIDs, bCopyPics, bCopySubgroups: Boolean); stdcall;
     // Prop handlers
    function  GetGroupsM: IPhoaMutablePicGroupList; stdcall;
    function  GetGroupByIDM(iID: Integer): IPhoaMutablePicGroup; stdcall;
    function  GetGroupByPathM(const wsPath: WideString): IPhoaMutablePicGroup; stdcall;
    function  GetOwnerM: IPhoaMutablePicGroup; stdcall;
    function  GetPicsM: IPhoaMutablePicList; stdcall;
    function  GetRootM: IPhoaMutablePicGroup; stdcall;
    procedure SetDescription(const Value: WideString); stdcall;
    procedure SetExpanded(Value: Boolean); stdcall;
    procedure SetIconData(const Value: TPhoaRawData); stdcall;
    procedure SetIndex(Value: Integer); stdcall;
    procedure SetOwner(Value: IPhoaPicGroup); stdcall;
    procedure SetText(const Value: WideString); stdcall;
     // Props
     // -- Group description
    property Description: WideString read GetDescription write SetDescription;
     // -- True if a group node is expanded
    property Expanded: Boolean read GetExpanded write SetExpanded;
     // -- 'Mutable' version of Groups
    property GroupsM: IPhoaMutablePicGroupList read GetGroupsM;
     // -- 'Mutable' version of GroupByID[]
    property GroupByIDM[iID: Integer]: IPhoaMutablePicGroup read GetGroupByIDM;
     // -- 'Mutable' version of GroupByPath[]
    property GroupByPathM[const wsPath: WideString]: IPhoaMutablePicGroup read GetGroupByPathM;
     // -- 'Mutable' version of IconData
    property IconData: TPhoaRawData read GetIconData write SetIconData;
     // -- Group index in its Owner's list
    property Index: Integer read GetIndex write SetIndex;
     // -- Group owner
    property Owner: IPhoaPicGroup read GetOwner write SetOwner;
     // -- 'Mutable' version of Owner
    property OwnerM: IPhoaMutablePicGroup read GetOwnerM;
     // -- 'Mutable' version of Pics
    property PicsM: IPhoaMutablePicList read GetPicsM;
     // -- 'Mutable' version of Root
    property RootM: IPhoaMutablePicGroup read GetRootM;
     // -- Group text (name)
    property Text: WideString read GetText write SetText;
  end;

   //===================================================================================================================
   // An alterable picture group list
   //===================================================================================================================

  IPhoaMutablePicGroupList = interface(IPhoaPicGroupList)
    ['{5B299022-5911-4154-8307-37170FDD7951}']
     // Creates an exact copy of list content
    procedure Assign(Source: IPhoaPicGroupList); stdcall;
     // Adds a group to the list; returns the index of the newly-added group
    function  Add(Group: IPhoaPicGroup): Integer; stdcall;
     // Inserts group at specified position
    procedure Insert(Index: Integer; Group: IPhoaPicGroup); stdcall;
     // Deletes a group
    procedure Delete(Index: Integer); stdcall;
     // Deletes the group by its ID. Returns the index of item just deleted, or -1 if no such ID found in the list
    function  Remove(iID: Integer): Integer; stdcall;
     // Clears the list
    procedure Clear; stdcall;
     // Move the group into new position
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
     // Prop handlers
    function  GetItemsM(Index: Integer): IPhoaMutablePicGroup; stdcall;
    function  GetOwnerM: IPhoaMutablePicGroup; stdcall;
     // Props
     // -- 'Mutable' version of Items[]
    property ItemsM[Index: Integer]: IPhoaMutablePicGroup read GetItemsM; default;
     // -- 'Mutable' version of Owner
    property OwnerM: IPhoaMutablePicGroup read GetOwnerM;
  end;

   //===================================================================================================================
   // An alterable picture sorting
   //===================================================================================================================

  IPhoaMutablePicSorting = interface(IPhoaPicSorting)
    ['{60F9DAF8-903E-4C48-A6D4-93800461D373}']
     // Creates an exact copy of a sorting
    procedure Assign(Source: IPhoaPicSorting); stdcall;
     // Reverses the sort direction
    procedure ToggleDirection; stdcall;
     // Prop handlers
    procedure SetProp(Value: TPicProperty); stdcall;
    procedure SetDirection(Value: TPhoaSortDirection); stdcall;
     // Props
     // -- Writable Prop
    property Prop: TPicProperty read GetProp write SetProp;
     // -- Writable Direction
    property Direction: TPhoaSortDirection read GetDirection write SetDirection;
  end;

   //===================================================================================================================
   // An alterable picture sorting list
   //===================================================================================================================

  IPhoaMutablePicSortingList = interface(IPhoaPicSortingList)
    ['{60F9DAF8-903E-4C48-A6D4-93800461D374}']
     // Creates an exact copy of list content
    procedure Assign(Source: IPhoaPicSortingList); stdcall;
     // Adds an item to the list; returns the index of the newly-added item
    function  Add(Item: IPhoaPicSorting): Integer; stdcall;
     // Inserts an item at specified position
    procedure Insert(Index: Integer; Item: IPhoaPicSorting); stdcall;
     // Deletes an item
    procedure Delete(Index: Integer); stdcall;
     // Clears the list
    procedure Clear; stdcall;
     // Moves an item into new position
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
     // Prop handlers
    function  GetItemsM(Index: Integer): IPhoaMutablePicSorting; stdcall;
     // Props
     // -- 'Mutable' version of Items[]
    property ItemsM[Index: Integer]: IPhoaMutablePicSorting read GetItemsM; default;
  end;

   //===================================================================================================================
   // An alterable picture grouping
   //===================================================================================================================

  IPhoaMutablePicGrouping = interface(IPhoaPicGrouping)
    ['{FB184478-F95A-4050-BF20-5E2E28AED36D}']
     // Creates an exact copy of a grouping
    procedure Assign(Source: IPhoaPicGrouping); stdcall;
     // Toggles UnclassifiedInOwnFolder value
    procedure ToggleUnclassifiedInOwnFolder; stdcall;
     // Prop handlers
    procedure SetProp(Value: TPicGroupByProperty); stdcall;
    procedure SetUnclassifiedInOwnFolder(Value: Boolean); stdcall;
     // Props
     // -- Writable Prop
    property Prop: TPicGroupByProperty read GetProp write SetProp;
     // -- Writable UnclassifiedInOwnFolder
    property UnclassifiedInOwnFolder: Boolean read GetUnclassifiedInOwnFolder write SetUnclassifiedInOwnFolder;
  end;

   //===================================================================================================================
   // An alterable picture grouping list
   //===================================================================================================================

  IPhoaMutablePicGroupingList = interface(IPhoaPicGroupingList)
    ['{FB184478-F95A-4050-BF20-5E2E28AED36E}']
     // Creates an exact copy of list content
    procedure Assign(Source: IPhoaPicGroupingList); stdcall;
     // Adds an item to the list; returns the index of the newly-added item
    function  Add(Item: IPhoaPicGrouping): Integer; stdcall;
     // Inserts an item at specified position
    procedure Insert(Index: Integer; Item: IPhoaPicGrouping); stdcall;
     // Deletes an item
    procedure Delete(Index: Integer); stdcall;
     // Clears the list
    procedure Clear; stdcall;
     // Moves an item into new position
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
     // Prop handlers
    function  GetItemsM(Index: Integer): IPhoaMutablePicGrouping; stdcall;
     // Props
     // -- 'Mutable' version of Items[]
    property ItemsM[Index: Integer]: IPhoaMutablePicGrouping read GetItemsM; default;
  end;

   //===================================================================================================================
   // An alterable PhoA view
   //===================================================================================================================

  IPhoaMutableViewList = interface;

  IPhoaMutableView = interface(IPhoaView)
    ['{CAD22216-6D18-45D4-8483-F2D53861A42F}']
     // Creates an exact copy of a view
    procedure Assign(Source: IPhoaView); stdcall;
     // Prop handlers
    function  GetGroupingsM: IPhoaMutablePicGroupingList; stdcall;
    function  GetListM: IPhoaMutableViewList; stdcall;
    function  GetSortingsM: IPhoaMutablePicSortingList; stdcall;
    procedure SetFilterExpression(const Value: WideString); stdcall;
    procedure SetName(const Value: WideString); stdcall;
     // Props
     // -- Writable FilterExpression
    property FilterExpression: WideString read GetFilterExpression write SetFilterExpression;
     // -- 'Mutable' version of Groupings
    property GroupingsM: IPhoaMutablePicGroupingList read GetGroupingsM;
     // -- 'Mutable' version of List
    property ListM: IPhoaMutableViewList read GetListM;
     // -- Writable Name
    property Name: WideString read GetName write SetName;
     // -- 'Mutable' version of Sortings
    property SortingsM: IPhoaMutablePicSortingList read GetSortingsM;
  end;

   //===================================================================================================================
   // An alterable PhoA view list
   //===================================================================================================================

  IPhoaMutableViewList = interface(IPhoaViewList)
    ['{CAD22216-6D18-45D4-8483-F2D53861A430}']
     // Creates an exact copy of list content
    procedure Assign(Source: IPhoaViewList); stdcall;
     // Adds an item to the list; returns the index of the newly-added item
    function  Add(Item: IPhoaView): Integer; stdcall;
     // Deletes an item
    procedure Delete(Index: Integer); stdcall;
     // Clears the list
    procedure Clear; stdcall;
     // Prop handlers
    function  GetItemsM(Index: Integer): IPhoaMutableView; stdcall;
    function  GetPicsM: IPhoaMutablePicList; stdcall;
     // Props
     // -- 'Mutable' version of Items[]
    property ItemsM[Index: Integer]: IPhoaMutableView read GetItemsM; default;
     // -- 'Mutable' version of Pics
    property PicsM: IPhoaMutablePicList read GetPicsM;
  end;

   //===================================================================================================================
   // An alterable PhoA photo album project
   //===================================================================================================================

  IPhoaMutableProject = interface(IPhoaProject)
    ['{769DBE0B-D86B-4F89-A557-9A8DA083E507}']
     // Creates an exact copy of source project
    procedure Assign(Source: IPhoaProject; bCopyRevision: Boolean); stdcall;
     // Clears and initializes the project
    procedure New; stdcall;
     // File loading and saving
    procedure LoadFromFile(const wsFileName: WideString); stdcall;
    procedure SaveToFile(const wsFileName, wsGenerator, wsRemark: WideString; iRevisionNumber: Integer); stdcall;
     // Prop handlers
    function  GetCurrentViewM: IPhoaMutableView; stdcall;
    function  GetPicsM: IPhoaMutablePicList; stdcall;
    function  GetRootGroupM: IPhoaMutablePicGroup; stdcall;
    function  GetViewRootGroupM: IPhoaMutablePicGroup; stdcall;
    function  GetViewsM: IPhoaMutableViewList; stdcall;
    procedure SetDescription(const Value: WideString); stdcall;
    procedure SetFileName(const Value: WideString); stdcall;
    procedure SetThumbnailQuality(Value: Byte); stdcall;
    procedure SetThumbnailSize(const Value: TSize); stdcall;
    procedure SetViewIndex(Value: Integer); stdcall;
     // Props
     // -- 'Mutable' version of CurrentView
    property CurrentViewM: IPhoaMutableView read GetCurrentViewM;
     // -- Writable Description
    property Description: WideString read GetDescription write SetDescription;
     // -- Writable FileName
    property FileName: WideString read GetFileName write SetFileName;
     // -- 'Mutable' version of Pics
    property PicsM: IPhoaMutablePicList read GetPicsM;
     // -- 'Mutable' version of RootGroup
    property RootGroupM: IPhoaMutablePicGroup read GetRootGroupM;
     // -- Writable ThumbnailSize
    property ThumbnailSize: TSize read GetThumbnailSize write SetThumbnailSize;
     // -- Writable ThumbnailQuality
    property ThumbnailQuality: Byte read GetThumbnailQuality write SetThumbnailQuality;
     // -- 'Mutable' version of ViewIndex
    property ViewIndex: Integer read GetViewIndex write SetViewIndex;
     // -- 'Mutable' version of ViewRootGroup
    property ViewRootGroupM: IPhoaMutablePicGroup read GetViewRootGroupM;
     // -- 'Mutable' version of Views
    property ViewsM: IPhoaMutableViewList read GetViewsM;
  end;

   //===================================================================================================================
   // An alterable PhoA application
   //===================================================================================================================

  IPhoaMutableApp = interface(IPhoaApp)
    ['{21629CF9-B22A-44B3-8DD7-FB9A35D4C004}']
     // Prop handlers
    function  GetCurGroupM: IPhoaMutablePicGroup;
    function  GetProjectM: IPhoaMutableProject;
    function  GetSelectedPicsM: IPhoaMutablePicList;
    function  GetViewedPicsM: IPhoaMutablePicList;
    procedure SetCurGroupM(Value: IPhoaMutablePicGroup);
     // Props
     // -- 'Mutable' version of CurGroup
    property CurGroupM: IPhoaMutablePicGroup read GetCurGroupM write SetCurGroupM;
     // -- 'Mutable' version of Project
    property ProjectM: IPhoaMutableProject read GetProjectM;
     // -- 'Mutable' version of SelectedPics
    property SelectedPicsM: IPhoaMutablePicList read GetSelectedPicsM;
     // -- 'Mutable' version of ViewedPics
    property ViewedPicsM: IPhoaMutablePicList read GetViewedPicsM;
  end;

implementation

end.
