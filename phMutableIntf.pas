//**********************************************************************************************************************
//  $Id: phMutableIntf.pas,v 1.6 2004-10-11 11:41:24 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phMutableIntf;

interface

uses Windows, phIntf;

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
    procedure ReloadPicFileData; stdcall;
     // Prop handlers
    function  GetKeywordsM: IPhoaMutableKeywordList; stdcall;
    procedure SetDate(Value: Integer); stdcall;
    procedure SetFileName(const Value: String); stdcall;
    procedure SetFlips(Value: TPicFlips); stdcall;
    procedure SetRotation(Value: TPicRotation); stdcall;
    procedure SetTime(Value: Integer); stdcall;
     // Props
     // -- Date
    property Date: Integer read GetDate write SetDate;
     // -- Picture file name
    property FileName: String read GetFileName write SetFileName;
     // -- Picture flip flags
    property Flips: TPicFlips read GetFlips write SetFlips;
     // -- Mutable keyword list
    property KeywordsM: IPhoaMutableKeywordList read GetKeywordsM;
     // -- Picture rotation
    property Rotation: TPicRotation read GetRotation write SetRotation;
     // -- Time
    property Time: Integer read GetTime write SetTime;
  end;

   //===================================================================================================================
   // An alterable list of picture keywords
   //===================================================================================================================

  IPhoaMutableKeywordList = interface(IPhoaKeywordList)
    ['{C000BFAE-CE13-4DF3-A774-8A21E1727D54}']
     // Creates an exact copy of list contents
    procedure Assign(Source: IPhoaKeywordList); stdcall;
     // Adds an item; returns the index of the newly-added item
    function  Add(const sKeyword: String): Integer; stdcall;
     // Deletes an item
    procedure Delete(Index: Integer); stdcall;
     // Removes a keyword; returns the index of the removed item, or -1 if no such item found
    function  Remove(const sKeyword: String): Integer; stdcall;
     // Clears the list
    procedure Clear; stdcall;
     // Prop handlers
    procedure SetCommaText(const Value: String); stdcall;
     // Props
     // -- Writable CommaText property
    property CommaText: String read GetCommaText write SetCommaText;
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
     // Creates an exact copy of list contents
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
     // Searches a picture by ID and returns True if found, and its position in Index. Otherwise returns False, and
     //   position of the nearest greater ID in Index
    function  FindID(iID: Integer; var Index: Integer): Boolean; stdcall;
     // Clears the list
    procedure Clear; stdcall;
     // Deletes the picture by Index
    procedure Delete(Index: Integer); stdcall;
     // Deletes the picture by its ID. Returns the index of item just deleted, or -1 if no such ID found in the list
    function  Remove(iID: Integer): Integer; stdcall;
     // Sorts the list using CompareFunc. dwData is an arbitrary value passed to CompareFunc. Only allowed when
     //   Sorted=False
    procedure CustomSort(CompareFunc: TPhoaPicListSortCompareFunc; dwData: Cardinal); stdcall;
     // Moves an item into new position. Only allowed when Sorted=False
    procedure Move(iCurIndex, iNewIndex: Integer); stdcall;
     // Prop handlers
    function  GetItemsByIDM(iID: Integer): IPhoaMutablePic; stdcall;
    function  GetItemsByFileNameM(const sFileName: String): IPhoaMutablePic; stdcall;
    function  GetItemsM(Index: Integer): IPhoaMutablePic; stdcall;
    function  GetSorted: Boolean; stdcall;
     // Props
     // -- 'Mutable' version of ItemsByID[]
    property ItemsByIDM[iID: Integer]: IPhoaMutablePic read GetItemsByIDM;
     // -- 'Mutable' version of ItemsByFileName[]
    property ItemsByFileNameM[const sFileName: String]: IPhoaMutablePic read GetItemsByFileNameM;
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
     // Copies the group properties: Text, Expanded;
     //   when bCopyIDs=True       - also IDs
     //   when bCopyPics=True      - also picture list
     //   when bCopySubgroups=True - recurses owned groups
    procedure Assign(Source: IPhoaPicGroup; bCopyIDs, bCopyPics, bCopySubgroups: Boolean); stdcall;
     // Prop handlers
    function  GetGroupsM: IPhoaMutablePicGroupList; stdcall;
    function  GetGroupByIDM(iID: Integer): IPhoaMutablePicGroup; stdcall;
    function  GetGroupByPathM(const sPath: String): IPhoaMutablePicGroup; stdcall;
    function  GetOwnerM: IPhoaMutablePicGroup; stdcall;
    function  GetPicsM: IPhoaMutablePicList; stdcall;
    function  GetRootM: IPhoaMutablePicGroup; stdcall;
    procedure SetDescription(const Value: String); stdcall;
    procedure SetExpanded(Value: Boolean); stdcall;
    procedure SetIndex(Value: Integer); stdcall;
    procedure SetOwner(Value: IPhoaPicGroup); stdcall;
    procedure SetText(const Value: String); stdcall;
     // Props
     // -- Group description
    property Description: String read GetDescription write SetDescription;
     // -- True if a group node is expanded
    property Expanded: Boolean read GetExpanded write SetExpanded;
     // -- 'Mutable' version of Groups
    property GroupsM: IPhoaMutablePicGroupList read GetGroupsM;
     // -- 'Mutable' version of GroupByID[]
    property GroupByIDM[iID: Integer]: IPhoaMutablePicGroup read GetGroupByIDM;
     // -- 'Mutable' version of GroupByPath[]
    property GroupByPathM[const sPath: String]: IPhoaMutablePicGroup read GetGroupByPathM;
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
    property Text: String read GetText write SetText;
  end;

   //===================================================================================================================
   // An alterable picture group list
   //===================================================================================================================

  IPhoaMutablePicGroupList = interface(IPhoaPicGroupList)
    ['{5B299022-5911-4154-8307-37170FDD7951}']
    // Creates an exact copy of list contents
    procedure Assign(Source: IPhoaMutablePicGroupList); stdcall;
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

implementation

end.
