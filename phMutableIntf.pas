//**********************************************************************************************************************
//  $Id: phMutableIntf.pas,v 1.3 2004-10-06 14:41:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phMutableIntf;

interface

uses Windows, phIntf;

type

   //-------------------------------------------------------------------------------------------------------------------
   // An alterable PhoA picture interface
   //-------------------------------------------------------------------------------------------------------------------

  IPhoaMutablePic = interface(IPhoaPic)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68F0}']
     // Rebuilds thumbnail and updates thumbnail, image and file parameters
    procedure ReloadPicFileData; stdcall;
     // Prop handlers
    procedure SetFileName(Value: PAnsiChar); stdcall;
    procedure SetFlips(Value: TPicFlips); stdcall;
    procedure SetRotation(Value: TPicRotation); stdcall;
     // Props
     // -- Picture file name
    property FileName: PAnsiChar read GetFileName write SetFileName;
     // -- Picture flip flags
    property Flips: TPicFlips read GetFlips write SetFlips;
     // -- Picture rotation
    property Rotation: TPicRotation read GetRotation write SetRotation;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // An alterable list of images
   //-------------------------------------------------------------------------------------------------------------------

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
     // Inserts a picture at given index. Returns True when Pic was actually inserted. Only allowed when Sorted=False
    function  Insert(Index: Integer; Pic: IPhoaPic; bSkipDuplicates: Boolean): Boolean; stdcall;
     // Searches a picture by ID and returns True if found, and its position in Index. Otherwise returns False, and
     //   position of the nearest greater ID in Index
    function  FindID(iID: Integer; var Index: Integer): Boolean; stdcall;
     // Clear the list
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
    function  GetSorted: Boolean; stdcall;
     // Props
     // -- True if the list is sorted by picture ID and allows no ID duplicates or resorting
    property Sorted: Boolean read GetSorted;
  end;

implementation

end.
