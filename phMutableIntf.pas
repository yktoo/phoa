//**********************************************************************************************************************
//  $Id: phMutableIntf.pas,v 1.1 2004-10-04 12:44:36 dale Exp $
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

  IPhoaMutablePic = interface(IInterface)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68F0}']

  end;

   //-------------------------------------------------------------------------------------------------------------------
   // An alterable list of images
   //-------------------------------------------------------------------------------------------------------------------

  IPhoaMutablePicList = interface(IPhoaPicList)
    ['{7AC3D444-5A48-4396-95C9-82AACDCF68F1}']
     // Add a picture and return its index. bSkipDuplicates controls whether duplicates should be checked and ignored
     //   (a sorted list always checks and ignores duplicates). Version with out bAdded also returns True if the picture
     //   was actually added, or False if it was just a duplicate
    function  Add(Pic: IPhoaPic; bSkipDuplicates: Boolean): Integer; stdcall; overload;
    function  Add(Pic: IPhoaPic; bSkipDuplicates: Boolean; out bAdded: Boolean): Integer; stdcall; overload;
     // Searches a picture by ID and returns True if found, and its position in Index. Otherwise returns False, and
     //   position of the nearest greater ID in Index
    function  FindID(iID: Integer; var Index: Integer): Boolean; stdcall;
     // Clear the list
    procedure Clear; stdcall;
     // Deletes the picture by Index
    procedure Delete(Index: Integer); stdcall;
     // Deletes the picture by its ID. Returns the index of item just deleted, or -1 if no such ID found in the list
    function  Remove(iID: Integer): Integer; stdcall;
     // Prop handlers
    function  GetSorted: Boolean; stdcall;
     // Props
     // -- True if the list is sorted by picture ID and allows no ID duplicates
    property Sorted: Boolean read GetSorted;
  end;

implementation

end.
