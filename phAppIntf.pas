//**********************************************************************************************************************
//  $Id: phAppIntf.pas,v 1.5 2005-03-01 21:35:40 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
// This unit contains application-related interface declarations and API

unit phAppIntf;

interface
uses phIntf;

type

   //===================================================================================================================
   // An action (the same as command)
   //===================================================================================================================

  IPhoaAction = interface(IInterface)
    ['{03A7EFBC-0C77-481B-A3B9-1D25BB2D458C}']
     // Executes the action. Returns True, if the action succeeded, or False if it was failed due to any reason
    function  Execute: LongBool; stdcall;
     // Prop handlers
    function  GetCaption: String; stdcall;
    function  GetCaptionW: WideString; stdcall;
    function  GetCategory: String; stdcall;
    function  GetCategoryW: WideString; stdcall;
    function  GetEnabled: LongBool; stdcall;
    function  GetHint: String; stdcall;
    function  GetHintW: WideString; stdcall;
    function  GetName: String; stdcall;
    function  GetNameW: WideString; stdcall;
    function  GetTag: Integer; stdcall;
    procedure SetCaption(const Value: String); stdcall;
    procedure SetCaptionW(const Value: WideString); stdcall;
    procedure SetCategory(const Value: String); stdcall;
    procedure SetCategoryW(const Value: WideString); stdcall;
    procedure SetEnabled(Value: LongBool); stdcall;
    procedure SetHint(const Value: String); stdcall;
    procedure SetHintW(const Value: WideString); stdcall;
    procedure SetTag(Value: Integer); stdcall;
     // Props
     // -- Action caption
    property Caption: String read GetCaption write SetCaption;
    property CaptionW: WideString read GetCaptionW write SetCaptionW;
     // -- Action category name. A category name is for grouping similar actions. Example: 'View' or 'Edit'
    property Category: String read GetCategory write SetCategory;
    property CategoryW: WideString read GetCategoryW write SetCategoryW;
     // -- True when action is enabled, otherwise False
    property Enabled: LongBool read GetEnabled write SetEnabled;
     // -- Action hint: a message that is displayed in the status bar and in a tooltip. To have two different
     //    messages displayed, separate tooltip text from status bar text with a pipe ('|'). Example:
     //    'Import...|Invokes the Import dialog'
    property Hint: String read GetHint write SetHint;
    property HintW: WideString read GetHintW write SetHintW;
     // -- Action name: an unique case-insensitive action identifier. The name is assigned to an action when it is
     //    created and cannot be changed afterwards.
     //    Standard PhoA actions all start with small 'a'. It would be good to conform that rule, but add your specific
     //    Id after a. For example: assume your plugin is named 'Door opener', and you write a knock action. So the
     //    action should be named 'aDoorOpener_Knock': your plugin name acts like a prefix.
    property Name: String read GetName;
    property NameW: WideString read GetNameW;
     // -- Custom data associated with the action
    property Tag: Integer read GetTag write SetTag;
  end;

   //===================================================================================================================
   // PhoA action list
   //===================================================================================================================

   // Action execute callback procedure prototype
  TPhoaActionExecuteProc = procedure(Sender: IPhoaAction); stdcall;

  IPhoaActionList = interface(IInterface)
    ['{03A7EFBC-0C77-481B-A3B9-1D25BB2D458D}']
     // Adds and returns a new action interface
    function  Add(const sName: String; AExecuteProc: TPhoaActionExecuteProc): IPhoaAction; stdcall;
    function  AddW(const sName: WideString; AExecuteProc: TPhoaActionExecuteProc): IPhoaAction; stdcall;
     // Removes an action interface and returns its index in the list it had before it was removed
    function  Remove(Item: IPhoaAction): Integer; stdcall;
     // Finds and returns the action by name; returns nil if no such action found
    function  FindName(const sName: String): IPhoaAction; stdcall;
    function  FindNameW(const sName: WideString): IPhoaAction; stdcall;
     // Prop handlers
    function  GetCount: Integer; stdcall;
    function  GetItems(Index: Integer): IPhoaAction; stdcall;
     // Props
     // -- Action count
    property Count: Integer read GetCount;
     // -- Action by index. Index in range 0..Count-1
    property Items[Index: Integer]: IPhoaAction read GetItems; default;
  end;

   //===================================================================================================================
   // An abstract menu entry
   //===================================================================================================================

  IPhoaMenu = interface;

  IPhoaMenuEntry = interface(IInterface)
    ['{D6AF0D63-5419-490F-A7AB-03AA448CED0D}']
     // Removes itself from owner menu
    procedure Remove; stdcall;
     // Prop handlers
    function  GetIndex: Integer; stdcall;
    function  GetOwner: IPhoaMenu; stdcall;
    procedure SetIndex(Value: Integer); stdcall;
     // Props
     // -- Index in Owner menu
    property Index: Integer read GetIndex write SetIndex;
     // -- Owner menu (nil for a top-level entry)
    property Owner: IPhoaMenu read GetOwner;
  end;

   //===================================================================================================================
   // A menu item
   //===================================================================================================================

  IPhoaMenuItem = interface(IPhoaMenuEntry)
    ['{D6AF0D63-5419-490F-A7AB-03AA448CED0E}']
     // Prop handlers
    function  GetAction: IPhoaAction; stdcall;
     // Props
     // -- Action associated with the item. Can be nil for standard menu items
    property Action: IPhoaAction read GetAction;
  end;

   //===================================================================================================================
   // A menu (or submenu)
   //===================================================================================================================

  IPhoaMenu = interface(IPhoaMenuEntry)
    ['{D6AF0D63-5419-490F-A7AB-03AA448CED0F}']
     // Adds and returns a new submenu
    function  AddMenu: IPhoaMenu; stdcall;
     // Adds and returns a new item
    function  AddItem(Action: IPhoaAction): IPhoaMenuItem; stdcall;
     // Prop handlers
    function  GetCaption: String; stdcall;
    function  GetCaptionW: WideString; stdcall;
    function  GetSubentryCount: Integer; stdcall;
    function  GetSubentries(Index: Integer): IPhoaMenuEntry; stdcall;
    procedure SetCaption(const Value: String); stdcall;
    procedure SetCaptionW(const Value: WideString); stdcall;
     // Props
     // -- Menu caption
    property Caption: String read GetCaption write SetCaption;
    property CaptionW: WideString read GetCaptionW write SetCaptionW;
     // -- Subentry count
    property SubentryCount: Integer read GetSubentryCount;
     // -- Subentries by index. Index in range 0..SubentryCount-1
    property Subentries[Index: Integer]: IPhoaMenuEntry read GetSubentries; default;
  end;

   //===================================================================================================================
   // PhoA application interface
   //===================================================================================================================

   // Control currently focused in the application
  TPhoaAppFocusedControl = (pafcNone, pafcGroupTree, pafcThumbViewer);

  IPhoaApp = interface(IInterface)
    ['{D931B4CD-F8F0-48F7-B844-F1BECAC0E045}']
     // Prop handlers
    function  GetActionList: IPhoaActionList; stdcall;
    function  GetCurGroup: IPhoaPicGroup; stdcall;
    function  GetFocusedControl: TPhoaAppFocusedControl; stdcall;
    function  GetMenu: IPhoaMenu; stdcall;
    function  GetProject: IPhoaProject; stdcall;
    function  GetSelectedPics: IPhoaPicList; stdcall;
    function  GetViewedPics: IPhoaPicList; stdcall;
    procedure SetCurGroup(Value: IPhoaPicGroup); stdcall;
     // Props
     // -- Application action list
    property ActionList: IPhoaActionList read GetActionList;
     // -- Group curently selected
    property CurGroup: IPhoaPicGroup read GetCurGroup write SetCurGroup;
     // -- Control currently focused
    property FocusedControl: TPhoaAppFocusedControl read GetFocusedControl;
     // -- The main menu of the application 
    property Menu: IPhoaMenu read GetMenu;
     // -- Active project
    property Project: IPhoaProject read GetProject;
     // -- Pictures (thumbnails) currently selected
    property SelectedPics: IPhoaPicList read GetSelectedPics;
     // -- Pictures (thumbnails) currently displayed
    property ViewedPics: IPhoaPicList read GetViewedPics;
  end;

implementation

end.
 
