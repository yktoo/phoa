//**********************************************************************************************************************
//  $Id: phAppIntf.pas,v 1.6 2005-03-02 17:13:45 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
// This unit contains application-related interface declarations and API designated mainly for plugins. Pure Unicode
// implementation to allow cross-platform development.

unit phAppIntf;

interface
uses phIntf;

type

  IPhoaPluginModule = interface;
  IPhoaPlugin       = interface;
  IPhoaPluginClass  = interface;

   //===================================================================================================================
   // An action (the same as command)
   //===================================================================================================================

  IPhoaAction = interface(IInterface)
    ['{03A7EFBC-0C77-481B-A3B9-1D25BB2D458C}']
     // Executes the action. Returns True, if the action succeeded, or False if it was failed due to any reason
    function  Execute: LongBool; stdcall;
     // Prop handlers
    function  GetCaption: WideString; stdcall;
    function  GetCategory: WideString; stdcall;
    function  GetEnabled: LongBool; stdcall;
    function  GetHint: WideString; stdcall;
    function  GetName: WideString; stdcall;
    function  GetTag: Integer; stdcall;
    procedure SetCaption(const Value: WideString); stdcall;
    procedure SetCategory(const Value: WideString); stdcall;
    procedure SetEnabled(Value: LongBool); stdcall;
    procedure SetHint(const Value: WideString); stdcall;
    procedure SetTag(Value: Integer); stdcall;
     // Props
     // -- Action caption
    property Caption: WideString read GetCaption write SetCaption;
     // -- Action category name. A category name is for grouping similar actions. Example: 'View' or 'Edit'
    property Category: WideString read GetCategory write SetCategory;
     // -- True when action is enabled, otherwise False
    property Enabled: LongBool read GetEnabled write SetEnabled;
     // -- Action hint: a message that is displayed in the status bar and in a tooltip. To have two different
     //    messages displayed, separate tooltip text from status bar text with a pipe ('|'). Example:
     //    'Import...|Invokes the Import dialog'
    property Hint: WideString read GetHint write SetHint;
     // -- Action name: an unique case-insensitive action identifier. The name is assigned to an action when it is
     //    created and cannot be changed afterwards.
     //    Standard PhoA actions all start with small 'a'. It would be good to conform that rule, but add your specific
     //    Id after a. For example: assume your plugin is named 'Door opener', and you write a knock action. So the
     //    action should be named 'aDoorOpener_Knock': your plugin name acts like a prefix.
    property Name: WideString read GetName;
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
     // Adds and returns a new action interface. The action interface isn't referenced in the list, so you will have to
     //   keep action references in your plugin module! Once the action is no more referenced, it is automatically
     //   deleted from the list
    function  Add(const sName: WideString; AExecuteProc: TPhoaActionExecuteProc): IPhoaAction; stdcall;
     // Finds and returns the action by name; returns nil if no such action found
    function  FindName(const sName: WideString): IPhoaAction; stdcall;
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
   // A menu separator
   //===================================================================================================================

  IPhoaMenuSeparator = interface(IPhoaMenuEntry)
    ['{D6AF0D63-5419-490F-A7AB-03AA448CED0F}']
  end;

   //===================================================================================================================
   // A menu (or submenu)
   //===================================================================================================================

  IPhoaMenu = interface(IPhoaMenuEntry)
    ['{D6AF0D63-5419-490F-A7AB-03AA448CED10}']
     // Adds and returns a new submenu. The newly-created menu interface isn't referenced in Subentries[] list unless
     //   this object is also created by your module. Once the menu is no more referenced, it is automatically
     //   deleted from the owner's menu
    function  AddMenu: IPhoaMenu; stdcall;
     // Adds and returns a new item. The same referencing rules as for AddMenu() apply 
    function  AddItem(Action: IPhoaAction): IPhoaMenuItem; stdcall;
     // Adds and returns a new separator. The same referencing rules as for AddMenu() apply
    function  AddSeparator: IPhoaMenuSeparator; stdcall;
     // Returns a first item which is associated with the specified Action identified by name. If bRecursive=True, also
     //   searches the subitems. If no item found, or Action=nil, returns nil
    function  FindItemByActionName(const sActionName: WideString; bRecursive: LongBool): IPhoaMenuItem; stdcall;
     // Prop handlers
    function  GetCaption: WideString; stdcall;
    function  GetItemByActionName(const sActionName: WideString; bRecursive: LongBool): IPhoaMenuItem; stdcall;
    function  GetSubentryCount: Integer; stdcall;
    function  GetSubentries(Index: Integer): IPhoaMenuEntry; stdcall;
    procedure SetCaption(const Value: WideString); stdcall;
     // Props
     // -- Menu caption
    property Caption: WideString read GetCaption write SetCaption;
     // -- Returns a first item corresponding to an action. Raises exception if no such item found
    property ItemByActionName[const sActionName: WideString; bRecursive: LongBool]: IPhoaMenuItem read GetItemByActionName;
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

   //===================================================================================================================
   // Plugin declarations
   //===================================================================================================================

   // A plugin module interface obtaining function prototype. Should be exported from a plugin DLL named
   //   'PhoaGetPluginModule'. Must create and return a module instance.
  TPhoaGetPluginModuleProc = function: IPhoaPluginModule; stdcall;

   // Plugin kind
  TPhoaPluginKind = (
    ppkBrowseMode // Browse mode plugin. The plugin is instantiated automatically once the application has finished initialization
  );
  TPhoaPluginKinds = set of TPhoaPluginKind;

   //===================================================================================================================
   // IPhoaPluginModule - A module implementing a number of plugin classes
   //===================================================================================================================

  IPhoaPluginModule = interface(IInterface)
    ['{B8F84EC7-5675-429D-88B1-13F819AB2E3B}']
     // Called by the application once all of the modules have been loaded and application initialization process is
     //   finished. Should perform any required module initialization. App is the main application interface, the
     //   module can (and should) copy and store it locally for future reference
    procedure AppInitialized(App: IPhoaApp); stdcall;
     // Called by the application when it needs to close and just before cleanup routine starts. Should perform any
     //   required module cleanup
    procedure AppFinalizing; stdcall;
     // Prop handlers
    function  GetInfoAuthor: WideString; stdcall;
    function  GetInfoCopyright: WideString; stdcall;
    function  GetInfoDescription: WideString; stdcall;
    function  GetInfoName: WideString; stdcall;
    function  GetInfoVersion: Cardinal;
    function  GetInfoVersionText: WideString;
    function  GetInfoWebsiteURL: WideString; stdcall;
    function  GetPluginClassCount: Integer; stdcall;
    function  GetPluginClasses(Index: Integer): IPhoaPluginClass; stdcall;
     // Props
     // -- Module author info. Example: 'SuperSoft Solutions Inc.'
    property InfoAuthor: WideString read GetInfoAuthor;
     // -- Module copyright info. Example: 'Copyright (c)1890-2005 SuperSoft Solutions Inc.'
    property InfoCopyright: WideString read GetInfoCopyright;
     // -- Module description. Example: 'A PhoA plugin doing just everything'
    property InfoDescription: WideString read GetInfoDescription;
     // -- Module name. Example: 'PhoA SuperPlugin'
    property InfoName: WideString read GetInfoName;
     // -- Module version; each byte represents a single version part. Example: version 1.1.10 will be $01010a00
    property InfoVersion: Cardinal read GetInfoVersion;
     // -- Module text version; should match InfoVersion value, but additionally can include a version qualifier.
     //    Example: '1.1.10 beta'
    property InfoVersionText: WideString read GetInfoVersionText;
     // -- Module website or download URL. Example: 'http://www.dk-soft.org/products/phoa/'; 
    property InfoWebsiteURL: WideString read GetInfoWebsiteURL;
     // -- Number of plugin classes described in the module
    property PluginClassCount: Integer read GetPluginClassCount;
     // -- Returns a plugin class interface (possibly, created only once); Index is in range 0..PluginClassCount-1
    property PluginClasses[Index: Integer]: IPhoaPluginClass read GetPluginClasses; default;
  end;

   //===================================================================================================================
   // IPhoaPluginClass - PhoA plugin class
   //===================================================================================================================

  IPhoaPluginClass = interface(IInterface)
    ['{B8F84EC7-5675-429D-88B1-13F819AB2E3C}']
     // Creates a new plugin instance
    function  CreatePlugin: IPhoaPlugin; stdcall;
     // Prop handlers
    function  GetDescription: WideString; stdcall;
    function  GetKind: TPhoaPluginKind; stdcall;
    function  GetName: WideString; stdcall;
     // Props
     // -- Plugin class description
    property Description: WideString read GetDescription;
     // -- Kind of plugins of the class
    property Kind: TPhoaPluginKind read GetKind;
     // -- Plugin class name. Example: 'SuperPlugin '
    property Name: WideString read GetName;
  end;

   //===================================================================================================================
   // IPhoaPlugin - A PhoA plugin
   //===================================================================================================================

  IPhoaPlugin = interface(IInterface)
    ['{B8F84EC7-5675-429D-88B1-13F819AB2E3D}']
     // Prop handlers
    function  GetPluginClass: IPhoaPluginClass; stdcall;
     // Props
     // -- Plugin's class
    property PluginClass: IPhoaPluginClass read GetPluginClass;
  end;

const
   // All plugin kinds
  PluginKinds_All = [Low(TPhoaPluginKind)..High(TPhoaPluginKind)];   

implementation

end.
 
