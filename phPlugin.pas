//**********************************************************************************************************************
//  $Id: phPlugin.pas,v 1.6 2005-02-26 20:18:09 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
// Declarations of PhoA plugin interface: pure Unicode implementation to allow cross-platform development
//
unit phPlugin;

interface

uses phIntf, phAppIntf;

type
  IPhoaPluginModule = interface;
  IPhoaPlugin       = interface;
  IPhoaPluginClass  = interface;

   // A plugin module interface obtaining function prototype. Should be exported from a plugin DLL named
   //   'PhoaGetPluginModule'. Must create and return a module instance.
  TPhoaGetPluginModuleProc = function: IPhoaPluginModule; stdcall;

   // Plugin kind
  TPhoaPluginKind = (
    ppkBrowseMode          // Browse mode plugin. The plugin is instantiated automatically once the application finished initialization
  );

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

implementation

end.
