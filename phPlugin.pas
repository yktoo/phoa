//**********************************************************************************************************************
//  $Id: phPlugin.pas,v 1.3 2005-02-15 14:15:35 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
// Declarations of PhoA plugin interface: pure Unicode implementation to allow cross-platform development
//
unit phPlugin;

interface

uses phIntf;

type
  IPhoaPluginModule = interface;
  IPhoaPlugin       = interface;
  IPhoaPluginClass  = interface;

   // A plugin module interface obtaining function prototype. Should be exported from a plugin DLL named
   //   'PhoaGetPluginModule'. Must create and return a module instance.
  TPhoaGetPluginModuleProc = function: IPhoaPluginModule; stdcall;

   //===================================================================================================================
   // IPhoaPluginModule - A module implementing a number of plugin classes
   //===================================================================================================================

  IPhoaPluginModule = interface(IInterface)
    ['{B8F84EC7-5675-429D-88B1-13F819AB2E3B}']
     // Prop handlers
    function  GetInfoAuthor: WideString; stdcall;
    function  GetInfoCopyright: WideString; stdcall;
    function  GetInfoDescription: WideString; stdcall;
    function  GetInfoName: WideString; stdcall;
    function  GetInfoWebsiteURL: WideString; stdcall;
    function  GetPluginClassCount: Integer; stdcall;
    function  GetPluginClasses(Index: Integer): IPhoaPluginClass; stdcall;
     // Props
     // -- Module author info
    property InfoAuthor: WideString read GetInfoAuthor;
     // -- Module copyright info
    property InfoCopyright: WideString read GetInfoCopyright;
     // -- Module description
    property InfoDescription: WideString read GetInfoDescription;
     // -- Module name
    property InfoName: WideString read GetInfoName;
     // -- Module website URL
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
    function  GetName: WideString; stdcall;
     // Props
     // -- Plugin class description
    property Description: WideString read GetDescription;
     // -- Plugin class name
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
