//**********************************************************************************************************************
//  $Id: phPlugin.pas,v 1.2 2005-02-14 19:34:08 dale Exp $
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

  IPhoaPlugin = interface;
  IPhoaPluginClass = interface;

   // Plugin class enumeration procedure prototypes. These procedures should be exported from a plugin DLL.
   // -- Must return number of plugin classes contained in the DLL in iCount (1 or more). Should be exported named
   //    'PhoaPluginGetClassCount'
  TPhoaPluginGetClassCountProc = procedure(var iCount: Integer); stdcall;
   // -- Must create and return an instance of iIndex-th plugin class (iIndex in range 0..iCount-1 where iCount is the
   //    value returned by PhoaPluginGetCount()). Should be exported named 'PhoaPluginGetClass'
  TPhoaPluginGetClassProc = procedure(iIndex: Integer; var PluginClass: IPhoaPluginClass); stdcall;

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
    function  GetPluginClass: IPhoaPluginClass;
     // Props
     // -- Plugin's class
    property PluginClass: IPhoaPluginClass read GetPluginClass;
  end;



implementation

end.
