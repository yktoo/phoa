//**********************************************************************************************************************
//  $Id: Main.pas,v 1.6 2005-03-01 21:35:40 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit Main;

interface

uses Windows, phPlugin, phAppIntf;

type

   //===================================================================================================================
   // Demo plugin module
   //===================================================================================================================

  TPhoaDemoPluginModule = class(TInterfacedObject, IPhoaPluginModule)
  private
     // Plugin class which is being created only once
    FPluginClass: IPhoaPluginClass;
     // IPhoaPluginModule
    procedure AppInitialized(App: IPhoaApp); stdcall;
    procedure AppFinalizing; stdcall;
    function  GetInfoAuthor: WideString; stdcall;
    function  GetInfoCopyright: WideString; stdcall;
    function  GetInfoDescription: WideString; stdcall;
    function  GetInfoName: WideString; stdcall;
    function  GetInfoVersion: Cardinal;
    function  GetInfoVersionText: WideString;
    function  GetInfoWebsiteURL: WideString; stdcall;
    function  GetPluginClassCount: Integer; stdcall;
    function  GetPluginClasses(Index: Integer): IPhoaPluginClass; stdcall;
  end;

   //===================================================================================================================
   // Demo plugin class
   //===================================================================================================================

  TPhoaDemoPluginClass = class(TInterfacedObject, IPhoaPluginClass)
  private
     // IPhoaPluginClass
    function  CreatePlugin: IPhoaPlugin; stdcall;
    function  GetDescription: WideString; stdcall;
    function  GetKind: TPhoaPluginKind; stdcall;
    function  GetName: WideString; stdcall;
  end;

   //===================================================================================================================
   // Demo plugin
   //===================================================================================================================

  TPhoaDemoPlugin = class(TInterfacedObject, IPhoaPlugin)
  private
     // Plugin menu item
    FItem: IPhoaMenuItem;
     // Main plugin action
    FAction: IPhoaAction;
     // Prop storage
    FPluginClass: IPhoaPluginClass;
     // Plugin action execute proc
    procedure ActionExec;
     // IPhoaPlugin
    function  GetPluginClass: IPhoaPluginClass; stdcall;
  public
    constructor Create(APluginClass: IPhoaPluginClass);
    destructor Destroy; override;
  end;

var
   // Globally accessible application reference. Should be thread-safe since initialization and finalization are always
   //   performed "in the foreground", by the main thread. If your plugin contains multiple threads, be sure to
   //   terminate them before finalization (and interface cleanup) takes place; otherwise you'll always get an Access
   //   Violation because the plugin module gets unloaded shortly after the finalization (Module.AppFinalizing call). 
  PhoaApp: IPhoaApp;

   // Exported function
  function PhoaGetPluginModule: IPhoaPluginModule; stdcall;

implementation

  function PhoaGetPluginModule: IPhoaPluginModule;
  begin
    Result := TPhoaDemoPluginModule.Create;
  end;

   //===================================================================================================================
   // TPhoaDemoPluginModule
   //===================================================================================================================

  procedure TPhoaDemoPluginModule.AppFinalizing;
  begin
     // ALWAYS RELEASE ALL INTERFACE REFERENCES HERE because it's the last point where plugin library is still loaded!
    PhoaApp := nil;
  end;

  procedure TPhoaDemoPluginModule.AppInitialized(App: IPhoaApp);
  begin
     // Store global application reference
    PhoaApp := App;
  end;

  function TPhoaDemoPluginModule.GetInfoAuthor: WideString;
  begin
    Result := 'Dmitry Kann';
  end;

  function TPhoaDemoPluginModule.GetInfoCopyright: WideString;
  begin
    Result := 'Copyright ©2005 DK Software';
  end;

  function TPhoaDemoPluginModule.GetInfoDescription: WideString;
  begin
    Result := 'This is a sample plugin doing nothing';
  end;

  function TPhoaDemoPluginModule.GetInfoName: WideString;
  begin
    Result := 'PhoA demo plugin';
  end;

  function TPhoaDemoPluginModule.GetInfoVersion: Cardinal;
  begin
    Result := $00010000;
  end;

  function TPhoaDemoPluginModule.GetInfoVersionText: WideString;
  begin
    Result := '0.01.00.00';
  end;

  function TPhoaDemoPluginModule.GetInfoWebsiteURL: WideString;
  begin
    Result := 'http://www.dk-soft.org/';
  end;

  function TPhoaDemoPluginModule.GetPluginClassCount: Integer;
  begin
    Result := 1;
  end;

  function TPhoaDemoPluginModule.GetPluginClasses(Index: Integer): IPhoaPluginClass;
  begin
     // Create a plugin class if it isn't created yet
    if FPluginClass=nil then FPluginClass := TPhoaDemoPluginClass.Create;
    Result := FPluginClass;
  end;

   //===================================================================================================================
   // TPhoaDemoPluginClass
   //===================================================================================================================

  function TPhoaDemoPluginClass.CreatePlugin: IPhoaPlugin;
  begin
    Result := TPhoaDemoPlugin.Create(Self);
  end;

  function TPhoaDemoPluginClass.GetDescription: WideString;
  begin
    Result := 'This is a sample plugin to show how PhoA plugin mechanism.';
  end;

  function TPhoaDemoPluginClass.GetKind: TPhoaPluginKind;
  begin
    Result := ppkBrowseMode;
  end;

  function TPhoaDemoPluginClass.GetName: WideString;
  begin
    Result := 'Demo PhoA plugin';
  end;

   //===================================================================================================================
   // TPhoaDemoPlugin
   //===================================================================================================================

  procedure DemoPluginActionExec(Sender: IPhoaAction); stdcall;
  begin
     // We store TPhoaDemoPlugin instance reference in action's Tag
    TPhoaDemoPlugin(Sender.Tag).ActionExec;
  end;

  procedure TPhoaDemoPlugin.ActionExec;
  begin
    MessageBox(0, 'Hello world!', 'Greetings', MB_OK);
  end;

  constructor TPhoaDemoPlugin.Create(APluginClass: IPhoaPluginClass);
  begin
    inherited Create;
    FPluginClass := APluginClass;
     // Create and register a new action
    FAction := PhoaApp.ActionList.AddW('aDemoPlugin_Info', DemoPluginActionExec);
    FAction.CategoryW := 'Demo';
    FAction.CaptionW  := '&Demo plugin info...';
    FAction.HintW     := 'Demo plugin info...|Click and you''ll see!';
     // Store self reference into action tag to track the target of the action in execute handler.
     // WARNING: avoid referencing self as an interface because this would cause circular interface reference (we have
     //   to keep Action reference to track its state, maybe Caption etc.)
    FAction.Tag       := Integer(Self);
     // Create a new item corresponding to the action
    FItem := (PhoaApp.Menu.Subentries[0] as IPhoaMenu).AddItem(FAction);
  end;

  destructor TPhoaDemoPlugin.Destroy;
  begin
     // Remove menu item
    if FItem<>nil then begin
      FItem.Remove;
      FItem := nil;
    end;
     // Release action
    if FAction<>nil then begin
      PhoaApp.ActionList.Remove(FAction);
      FAction := nil;
    end;
    inherited Destroy;
  end;

  function TPhoaDemoPlugin.GetPluginClass: IPhoaPluginClass;
  begin
    Result := FPluginClass;
  end;

end.
 
