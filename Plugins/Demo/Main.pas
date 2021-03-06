//**********************************************************************************************************************
//  $Id: Main.pas,v 1.8 2005-09-11 06:45:57 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit Main;

interface

uses Windows, phAppIntf;

type

   //===================================================================================================================
   // Demo plugin module
   //===================================================================================================================

  TPhoaDemoPluginModule = class(TInterfacedObject, IPhoaPluginModule)
  private
     // Plugin class which is being created only once
    FPluginClass: IPhoaPluginClass;
     // IPhoaPluginModule
    procedure Initialize(App: IPhoaApp); stdcall;
    procedure Finalize; stdcall;
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
     // Plugin menu
    FMenu: IPhoaMenu;
     // Plugin menu separator
    FSeparator: IPhoaMenuSeparator;
     // Prop storage
    FPluginClass: IPhoaPluginClass;
     // Plugin action execute proc
    procedure ActionExec;
     // IPhoaPlugin
    function  GetPluginClass: IPhoaPluginClass; stdcall;
  public
    constructor Create(APluginClass: IPhoaPluginClass);
  end;

var
   // Globally accessible application reference. Should be thread-safe since initialization and finalization are always
   //   performed "in the foreground", by the main thread. If your plugin contains multiple threads, be sure to
   //   terminate them before finalization (and interface cleanup) takes place; otherwise you'll always get an Access
   //   Violation because the plugin module gets unloaded shortly after the finalization (Module.Finalize call).
  PhoaApp: IPhoaApp;

   // Exported functions
  function PhoaGetPluginSubsystemRevision: Integer; stdcall; 
  function PhoaGetPluginModule: IPhoaPluginModule; stdcall;

implementation

  function PhoaGetPluginSubsystemRevision: Integer;
  begin
    Result := IPhoaPluginSubsystemRevision;
  end;

  function PhoaGetPluginModule: IPhoaPluginModule;
  begin
    Result := TPhoaDemoPluginModule.Create;
  end;

   //===================================================================================================================
   // TPhoaDemoPluginModule
   //===================================================================================================================

  procedure TPhoaDemoPluginModule.Finalize;
  begin
     // ALWAYS RELEASE ALL INTERFACE REFERENCES HERE because it's the last point where plugin library is still loaded!
    PhoaApp := nil;
  end;

  function TPhoaDemoPluginModule.GetInfoAuthor: WideString;
  begin
    Result := 'Dmitry Kann';
  end;

  function TPhoaDemoPluginModule.GetInfoCopyright: WideString;
  begin
    Result := 'Copyright �2005 DK Software';
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

  procedure TPhoaDemoPluginModule.Initialize(App: IPhoaApp);
  begin
     // Store global application reference
    PhoaApp := App;
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
    Result := 'This is a sample plugin to show how PhoA plugin mechanism works.';
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
    MessageBox(PhoaApp.Handle, 'Hello world!', 'Greetings', MB_OK);
  end;

  constructor TPhoaDemoPlugin.Create(APluginClass: IPhoaPluginClass);
  var
    ExitItem: IPhoaMenuItem;
    Action: IPhoaAction;
  begin
    inherited Create;
    FPluginClass := APluginClass;
     // Create and register a new action
    Action := PhoaApp.ActionList.Add('aDemoPlugin_Info', DemoPluginActionExec);
    Action.Category := 'Demo';
    Action.Caption  := '&Demo plugin info...';
    Action.Hint     := 'Demo plugin info...|Click and you''ll see!';
     // Store self reference into action tag to track the target of the action in execute handler (weak reference to
     //   avoid circular dependency!)
    Action.Tag       := Integer(Self);
     // Find the 'Exit' item
    ExitItem := PhoaApp.Menu.ItemByActionName['aExit', True];
     // Create a menu and insert it before 'Exit'
    FMenu := ExitItem.Owner.AddMenu;
    FMenu.Index := ExitItem.Index;
    FMenu.Caption := 'Demo pl&ugin menu';
     // Create a new item corresponding to the action. This creates a permanent Action reference
    FMenu.AddItem(Action);
     // Create a separator between menu and 'Exit'
    FSeparator := ExitItem.Owner.AddSeparator;
    FSeparator.Index := ExitItem.Index;
  end;

  function TPhoaDemoPlugin.GetPluginClass: IPhoaPluginClass;
  begin
    Result := FPluginClass;
  end;

end.
 
