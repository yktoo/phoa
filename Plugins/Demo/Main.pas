//**********************************************************************************************************************
//  $Id: Main.pas,v 1.4 2005-02-27 15:51:49 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit Main;

interface

uses Windows, phPlugin, phAppIntf, phActionImpl;

type

   //===================================================================================================================
   // Demo plugin module
   //===================================================================================================================

  TPhoaDemoPluginModule = class(TInterfacedObject, IPhoaPluginModule)
  private
     // Locally stored Application interface
    FApp: IPhoaApp;
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
  public
    constructor Create;
  end;

  TPhoaDemoPluginClass = class(TInterfacedObject, IPhoaPluginClass)
  private
     // IPhoaPluginClass
    function  CreatePlugin: IPhoaPlugin; stdcall;
    function  GetDescription: WideString; stdcall;
    function  GetKind: TPhoaPluginKind; stdcall;
    function  GetName: WideString; stdcall;
  public
    constructor Create;
  end;

  TPhoaDemoPlugin = class(TInterfacedObject, IPhoaPlugin)
  private
     // Application reference
    FApp: IPhoaApp;
     // Plugin menu item
    FItem: IPhoaMenuItem;
     // Main plugin action
    FAction: IPhoaAction;
     // Prop storage
    FPluginClass: IPhoaPluginClass;
     // IPhoaPlugin
    function  GetPluginClass: IPhoaPluginClass; stdcall;
  public
    constructor Create(APluginClass: IPhoaPluginClass; AApp: IPhoaApp);
    destructor Destroy; override;
  end;

  TPhoaDemoPlugin_InfoAction = class(TPhoaPluginAction)
  protected
    function  Execute: LongBool; override; stdcall; 
  end;

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
    FApp := nil;
  end;

  procedure TPhoaDemoPluginModule.AppInitialized(App: IPhoaApp);
  var Action: IPhoaAction;
  begin
    FApp := App;
     // Create and register a new action
    Action := TPhoaDemoPlugin_InfoAction.Create(nil, 'aDemoPlugin_Info', 'Demo', '&Demo plugin info...', 'Demo plugin info...|Click and you''ll see!');
    FApp.ActionList.Add(Action);
     // Create a new item corresponding to the action
    (FApp.Menu.Subentries[0] as IPhoaMenu).AddItem(Action); 
  end;

  constructor TPhoaDemoPluginModule.Create;
  begin
    inherited Create;
     // Create a plugin class
    FPluginClass := TPhoaDemoPluginClass.Create;
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
    Result := FPluginClass;
  end;

   //===================================================================================================================
   // TPhoaDemoPluginClass
   //===================================================================================================================

  constructor TPhoaDemoPluginClass.Create;
  begin
    inherited Create;
    MessageBox(0, 'Plugin class created.', 'Demo plugin', 0);
  end;

  function TPhoaDemoPluginClass.CreatePlugin: IPhoaPlugin;
  begin
    Result := nil;
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

  constructor TPhoaDemoPlugin.Create(APluginClass: IPhoaPluginClass; AApp: IPhoaApp);
  begin
    inherited Create;
    FPluginClass := APluginClass;
    FApp := AApp;
  end;

  destructor TPhoaDemoPlugin.Destroy;
  begin
     // Remove menu item
    FItem.Remove; 
     // Release action
    FApp.ActionList.Remove(FAction);
    inherited Destroy;
  end;

  function TPhoaDemoPlugin.GetPluginClass: IPhoaPluginClass;
  begin
    Result := FPluginClass;
  end;

   //===================================================================================================================
   // TPhoaDemoPlugin_InfoAction
   //===================================================================================================================

  function TPhoaDemoPlugin_InfoAction.Execute: LongBool;
  begin
    MessageBox(0, 'Hello world!', 'Greetings', MB_OK);
    Result := True;
  end;

end.
 
