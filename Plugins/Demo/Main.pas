//**********************************************************************************************************************
//  $Id: Main.pas,v 1.5 2005-03-01 14:24:53 dale Exp $
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

  IPhoaDemoPluginModule = interface(IPhoaPluginModule)
    ['{FF55A378-9411-480B-A337-60B894AF4273}']
     // Prop handlers
    function  GetApp: IPhoaApp; 
     // Props
     // -- Application reference
    property App: IPhoaApp read GetApp;
  end;

  TPhoaDemoPluginModule = class(TInterfacedObject, IPhoaPluginModule, IPhoaDemoPluginModule)
  private
     // Prop storage
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
     // IPhoaDemoPluginModule
    function  GetApp: IPhoaApp;
  end;

   //===================================================================================================================
   // Demo plugin class
   //===================================================================================================================

  IPhoaDemoPluginClass = interface(IPhoaPluginClass)
    ['{FF55A378-9411-480B-A337-60B894AF4274}']
     // Prop handlers
    function  GetModule: IPhoaDemoPluginModule;
     // Props
     // -- Module reference
    property Module: IPhoaDemoPluginModule read GetModule;
  end;

  TPhoaDemoPluginClass = class(TInterfacedObject, IPhoaPluginClass, IPhoaDemoPluginClass)
  private
     // Prop storage
    FModule: IPhoaDemoPluginModule;
     // IPhoaPluginClass
    function  CreatePlugin: IPhoaPlugin; stdcall;
    function  GetDescription: WideString; stdcall;
    function  GetKind: TPhoaPluginKind; stdcall;
    function  GetName: WideString; stdcall;
     // IPhoaDemoPluginClass
    function  GetModule: IPhoaDemoPluginModule;
  public
    constructor Create(AModule: IPhoaDemoPluginModule);
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
    FPluginClass: IPhoaDemoPluginClass;
     // IPhoaPlugin
    function  GetPluginClass: IPhoaPluginClass; stdcall;
  public
    constructor Create(APluginClass: IPhoaDemoPluginClass);
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
  begin
     // Store application reference
    FApp := App;
  end;

  function TPhoaDemoPluginModule.GetApp: IPhoaApp;
  begin
    Result := FApp;
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
    if FPluginClass=nil then FPluginClass := TPhoaDemoPluginClass.Create(Self);
    Result := FPluginClass;
  end;

   //===================================================================================================================
   // TPhoaDemoPluginClass
   //===================================================================================================================

  constructor TPhoaDemoPluginClass.Create(AModule: IPhoaDemoPluginModule);
  begin
    inherited Create;
    FModule := AModule;
    MessageBox(0, 'Plugin class created.', 'Demo plugin', 0);
  end;

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

  function TPhoaDemoPluginClass.GetModule: IPhoaDemoPluginModule;
  begin
    Result := FModule;
  end;

  function TPhoaDemoPluginClass.GetName: WideString;
  begin
    Result := 'Demo PhoA plugin';
  end;

   //===================================================================================================================
   // TPhoaDemoPlugin
   //===================================================================================================================

  constructor TPhoaDemoPlugin.Create(APluginClass: IPhoaDemoPluginClass);
  begin
    inherited Create;
    FPluginClass := APluginClass;
     // Create and register a new action
    FAction := TPhoaDemoPlugin_InfoAction.Create(nil, 'aDemoPlugin_Info', 'Demo', '&Demo plugin info...', 'Demo plugin info...|Click and you''ll see!');
    FPluginClass.Module.App.ActionList.Add(FAction);
     // Create a new item corresponding to the action
    FItem := (FPluginClass.Module.App.Menu.Subentries[0] as IPhoaMenu).AddItem(FAction);
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
      FPluginClass.Module.App.ActionList.Remove(FAction);
      FAction := nil;
    end;
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
 
