//**********************************************************************************************************************
//  $Id: Main.pas,v 1.2 2005-02-15 14:15:35 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit Main;

interface

uses Windows, phPlugin;

type

   //===================================================================================================================
   // Demo plugin module
   //===================================================================================================================

  TPhoaDemoPluginModule = class(TInterfacedObject, IPhoaPluginModule)
  private
     // IPhoaPluginModule
    function  GetInfoAuthor: WideString; stdcall;
    function  GetInfoCopyright: WideString; stdcall;
    function  GetInfoDescription: WideString; stdcall;
    function  GetInfoName: WideString; stdcall;
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
    function  GetName: WideString; stdcall;
  public
    constructor Create;
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

  constructor TPhoaDemoPluginModule.Create;
  begin
    inherited Create;
    MessageBox(0, 'Module interface created.', 'Demo plugin', 0);
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
    Result := TPhoaDemoPluginClass.Create;
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
    Result := 'This is a sample plugin to demonstrate PhoA plugin mechanism.';
  end;

  function TPhoaDemoPluginClass.GetName: WideString;
  begin
    Result := 'Demo PhoA plugin';
  end;

end.
 
