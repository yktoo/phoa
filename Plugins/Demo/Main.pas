//**********************************************************************************************************************
//  $Id: Main.pas,v 1.1 2005-02-14 19:34:09 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit Main;

interface

uses Windows, phPlugin;

type
  TPhoaDemoPluginClass = class(TInterfacedObject, IPhoaPluginClass)
  private
     // IPhoaPluginClass
    function  CreatePlugin: IPhoaPlugin; stdcall;
    function  GetDescription: WideString; stdcall;
    function  GetName: WideString; stdcall;
  public
    constructor Create;
  end;

   // Exported procs
  procedure PhoaPluginGetClassCount(var iCount: Integer); stdcall;
  procedure PhoaPluginGetClass(iIndex: Integer; var PluginClass: IPhoaPluginClass); stdcall;

implementation

  procedure PhoaPluginGetClassCount(var iCount: Integer);
  begin
    iCount := 1;
  end;

  procedure PhoaPluginGetClass(iIndex: Integer; var PluginClass: IPhoaPluginClass); stdcall;
  begin
    case iIndex of
      0:   PluginClass := TPhoaDemoPluginClass.Create;
      else PluginClass := nil;
    end;
  end;

   //===================================================================================================================
   // TPhoaDemoPluginClass
   //===================================================================================================================

  constructor TPhoaDemoPluginClass.Create;
  begin
    inherited Create;
    MessageBox(0, 'Plugin class created.', 'Info', 0);
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
 
