//**********************************************************************************************************************
//  $Id: PhoaDemoPlugin.dpr,v 1.1 2005-02-14 19:34:09 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
library PhoaDemoPlugin;

{$R *.res}

uses
  phIntf in '..\..\phIntf.pas',
  phPlugin in '..\..\phPlugin.pas',
  Main in 'Main.pas';

exports
  PhoaPluginGetClassCount name 'PhoaPluginGetClassCount',
  PhoaPluginGetClass      name 'PhoaPluginGetClass';

begin
end.

