//**********************************************************************************************************************
//  $Id: PhoaDemoPlugin.dpr,v 1.3 2005-02-26 12:35:51 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
library PhoaDemoPlugin;

{$R *.res}

uses
  phIntf in '..\..\phIntf.pas',
  phAppIntf in '..\..\phAppIntf.pas',
  phPlugin in '..\..\phPlugin.pas',
  Main in 'Main.pas',
  phActionImpl in '..\phActionImpl.pas';

exports
  PhoaGetPluginModule;

begin
end.

