//**********************************************************************************************************************
//  $Id: phoadump.dpr,v 1.3 2004-09-11 17:52:37 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
//**********************************************************************************************************************
//
// PhoA sample project
// The whole code (c) Dmitry Kann, except where otherwise explicitly noted
// Home sites:
//   http://devtools.narod.ru/
//   http://phoa.narod.ru/
//
// ATTENTION! None of this code can be reproduced in any form without prior
// permission issued by the author.
//
// Contact email: phoa@narod.ru
//
// NB: To compile the project you will need phPhoa.pas file included with
//     main PhoA distribution, available at http://phoa.narod.ru
//
// Target platform: Borland Delphi 7 (but may compile on earlier platforms)
// Target OS:       Windows
// Language:        Object Pascal
//
//**********************************************************************************************************************
program phoadump;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  phPhoa in '..\..\phPhoa.pas'; // You will probably need to edit this path for this to point to the right place

const
  SAppName      = 'phoadump';
  SAppVersion   = '0.01';
  SAppCopyright = 'Copyright (c)2002-2004 Dmitry Kann, http://phoa.narod.ru';

   // Terminates the program with error message reported
  procedure Err(const sMessage: String);
  begin
    Writeln('Error: '+sMessage);
    Halt(1);
  end;

   // Prints out program usage info
  procedure ExplainUsage;
  begin
    Writeln(Format('  Usage: %s <filename>[.phoa]', [SAppName]));
  end;

   // Performs file processing
  procedure DoDump(const sFileName: String);
  var
    sPhoaFile: String;
    Filer: TPhoaFiler;

     // Appends '.phoa' extension if filename doesn't have one
    function AddPhoaExt(const sFileName: String): String;
    var iDotPos: Integer;
    begin
      Result := sFileName;
      iDotPos := LastDelimiter('.', Result);
      if (iDotPos=0) or (iDotPos<LastDelimiter('\', Result)) then Result := Result+'.phoa';
    end;

     // Returns index in phPhoa.aPhFileRevisions[] array corresponding to given revision number, or -1 if not found
    function IndexOfRevision(iRevNumber: Integer): Integer;
    begin
      for Result := 0 to High(aPhFileRevisions) do
        if aPhFileRevisions[Result].iNumber=iRevNumber then Exit;
      Result := -1;
    end;

     // Returns string revision description
    function DescribeRevision(iRevNumber: Integer): String;
    var idx: Integer;
    begin
      idx := IndexOfRevision(iRevNumber);
      if idx<0 then
        Result := '<revision is unknown>'
      else
        Result := Format(
          'Revision name: "%s"; PhoA version to open: %s',
          [aPhFileRevisions[idx].sName, aPhFileRevisions[idx].sMinName]);
    end;

     // Dumps chunks one-by-one
    procedure DumpChunks;

      procedure DumpChunk(iLevel: Integer; Code: TPhChunkCode; Datatype: TPhChunkDatatype);
      const
        aChDatatypeNames: Array[TPhChunkDatatype] of String =
          ('Empty', 'Byte', 'Word', 'Int', 'StringB', 'StringW', 'StringI');
      begin
        Writeln(Format('%s%.4x - %s', [StringOfChar(' ', iLevel*2), Code, aChDatatypeNames[Datatype]]));
      end;

      procedure DumpNested(OpenCode: TPhChunkCode; iLevel: Integer);
      var
        Code: TPhChunkCode;
        Datatype: TPhChunkDatatype;
        vValue: Variant;
      begin
        while Filer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do begin
           // OpenCode of 0 means top-level invocation
          if (OpenCode>0) and IsCloseChunk(Code, OpenCode) then begin
            DumpChunk(iLevel-1, Code, Datatype);
            Break;
          end;
          DumpChunk(iLevel, Code, Datatype);
          if IsOpenChunk(Code) then DumpNested(Code, iLevel+1);
        end;
      end;

    begin
      DumpNested(0, 0);
    end;

  begin
     // Check whether extension is specified
    sPhoaFile := AddPhoaExt(sFileName);
    Writeln(Format('* File: %s', [sPhoaFile]));
    try
       // Create a filer object
      Filer := TPhoaFiler.Create(psmRead, sPhoaFile);
      try
         // Read the file header
        Filer.ReadHeader;
         // Print out the revision
        Writeln(Format('* Revision: %d', [Filer.RevisionNumber]));
        Writeln('  '+DescribeRevision(Filer.RevisionNumber));
         // Process chunks
        DumpChunks;
      finally
        Filer.Free;
      end;
    except
      on e: Exception do Err(e.Message);
    end;
  end;

begin
   // Some HelloWorld-ish words...
  Writeln(Format('%s v%s. %s', [SAppName, SAppVersion, SAppCopyright]));
   // If user hasn't specified any parameters on the command line, learn him. Process the file otherwise
  if ParamCount<1 then ExplainUsage else DoDump(ParamStr(1));
end.
