//**********************************************************************************************************************
//  $Id: phoaxml.dpr,v 1.2 2004-04-15 12:54:11 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
//**********************************************************************************************************************
//
// PhoA sample project
// Authors:
//   Max Belugin
//     http://belugin.newmail.ru/
//   Dmitry Kann <phoa@narod.ru>
//     http://devtools.narod.ru/
//     http://phoa.narod.ru/
//
// NB: To compile the project you will need phPhoa.pas file included with
//     main PhoA distribution, available at http://phoa.narod.ru
//
// Target platform: Borland Delphi 7 (but may compile on earlier platforms)
// Target OS:       Windows
// Language:        Object Pascal
//
//**********************************************************************************************************************
program phoaxml;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  ActiveX,
  phPhoa in '..\..\phPhoa.pas', // You will probably need to edit this path for this to point to the right place
  phoaXMLGen in 'phoaXMLGen.pas';

const
  SAppName      = 'phoaxml';
  SAppVersion   = '0.02';
  SAppCopyright = 'Copyright (c)2004 Max Belugin, Dmitry Kann, http://phoa.narod.ru';

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
    sPhoaFile, sXMLFile: String;
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
    var XMLGen: TPhoaXMLGen;

      procedure DumpNested(OpenCode: TPhChunkCode);
      var
        Code: TPhChunkCode;
        Datatype: TPhChunkDatatype;
        vValue: Variant;
      begin
        while Filer.ReadChunkValue(Code, Datatype, vValue, True, True)=rcrOK do begin
           // OpenCode of 0 means top-level invocation
          if (OpenCode>0) and IsCloseChunk(Code, OpenCode) then begin
            XMLGen.ParseChunk(Code, Datatype, vValue);
            Break;
          end;
          XMLGen.ParseChunk(Code, Datatype, vValue);
          if IsOpenChunk(Code) then DumpNested(Code);
        end;
      end;

    begin
      Write('  Processing the file...');
      XMLGen := TPhoaXMLGen.Create;
      try
        DumpNested(0);
        XMLGen.SaveToFile(sXMLFile);
        Write('OK');
      finally
        XMLGen.Free;
        Writeln;
      end;
    end;

  begin
     // Check whether extension is specified
    sPhoaFile := AddPhoaExt(sFileName);
    Writeln(Format('* Input PhoA file: %s', [sPhoaFile]));
    try
       // Create a filer object
      Filer := TPhoaFiler.Create(psmRead, sPhoaFile);
      try
         // Read the file header
        Filer.ReadHeader;
         // Print out the revision
        Writeln('  '+DescribeRevision(Filer.RevisionNumber));
         // Process the file
        sXMLFile  := ChangeFileExt(sPhoaFile, '.xml');
        Writeln(Format('* Output XML file: %s', [sXMLFile]));
        DumpChunks;
      finally
        Filer.Free;
      end;
    except
      on e: Exception do Err(e.Message);
    end;
  end;

begin
  CoInitialize(nil);
  try
    Writeln(Format('%s v%s. %s', [SAppName, SAppVersion, SAppCopyright]));
     // If user hasn't specified any parameters on the command line, learn him. Process the file otherwise
    if ParamCount<1 then ExplainUsage else DoDump(ParamStr(1));
  finally
    CoUninitialize;
  end;
end.

