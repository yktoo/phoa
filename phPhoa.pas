//**********************************************************************************************************************
//  $Id: phPhoa.pas,v 1.12 2005-05-31 17:29:49 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
//**********************************************************************************************************************
//
// PhoA file format description
// The whole code (c)2002-2005 Dmitry Kann, except where otherwise explicitly
// noted
// Home site:
//   http://www.dk-soft.org/
//
// ATTENTION! None of this code can be reproduced in any form without prior
// permission issued by the author.
//
// This unit describes general photo album file structure introduced in PhoA
// picture arranging program in release 1.1.1a. This revision is known as
// 'Revision 3' of .phoa file, and is the first one that supports so-called
// chunk-based organization, and therefore is extensible. Revisions 1 and 2
// were fixed-structure binary files, now they are proprietary PhoA formats
// supported only by the program itself. Their specifications may be supplied
// on an additional request.
//
// Contact email: phoa@narod.ru
//
// Target platform: Borland Delphi 7
// Target OS:       Windows
// Language:        Object Pascal
//
// Change log:
//   May 25, 2005 - dale - Added IPhChunk_Group_IconData chunk
//   Dec 07, 2004 - dale - Added IPhChunk_View_FilterExpression chunk
//   Nov 24, 2004 - dale - Fixes to TimeToPhoaTime() (now rounds the time value to minimize the errors)
//   Nov 19, 2004 - dale - Added new sorting properties (thumbnail width, height and dimensions) and sorting prop constants
//   Jun 06, 2004 - dale - Added a chunk for Group Description property
//   Jun 02, 2004 - dale - Added a chunk for Group ID property
//   May 30, 2004 - dale - Added chunks for picture Rotation and Flips properties
//**********************************************************************************************************************
unit phPhoa;

interface
uses SysUtils, Windows, Classes;

   // 1. Introduction
   // ===============
   // Photo album files (phoa-files) are binary files accessible only with
   // special programs. Native creator of .phoa is PhoA - picture arranging
   // tool, which is available for free download at http://www.dk-soft.org/
   //
   // Phoa-files contain various data on how pictures are arranged in the
   // album. At the moment there are five major logical sections in the
   // phoa-file data:
   //
   //  - File header
   //  - General photo album data
   //  - Pictures and their data as they appear in the phoa storage
   //  - Groups hierarchy and links to the pictures
   //  - Additional album data and structures
   //
   // 2. Photo album file sections
   // ============================
   // 2.1 File header
   // ---------------
   // Phoa-file always starts with ASCII string 'PhoA [PhotoAlbum] project
   // file', represented by SPhoAFileSignature constant.
   //
   // Next to the signature follows Revision Number: 4-byte signed integer,
   // Intel byte-order. Current Revision Number is the value of the
   // IPhFileRevisionNumber constant.
   //
   // The header contents are constant and do not differ between revisions
   // (except Revision Number itself). This allows reliable recognition
   // of newer revisions, with subsequent denial of opening, and also
   // allows to build an unified opening procedure (at least for header).
   //
   // WARNING:
   // All information below refers to Revision 3 (see the header of this file
   // for details). Next sections operate with chunks as data storage blocks.
   //
   // 2.2 General photo album data
   // ----------------------------
   // These data include common photo album data such as album description or
   // thumbnail compression rate, as described by corresponding chunks.
   //
   // 2.3 Pictures and their data as they appear in the phoa storage
   // --------------------------------------------------------------
   // A planar list of pictures, enclosed with the list open/close-chunks
   //
   // 2.4 Groups hierarchy and links to the pictures
   // ----------------------------------------------
   // Hierarchically (recursively) organized group tree, each group maintaining
   // list of child groups as well as list of picture IDs
   //
   // 2.5 Additional album data and structures
   // ----------------------------------------
   // Other photo album data, such as views
   //
   // These sections are pretty relative (except 2.1), and must be overseen in
   // each case.
   //
   // 3. Chunks
   // =========
   // Starting with the section 2.2 (right after header), all phoa-file data
   // are organized with chunks (Revision 3+ !)
   // A CHUNK is an unsigned 2-byte Intel-ordered value, ranged from $0000 to
   // $ffff, each value has its own predetermined meaning (if any), described
   // in IPhChunk_xxxxxx constants. Each chunk has its own, fixed datatype,
   // which can never be changed in later releases. So if you have read chunk's
   // code, you can say for sure what datatype it contains. Moreover, each
   // chunk is followed by one-byte datatype code, and then datatype-specific
   // number of data.
   //
   // So, common chunk structure is as follows:
   //
   // 2 bytes                     Chunk code
   // 1 byte                      Datatype code
   // N bytes (datatype-specific) Chunk data
   //
   // 3.1 Chunk datatypes
   // -------------------
   // Code Name        Data length        Description
   //                    (bytes)
   // ---- --------  -----------------  --------------------------------------------------------------------------------
   //    0 Empty            0           A chunk with no data
   //    1 Byte             1           unsigned 1-byte (0..255)
   //    2 Word             2           unsigned 2-byte (0..65535)
   //    3 Int              4           signed 4-byte (-2147483648..2147483647)
   //    4 StringB      1+(0..255)      Ansi string, with length Byte preceding (max length 255 bytes)
   //    5 StringW     2+(0..65535)     Ansi string (not a *wide* string), with length Word preceding (max length 65 KB)
   //    6 StringI   4+(0..2147483647)  Ansi string, with length Int preceding (max length 2 GB)
   //
   //  * Note on chunk data types:
   //    Only Intel byte-order is used, where $12345678 is represented as byte
   //    sequence: $78,$56,$34,$12.
   //    Each chunk has its own, fixed datatype, which can never be changed in
   //    later releases. Any mismatch with predefined chunk datatype must be
   //    treated as error.
   //
   // 3.2 Chunk codes
   // ---------------
   // According to stated above, chunk code is an unsigned 2-byte number. The
   // datatype and meaning of each chunk is described along with its
   // declaration.
   //
   // Chunk codes $4000..$4fff are open-chunks for nested elements, always
   // having Empty datatype. Each open-chunk has corresponding close-chunk
   // with bit 15 set (ranged $c000..$cfff), eg:
   //   $4000 (open) -> $c000 (close)
   //   $4001 (open) -> $c001 (close)
   // & so on. This allows correct recognition of close-chunks knowing none of
   // the open-chunk purpose.
   //
   // 4. Common phoa-file Reader behaviour
   // ====================================
   // An algorithm designed for reading phoa-files (the Reader) must conform
   // to the following rules:
   //
   // - The Reader reads file header:
   //   o Signature must be exact copy of the PhoA file signature, any mismatch
   //     leads to an error.
   //   o Revision must be 3 or higher for the Reader to treat file as
   //     chunk-based, otherwise this is not chunk-organized phoa-file, and
   //     Reader must implement specific routines to handle it.
   //   o Each specific Revision means possible INCOMPATIBILITIES compared to
   //     lower Revision. So Reader must NOT read Revision it is not intended
   //     to read. Though common file structure would be possibly leaved
   //     intact, there might be some changes in chunk handling so the Reader
   //     can only try to handle such file 'at its own risk'. Generally, this
   //     format was designed just not to be updated frequently. The ability
   //     to handle files with higher revisions is controlled by
   //     StrictRevision conditional define, turn it off to let Reader to
   //     continue with parsing file of 'wrong' revision.
   // - In case Revision is 3 or higher (otherwise see note at the beginning of
   //   this file):
   //   o The Reader processes each chunk one-by-one. The main chunk principle
   //     (and goal) is expansibility, so if the Reader encounters chunk not
   //     known to it, it must just SKIP this chunk whole (along with its
   //     data and/or nested chunks). The purpose of skipping chunk with its
   //     nested chunks is that a chunk cannot be reliably recognized without
   //     its context.
   //     * For known chunk Reader then retrieves its stored datatype code and
   //       does or does not verify this datatype. Generally, mismathing chunk
   //       datatype may rise from broken phoa-files and must be treated as
   //       error.
   //     * If chunk is open-chunk (always with no data, see above), the Reader
   //       handles all subsequent chunks till the terminating close-chunk as
   //       nested (action depends on a particular case).
   //     * If chunk is a data-chunk, it may or may not be processed, depending
   //       on the Reader implementation.
   //
   // Notes:
   //  - The Group IDs introduced in PhoA 1.1.5 aren't supported in previous
   //    versions, and therefore require some fixup. The fixup is that the
   //    Reader should review all the groups once the loading is complete
   //    assigning an unique ID to each group having no assigned one yet.
   //    This workaround doesn't allow for steady referencing groups by ID
   //    (as this ID may change each time the photo album is open, unless
   //    you have saved it by a program version supporting the Group ID chunk),
   //    but it is better than nothing.
   //
   // That is all for now. Yeah, quite long preface :)

{$DEFINE StrictRevision} // If defined, fail opening files with revisions higher than specified in this unit

type
   // Possible chunk datatypes
  TPhChunkDatatype = (pcdEmpty, pcdByte, pcdWord, pcdInt, pcdStringB, pcdStringW, pcdStringI);

  TPhChunkCode = Word;

const
   // Phoa-file signature
  SPhoAFileSignature               = 'PhoA [PhotoAlbum] project file'; // NEVER LOCALIZE!

   // Phoa-files revisions. Revision at index 0 is always the latest one
  aPhFileRevisions: Array[0..2] of record
    iNumber:  Integer; // Phoa-file Revision Number
    sName:    String;  // Name of version family to recognize that revision
    sMinName: String;  // Name of PhoA version introduced that revision
  end = (
    (iNumber: $0003; sName: 'PhoA 1.1+';  sMinName: '1.1.1a'),
    (iNumber: $0002; sName: 'PhoA 1.0.x'; sMinName: '1.0.1a'),
    (iNumber: $0001; sName: 'PhoA 0.x';   sMinName: '0.02b'));

   // Current photo album file Revision Number
  IPhFileRevisionNumber            = $0003;
   // Starting revision for chunk-based handling
  IPhFile_MinChunkRevNumber        = $0003;

   //-------------------------------------------------------------------------------------------------------------------
   // Chunk codes
   //-------------------------------------------------------------------------------------------------------------------
   // Base codes
  IPhChunk_Open_Low                = $4000; // Low bound for all open-chunks
  IPhChunk_Open_High               = $4fff; // High bound for all open-chunks
  IPhChunk_Close_Low               = $c000; // Low bound for all close-chunks
  IPhChunk_Close_High              = $cfff; // High bound for all close-chunks

   // General stuff
  IPhChunk_Remark                  = $0000; // StringW  Any text, ignored by the Reader
   // Common photo album data
  IPhChunk_PhoaGenerator           = $1001; // StringB  Application created the album
  IPhChunk_PhoaSavedDate           = $1002; // Int      Date file saved: number of days since Jan 01, 0001
  IPhChunk_PhoaSavedTime           = $1003; // Int      Time file saved: number of seconds since midnight
  IPhChunk_PhoaDescription         = $1010; // StringW  Photo album description text
  IPhChunk_PhoaThumbQuality        = $1020; // Byte     Photo album thumbnail JPEG-quality level [1..100]
  IPhChunk_PhoaThumbWidth          = $1021; // Word     Photo album thumbnail width  [32..1024]
  IPhChunk_PhoaThumbHeight         = $1022; // Word     Photo album thumbnail height [32..1024]
   // Picture properties
  IPhChunk_Pic_ID                  = $1101; // Int      ID: an unique picture identifier, >=1
  IPhChunk_Pic_ThumbnailData       = $1110; // StringI  Picture thumnail: JPEG data stream
  IPhChunk_Pic_ThumbWidth          = $1111; // Word     Thumbnail width in pixels
  IPhChunk_Pic_ThumbHeight         = $1112; // Word     Thumbnail height in pixels
  IPhChunk_Pic_PicFileName         = $1120; // StringW  Absolute or relative (to the phoa-file) picture filename
  IPhChunk_Pic_PicFileSize         = $1121; // Int      Picture file size, bytes
  IPhChunk_Pic_PicWidth            = $1122; // Int      Image width, pixels
  IPhChunk_Pic_PicHeight           = $1123; // Int      Image height, pixels
  IPhChunk_Pic_PicFormat           = $1124; // Byte     Pixel image format ID (see below)
  IPhChunk_Pic_Date                = $1130; // Int      Date: number of days since Jan 01, 0001
  IPhChunk_Pic_Time                = $1131; // Int      Time: number of seconds since midnight
  IPhChunk_Pic_Place               = $1132; // StringW  Place
  IPhChunk_Pic_FilmNumber          = $1133; // StringW  Film number or name
  IPhChunk_Pic_FrameNumber         = $1134; // StringB  Frame number
  IPhChunk_Pic_Author              = $1135; // StringW  Picture author
  IPhChunk_Pic_Media               = $1136; // StringW  Media name or code
  IPhChunk_Pic_Desc                = $1137; // StringW  Description
  IPhChunk_Pic_Notes               = $1138; // StringW  Notes
  IPhChunk_Pic_Keywords            = $1139; // StringW  Keywords: Comma-separated list. Single entries containing spaces or commas must be double-quoted
  IPhChunk_Pic_Rotation            = $1140; // Byte     Image Rotation ID (see below)
  IPhChunk_Pic_Flips               = $1141; // Byte     Image Flip flags (see below)
   // Picture group properties
  IPhChunk_Group_ID                = $1200; // Int      Group ID
  IPhChunk_Group_Text              = $1201; // StringW  Group text (name)
  IPhChunk_Group_Expanded          = $1202; // Byte     Group-node expanded flag (0/1)
  IPhChunk_Group_Description       = $1203; // StringW  Group description
  IPhChunk_Group_IconData          = $1204; // StringI  Group icon: 32-bit bitmap data stream. When missing or empty, the default icon should be used 
   // Picture linked in group properties
  IPhChunk_GroupPic_ID             = $1220; // Int      Link to picture (the picture's ID)
   // Photo album view properties
  IPhChunk_View_Name               = $1301; // StringW  View name
  IPhChunk_View_FilterExpression   = $1302; // StringW  View picture filter expression
  IPhChunk_ViewGrouping_Prop       = $1310; // Word     View grouping property (see below)
  IPhChunk_ViewGrouping_Unclass    = $1311; // Byte     View grouping switch: place unclassified pictures to a separate folder (0/1)
  IPhChunk_ViewSorting_Prop        = $1320; // Word     View sorting property (see below)
  IPhChunk_ViewSorting_Order       = $1321; // Byte     View sorting sort direction: 0=Ascending, 1=Descending
   // Open-chunks
  IPhChunk_Pics_Open               = $4010; // Empty    Open-chunk for photo album picture list
  IPhChunk_Pic_Open                = $4011; // Empty    Open-chunk for single photo album picture entry (inside the list)
  IPhChunk_Group_Open              = $4020; // Empty    Open-chunk for single picture group (root or nested)
  IPhChunk_Groups_Open             = $4021; // Empty    Open-chunk for nested picture groups
  IPhChunk_GroupPics_Open          = $4030; // Empty    Open-chunk for pics contained in the group
  IPhChunk_Views_Open              = $4050; // Empty    Open-chunk for photo album views
  IPhChunk_View_Open               = $4060; // Empty    Open-chunk for single photo album view
  IPhChunk_ViewGroupings_Open      = $4061; // Empty    Open-chunk for photo album view groupings
  IPhChunk_ViewGrouping_Open       = $4062; // Empty    Open-chunk for single photo album view grouping
  IPhChunk_ViewSortings_Open       = $4063; // Empty    Open-chunk for photo album view sortings
  IPhChunk_ViewSorting_Open        = $4064; // Empty    Open-chunk for single photo album view sorting
   // Close-chunks
  IPhChunk_Pics_Close              = $c010; // Empty    Close-chunk for photo album picture list
  IPhChunk_Pic_Close               = $c011; // Empty    Close-chunk for single photo album picture entry (inside the list)
  IPhChunk_Group_Close             = $c020; // Empty    Close-chunk for single picture group (root or nested)
  IPhChunk_Groups_Close            = $c021; // Empty    Close-chunk for nested picture groups
  IPhChunk_GroupPics_Close         = $c030; // Empty    Close-chunk for pics contained in the group
  IPhChunk_Views_Close             = $c050; // Empty    Close-chunk for photo album views
  IPhChunk_View_Close              = $c060; // Empty    Close-chunk for single photo album view
  IPhChunk_ViewGroupings_Close     = $c061; // Empty    Close-chunk for photo album view groupings
  IPhChunk_ViewGrouping_Close      = $c062; // Empty    Close-chunk for single photo album view grouping
  IPhChunk_ViewSortings_Close      = $c063; // Empty    Close-chunk for photo album view sortings
  IPhChunk_ViewSorting_Close       = $c064; // Empty    Close-chunk for single photo album view sorting

   // Sorting property values
  IPhSortingProp_ID                =  0; // Picture ID
  IPhSortingProp_FileName          =  1; // Picture filename
  IPhSortingProp_FullFileName      =  2; // Picture filename with path
  IPhSortingProp_FilePath          =  3; // Picture file path
  IPhSortingProp_FileSize          =  4; // Picture file size
  IPhSortingProp_FileSizeBytes     =  5; // Picture file size in bytes (for sorting is just the same as 'Picture file size')
  IPhSortingProp_PicWidth          =  6; // Image width
  IPhSortingProp_PicHeight         =  7; // Image height
  IPhSortingProp_PicDims           =  8; // Image dimensions
  IPhSortingProp_Format            =  9; // Pixel format
  IPhSortingProp_Date              = 10; // Date
  IPhSortingProp_Time              = 11; // Time
  IPhSortingProp_Place             = 12; // Place
  IPhSortingProp_FilmNumber        = 13; // Film number
  IPhSortingProp_FrameNumber       = 14; // Frame number
  IPhSortingProp_Author            = 15; // Author
  IPhSortingProp_Description       = 16; // Description
  IPhSortingProp_Notes             = 17; // Notes
  IPhSortingProp_Media             = 18; // Media
  IPhSortingProp_Keywords          = 19; // Keywords (keywords are always ordered alphabetically, case-insensitively)
  IPhSortingProp_Rotation          = 20; // Rotation
  IPhSortingProp_Flips             = 21; // Flips
  IPhSortingProp_ThumbWidth        = 22; // Thumbnail image width
  IPhSortingProp_ThumbHeight       = 23; // Thumbnail image height
  IPhSortingProp_ThumbDims         = 24; // Thumbnail image dimensions

type
   // Local chunk entry, used in aPhChunks[]
  PPhChunkEntry = ^TPhChunkEntry;
  TPhChunkEntry = record
    wCode:     TPhChunkCode;     // Chunk code, one of the IPhChunk_xxxxxx constants
    Datatype:  TPhChunkDatatype; // Predefined chunk datatype
    iRangeMin: Integer;          // Low value limit (for ordinal values)
    iRangeMax: Integer;          // High value limit (for ordinal values)
  end;

   // List of chunks known for the moment, and their datatypes and ranges
const
  aPhChunks: Array[0..62] of TPhChunkEntry = (
    (wCode: IPhChunk_Remark;                 Datatype: pcdStringW),
    (wCode: IPhChunk_PhoaGenerator;          Datatype: pcdStringB),
    (wCode: IPhChunk_PhoaSavedDate;          Datatype: pcdInt;    iRangeMin: 0;  iRangeMax: 3652058 {Dec 31, 9999}),
    (wCode: IPhChunk_PhoaSavedTime;          Datatype: pcdInt;    iRangeMin: 0;  iRangeMax: 24*60*60),
    (wCode: IPhChunk_PhoaDescription;        Datatype: pcdStringW),
    (wCode: IPhChunk_PhoaThumbQuality;       Datatype: pcdByte;   iRangeMin: 1;  iRangeMax: 100),
    (wCode: IPhChunk_PhoaThumbWidth;         Datatype: pcdWord;   iRangeMin: 32; iRangeMax: 1024),
    (wCode: IPhChunk_PhoaThumbHeight;        Datatype: pcdWord;   iRangeMin: 32; iRangeMax: 1024),
    (wCode: IPhChunk_Pic_ID;                 Datatype: pcdInt;    iRangeMin: 1;  iRangeMax: High(Integer)),
    (wCode: IPhChunk_Pic_ThumbnailData;      Datatype: pcdStringI),
    (wCode: IPhChunk_Pic_ThumbWidth;         Datatype: pcdWord;   iRangeMin: 32; iRangeMax: 1024),
    (wCode: IPhChunk_Pic_ThumbHeight;        Datatype: pcdWord;   iRangeMin: 32; iRangeMax: 1024),
    (wCode: IPhChunk_Pic_PicFileName;        Datatype: pcdStringW),
    (wCode: IPhChunk_Pic_PicFileSize;        Datatype: pcdInt;    iRangeMin: 0;  iRangeMax: High(Integer)),
    (wCode: IPhChunk_Pic_PicWidth;           Datatype: pcdInt;    iRangeMin: 0;  iRangeMax: High(Integer)),
    (wCode: IPhChunk_Pic_PicHeight;          Datatype: pcdInt;    iRangeMin: 0;  iRangeMax: High(Integer)),
    (wCode: IPhChunk_Pic_PicFormat;          Datatype: pcdByte;   iRangeMin: 0;  iRangeMax: 8),
    (wCode: IPhChunk_Pic_Date;               Datatype: pcdInt;    iRangeMin: 0;  iRangeMax: 3652058 {Dec 31, 9999}),
    (wCode: IPhChunk_Pic_Time;               Datatype: pcdInt;    iRangeMin: 0;  iRangeMax: 24*60*60),
    (wCode: IPhChunk_Pic_Place;              Datatype: pcdStringW),
    (wCode: IPhChunk_Pic_FilmNumber;         Datatype: pcdStringW),
    (wCode: IPhChunk_Pic_FrameNumber;        Datatype: pcdStringB),
    (wCode: IPhChunk_Pic_Author;             Datatype: pcdStringW),
    (wCode: IPhChunk_Pic_Media;              Datatype: pcdStringW),
    (wCode: IPhChunk_Pic_Desc;               Datatype: pcdStringW),
    (wCode: IPhChunk_Pic_Notes;              Datatype: pcdStringW),
    (wCode: IPhChunk_Pic_Keywords;           Datatype: pcdStringW),
    (wCode: IPhChunk_Pic_Rotation;           Datatype: pcdByte;   iRangeMin: 0;  iRangeMax: 3),
    (wCode: IPhChunk_Pic_Flips;              Datatype: pcdByte;   iRangeMin: 0;  iRangeMax: 3),
    (wCode: IPhChunk_Group_ID;               Datatype: pcdInt;    iRangeMin: 1;  iRangeMax: High(Integer)),
    (wCode: IPhChunk_Group_Text;             Datatype: pcdStringW),
    (wCode: IPhChunk_Group_Expanded;         Datatype: pcdByte;   iRangeMin: 0;  iRangeMax: 1),
    (wCode: IPhChunk_Group_Description;      Datatype: pcdStringW),
    (wCode: IPhChunk_Group_IconData;         Datatype: pcdStringI),
    (wCode: IPhChunk_GroupPic_ID;            Datatype: pcdInt;    iRangeMin: 1;  iRangeMax: High(Integer)),
    (wCode: IPhChunk_View_Name;              Datatype: pcdStringW),
    (wCode: IPhChunk_View_FilterExpression;  Datatype: pcdStringW),
    (wCode: IPhChunk_ViewGrouping_Prop;      Datatype: pcdWord;   iRangeMin: 0;  iRangeMax: 10),
    (wCode: IPhChunk_ViewGrouping_Unclass;   Datatype: pcdByte;   iRangeMin: 0;  iRangeMax: 1),
    (wCode: IPhChunk_ViewSorting_Prop;       Datatype: pcdWord;   iRangeMin: 0;  iRangeMax: 21),
    (wCode: IPhChunk_ViewSorting_Order;      Datatype: pcdByte;   iRangeMin: 0;  iRangeMax: 1),
    (wCode: IPhChunk_Pics_Open;              Datatype: pcdEmpty),
    (wCode: IPhChunk_Pic_Open;               Datatype: pcdEmpty),
    (wCode: IPhChunk_Group_Open;             Datatype: pcdEmpty),
    (wCode: IPhChunk_Groups_Open;            Datatype: pcdEmpty),
    (wCode: IPhChunk_GroupPics_Open;         Datatype: pcdEmpty),
    (wCode: IPhChunk_Views_Open;             Datatype: pcdEmpty),
    (wCode: IPhChunk_View_Open;              Datatype: pcdEmpty),
    (wCode: IPhChunk_ViewGroupings_Open;     Datatype: pcdEmpty),
    (wCode: IPhChunk_ViewGrouping_Open;      Datatype: pcdEmpty),
    (wCode: IPhChunk_ViewSortings_Open;      Datatype: pcdEmpty),
    (wCode: IPhChunk_ViewSorting_Open;       Datatype: pcdEmpty),
    (wCode: IPhChunk_Pics_Close;             Datatype: pcdEmpty),
    (wCode: IPhChunk_Pic_Close;              Datatype: pcdEmpty),
    (wCode: IPhChunk_Group_Close;            Datatype: pcdEmpty),
    (wCode: IPhChunk_Groups_Close;           Datatype: pcdEmpty),
    (wCode: IPhChunk_GroupPics_Close;        Datatype: pcdEmpty),
    (wCode: IPhChunk_Views_Close;            Datatype: pcdEmpty),
    (wCode: IPhChunk_View_Close;             Datatype: pcdEmpty),
    (wCode: IPhChunk_ViewGroupings_Close;    Datatype: pcdEmpty),
    (wCode: IPhChunk_ViewGrouping_Close;     Datatype: pcdEmpty),
    (wCode: IPhChunk_ViewSortings_Close;     Datatype: pcdEmpty),
    (wCode: IPhChunk_ViewSorting_Close;      Datatype: pcdEmpty));

   // Possible values for Pixel Format are (see note below):
   //
   // Value  Default  Description
   // -----  -------  --------------
   //   0             Device-dependent
   //   1             1-bit
   //   2             4-bit
   //   3             8-bit
   //   4             15-bit
   //   5             16-bit
   //   6             24-bit
   //   7             32-bit
   //   8       *     Custom or unknown
   //
   // Possible values for Image Rotation are (see note below):
   //
   // Value  Default  Description
   // -----  -------  --------------
   //   0       *     No rotation (0 degrees)
   //   1             90 degrees clockwise rotation
   //   2             180 degrees rotation
   //   3             270 degrees clockwise rotation (=90 degrees counter-clockwise rotation)
   //
   // Possible values for Image Flip flags are (see note below):
   //
   // Value  Default  Description
   // -----  -------  --------------
   //   0       *     No flips applied
   //   1             Apply horizontal flip to the image
   //   2             Apply vertical flip to the image
   //   3             Apply both horizontal and vertical flips to the image
   //
   // Possible values for Grouping Property are (see note below):
   //
   // Value  Description
   // -----  --------------
   //   0    Picture file path
   //   1    Date year
   //   2    Date month
   //   3    Date day
   //   4    Time hour
   //   5    Time minute
   //   6    Place
   //   7    Film number
   //   8    Author
   //   9    Media name/code
   //  10    Keywords
   //
   // Possible values for Sorting Property are ones declared with IPhSortingProp_XXX constants (see note below):
   //
   // --------
   // * Note on 'enumerated' values: the Reader should IGNORE values with
   //     unknown code, this allows to extend specifications in future. The
   //     'Default' value is just the one used for initializing (may be helpful
   //     in such case), if applicable.

   // TPhoaStreamer status constants
  IPhStatus_OK                     =  0; // Succeeded
  IPhStatus_InvalidMode            =  1; // Invalid opening mode (trying to write in read mode and vice versa)
  IPhStatus_CannotRead             =  2; // Stream read error
  IPhStatus_CannotWrite            =  3; // Stream write error
  IPhStatus_CannotAlterRevision    =  4; // Trying to modify Revision Number after some data have been written
  IPhStatus_InvalidSignature       =  5; // Invalid phoa-file signature
  IPhStatus_NotAChunkedFile        =  6; // File being open is not a chunk-based one, cannot be handled with this unit
  IPhStatus_FileRevNewer           =  7; // File being open has higher revision than possible with this unit (was created with the newer program version)
  IPhStatus_UnknownChunkToWrite    =  8; // Code of a chunk is unknown
  IPhStatus_WrongDatatypePassed    =  9; // Wrong datatype passed to a WriteChunkxxxx() procedure
  IPhStatus_InvalidDatatype        = 10; // Chunk datatype invalid or unknown

type
   //-------------------------------------------------------------------------------------------------------------------
   // Basic implementation of I/O routines (btw used in PhoA)
   //-------------------------------------------------------------------------------------------------------------------

   // Base Exception class
  EPhoaStreamerError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(const Msg: String; iErrCode: Integer);
    constructor CreateFmt(const Msg: String; const Args: Array of const; iErrCode: Integer);
     // Props
     // -- Code of error encountered, one of the IPhStatus_xxxxxx constants
    property ErrorCode: Integer read FErrorCode;
  end;

  TPhoaStreamingMode = (psmRead, psmWrite);

   // Possible ReadChunk result
  TPhReadingChunkResult = (
    rcrOK,               // No errors
    rcrUnknown,          // Chunk code is unknown
    rcrInvalidDatatype,  // Datatype invalid or unknown (out of range)
    rcrDatatypeMismatch, // Datatype mismatch
    rcrEOF);             // No more chunks (end of file encountered)

   // Base class for storing/reading photo album data to/from a stream
  TPhoaStreamer = class(TObject)
  private
     // Prop storage
    FStream: TStream;
    FMode: TPhoaStreamingMode;
    FTransferredBytes: Cardinal;
    FRevisionNumber: Integer;
    FBasePath: String;
    FErrorsOccured: Boolean;
     // Prop handlers
    procedure SetRevisionNumber(Value: Integer);
    function  GetChunked: Boolean;
  protected
     // Writes specified number of bytes from Buffer to the file
    procedure Write(const Buffer; iSize: Integer);
     // Reads specified number of bytes from the file into Buffer
    procedure Read(var Buffer; iSize: Integer);
     // Raises an exception if RequiredMode<>Mode
    procedure CheckMode(RequiredMode: TPhoaStreamingMode);
     // Checking header data validity, virtual to have possibility to alter behaviour in a descendant
    procedure ValidateSignature(const sReadSignature: String); virtual;
    procedure ValidateRevision; virtual;
  public
    constructor Create(AStream: TStream; AMode: TPhoaStreamingMode; const sBasePath: String);
     // Writing/reading routines for typed data
    procedure WriteByte(b: Byte);
    procedure WriteWord(w: Word);
    procedure WriteInt(i: Integer);
    procedure WriteStringB(const s: String);
    procedure WriteStringW(const s: String);
    procedure WriteStringI(const s: String);
    function  ReadByte: Byte;
    function  ReadWord: Word;
    function  ReadInt: Integer;
    function  ReadStringB: String;
    function  ReadStringW: String;
    function  ReadStringI: String;
     // Writes/reads file header
    procedure WriteHeader;
    procedure ReadHeader;
     // Writing/reading chunks (NO DATA are being written/read, only chunk code and datatype!)
     // -- Version with auto-detecting chunk datatype
    procedure WriteChunk(Code: TPhChunkCode); overload;
     // -- Version with forced chunk datatype
    procedure WriteChunk(Code: TPhChunkCode; Datatype: TPhChunkDatatype); overload;
     // -- Version writing typed values (strict datatype)
    procedure WriteChunkByte(Code: TPhChunkCode; b: Byte);
    procedure WriteChunkWord(Code: TPhChunkCode; w: Word);
    procedure WriteChunkInt(Code: TPhChunkCode; i: Integer);
    procedure WriteChunkString(Code: TPhChunkCode; const s: String); // Auto-detecting type
     // -- Reads chunk code and datatype
    function  ReadChunk(out Code: TPhChunkCode; out Datatype: TPhChunkDatatype): TPhReadingChunkResult;
     // -- Reads chunk code, datatype and value. Value is being read only if result is not rcrInvalidDatatype or rcrEOF,
     //    invalid datatype raises exception. bSkipUnknown controls whether to skip unknown chunks (including all nested,
     //    if necessary). bSkipUnmatched controls whether to skip chunks which datatype mismatches from predefined one
    function  ReadChunkValue(out Code: TPhChunkCode; out Datatype: TPhChunkDatatype; var vValue: Variant; bSkipUnknown, bSkipUnmatched: Boolean): TPhReadingChunkResult;
     // Skips all chunks until close-chunk for OpenCode chunk code encountered. May be used for ignoring unknown
     //   open-chunks. It is valid to call SkipNestedChunks for non-open-chunks (ignored if it's the case)
    procedure SkipNestedChunks(OpenCode: TPhChunkCode);
     // Props
     // -- True if chunk-based revision used
    property Chunked: Boolean read GetChunked;
     // -- Photo album file path (for translating relative picture file paths)
    property BasePath: String read FBasePath;
     // -- True if there were errors while reading or writing using the streamer
    property ErrorsOccured: Boolean read FErrorsOccured write FErrorsOccured;
     // -- Mode in which the object was created
    property Mode: TPhoaStreamingMode read FMode;
     // -- PhoA Revision Number, readonly in Read mode; can only be modified before any data are written
    property RevisionNumber: Integer read FRevisionNumber write SetRevisionNumber;
     // -- Stream used for transferring data
    property Stream: TStream read FStream;
     // -- Number of bytes read or written from/to the stream
    property TransferredBytes: Cardinal read FTransferredBytes;
  end;

   // Class for storing/reading photo album data to/from a file
  TPhoaFiler = class(TPhoaStreamer)
  private
     // Real name of the file used to save the data
    FWriteFilename: String;
     // Prop handlers
    FFilename: String;
  public
    constructor Create(AMode: TPhoaStreamingMode; const sFilename: String);
    destructor Destroy; override;
     // Props
     // -- Open file name
    property Filename: String read FFilename;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // Utility routines
   //-------------------------------------------------------------------------------------------------------------------

   // Searches aPhChunks[] for a chunk code, returns pointer to entry if found, otherwise nil.
  function FindChunk(Code: TPhChunkCode): PPhChunkEntry;
   // The same as FindChunk(), but never returns nil; if Code not found, raises exception
  function FindChunkStrict(Code: TPhChunkCode): PPhChunkEntry;
   // Checking whether chunk Code is an open-chunk
  function IsOpenChunk(Code: TPhChunkCode): Boolean;
   // Checking whether chunk Code is close-chunk for chunk OpenCode
  function IsCloseChunk(Code, OpenCode: TPhChunkCode): Boolean;

   // Date and time conversion
   // Converts date into number of days passed since Jan 01, 0001
  function  DateToPhoaDate(const Date: TDateTime): Integer;
   // Converts time into number of seconds passed since midnight
  function  TimeToPhoaTime(const Time: TDateTime): Integer;
   // Converts phoa-date (days since Jan 01, 0001) into TDateTime type
  function  PhoaDateToDate(iDate: Integer): TDateTime;
   // Converts phoa-time (number of seconds since midnight) into TDateTime type
  function  PhoaTimeToTime(iTime: Integer): TDateTime;

resourcestring
   // Error messages
  SPhStreamErr_InvalidMode         = 'Invalid opening mode';
  SPhStreamErr_CannotRead          = 'Cannot read %d bytes from the stream';
  SPhStreamErr_CannotWrite         = 'Cannot write %d bytes to the stream';
  SPhStreamErr_CannotAlterRevision = 'RevisionNumber cannot be modified after some data have been written';
  SPhStreamErr_InvalidSinature     = 'Invalid file signature';
  SPhStreamErr_NotAChunkedFile     = 'File is not chunk-based (old format)';
  SPhStreamErr_FileRevNewer        = 'File was created by the program version newer than this. The file cannot be loaded';
  SPhStreamErr_UnknownChunkToWrite = 'Unknown chunk code to write (%d)';
  SPhStreamErr_WrongDatatypePassed = 'Wrong Datatype of chunk passed to WriteChunkxxxxxx() (%s needed)';
  SPhStreamErr_InvalidDatatype     = 'Chunk datatype invalid or unknown (code: %d)';

implementation
uses Math, Variants;

  function FindChunk(Code: TPhChunkCode): PPhChunkEntry;
  var i: Integer;
  begin
    for i := 0 to High(aPhChunks) do begin
      Result := @aPhChunks[i];
      if Result^.wCode=Code then Exit;
    end;
    Result := nil;
  end;

  function FindChunkStrict(Code: TPhChunkCode): PPhChunkEntry;
  begin
    Result := FindChunk(Code);
    if Result=nil then
      raise EPhoaStreamerError.CreateFmt(SPhStreamErr_UnknownChunkToWrite, [Code], IPhStatus_UnknownChunkToWrite);
  end;

  function IsOpenChunk(Code: TPhChunkCode): Boolean;
  begin
    Result := (Code>=IPhChunk_Open_Low) and (Code<=IPhChunk_Open_High);
  end;

  function IsCloseChunk(Code, OpenCode: TPhChunkCode): Boolean;
  begin
    Result := (Code>=IPhChunk_Close_Low) and (Code<=IPhChunk_Close_High) and (Code=OpenCode or $8000);
  end;

  function DateToPhoaDate(const Date: TDateTime): Integer;
  begin
    Result := Trunc(Date)-Trunc(EncodeDate(0001, 01, 01));
  end;

  function TimeToPhoaTime(const Time: TDateTime): Integer;
  begin
    Result := Trunc(Frac(Time)*24*60*60+0.5);
  end;

  function PhoaDateToDate(iDate: Integer): TDateTime;
  begin
    Result := iDate+EncodeDate(0001, 01, 01);
  end;

  function PhoaTimeToTime(iTime: Integer): TDateTime;
  begin
    Result := Frac(iTime/(24*60*60));
  end;

   // Raises 'Wrong Datatype passed' exception
  procedure WrongWriteChunkDatatype(const sRequiredName: String);
  begin
    raise EPhoaStreamerError.CreateFmt(SPhStreamErr_WrongDatatypePassed, [sRequiredName], IPhStatus_WrongDatatypePassed);
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // EPhoaStreamerError
   //-------------------------------------------------------------------------------------------------------------------

  constructor EPhoaStreamerError.Create(const Msg: String; iErrCode: Integer);
  begin
    inherited Create(Msg);
    FErrorCode := iErrCode;
  end;

  constructor EPhoaStreamerError.CreateFmt(const Msg: String; const Args: Array of const; iErrCode: Integer);
  begin
    inherited CreateFmt(Msg, Args);
    FErrorCode := iErrCode;
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // TPhoaStreamer
   //-------------------------------------------------------------------------------------------------------------------

  procedure TPhoaStreamer.CheckMode(RequiredMode: TPhoaStreamingMode);
  begin
    if FMode<>RequiredMode then raise EPhoaStreamerError.Create(SPhStreamErr_InvalidMode, IPhStatus_InvalidMode);
  end;

  constructor TPhoaStreamer.Create(AStream: TStream; AMode: TPhoaStreamingMode; const sBasePath: String);
  begin
    inherited Create;
    FStream         := AStream;
    FMode           := AMode;
    FBasePath       := sBasePath;
    FRevisionNumber := IPhFileRevisionNumber; // Assume most modern revision by default
  end;

  function TPhoaStreamer.GetChunked: Boolean;
  begin
    Result := FRevisionNumber>=IPhFile_MinChunkRevNumber;
  end;

  procedure TPhoaStreamer.Read(var Buffer; iSize: Integer);
  begin
    try
      CheckMode(psmRead);
      if FStream.Read(Buffer, iSize)<>iSize then
        raise EPhoaStreamerError.CreateFmt(SPhStreamErr_CannotRead, [iSize], IPhStatus_CannotRead);
      Inc(FTransferredBytes, iSize);
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  function TPhoaStreamer.ReadByte: Byte;
  begin
    Read(Result, SizeOf(Result));
  end;

  function TPhoaStreamer.ReadChunk(out Code: TPhChunkCode; out Datatype: TPhChunkDatatype): TPhReadingChunkResult;
  var pe: PPhChunkEntry;
  begin
    try
       // Check if there are data at all
      if FStream.Position>=FStream.Size then begin
        Code     := 0;
        Datatype := pcdEmpty;
        Result   := rcrEOF;
       // Read the chunk and its datatype
      end else begin
        Code     := ReadWord;
        Datatype := TPhChunkDatatype(ReadByte);
         // Try to find a chunk
        pe := FindChunk(Code);
         // Validate Datatype
        if not (Datatype in [Low(Datatype)..High(Datatype)]) then Result := rcrInvalidDatatype
         // If not found
        else if pe=nil then Result := rcrUnknown
         // Compare Datatype
        else if Datatype<>pe.Datatype then Result := rcrDatatypeMismatch
         // Ok
        else Result := rcrOK;
      end;
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  function TPhoaStreamer.ReadChunkValue(out Code: TPhChunkCode; out Datatype: TPhChunkDatatype; var vValue: Variant; bSkipUnknown, bSkipUnmatched: Boolean): TPhReadingChunkResult;
  var pe: PPhChunkEntry;
  begin
    try
      vValue := Null;
      repeat
        Result := ReadChunk(Code, Datatype);
        case Result of
          rcrInvalidDatatype:  raise EPhoaStreamerError.CreateFmt(SPhStreamErr_InvalidDatatype, [Byte(Datatype)], IPhStatus_InvalidDatatype);
          rcrEOF:              Break;
        end;
         // Read the value
        case Datatype of
          pcdByte:    vValue := ReadByte;
          pcdWord:    vValue := ReadWord;
          pcdInt:     vValue := ReadInt;
          pcdStringB: vValue := ReadStringB;
          pcdStringW: vValue := ReadStringW;
          pcdStringI: vValue := ReadStringI;
        end;
         // Validate ranges for ordinal types. Outranged values assume unmatched
        if (Result=rcrOK) and (Datatype in [pcdByte, pcdWord, pcdInt]) then begin
          pe := FindChunk(Code);
          if (vValue<pe.iRangeMin) or (vValue>pe.iRangeMax) then Result := rcrDatatypeMismatch;
        end;
         // Check the chunk for validity
        case Result of
          rcrOK:               Break;
          rcrUnknown:          if not bSkipUnknown then Break;
          rcrDatatypeMismatch: if not bSkipUnmatched then Break;
        end;
         // Chunk is to be skipped, if we're here. Check if it's nested one
        SkipNestedChunks(Code);
      until False;
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.ReadHeader;
  var s: String;
  begin
    try
       // Load and check signature
      SetLength(s, Length(SPhoAFileSignature));
      Read(s[1], Length(s));
      ValidateSignature(s);
       // Read Revision Number
      FRevisionNumber := ReadInt;
      ValidateRevision;
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  function TPhoaStreamer.ReadInt: Integer;
  begin
    Read(Result, SizeOf(Result));
  end;

  function TPhoaStreamer.ReadStringB: String;
  var b: Byte;
  begin
    b := ReadByte;
    SetLength(Result, b);
    Read(Result[1], b);
  end;

  function TPhoaStreamer.ReadStringI: String;
  var i: Integer;
  begin
    i := ReadInt;
    SetLength(Result, i);
    Read(Result[1], i);
  end;

  function TPhoaStreamer.ReadStringW: String;
  var w: Word;
  begin
    w := ReadWord;
    SetLength(Result, w);
    Read(Result[1], w);
  end;

  function TPhoaStreamer.ReadWord: Word;
  begin
    Read(Result, SizeOf(Result));
  end;

  procedure TPhoaStreamer.SetRevisionNumber(Value: Integer);
  begin
    try
      CheckMode(psmWrite);
      if FTransferredBytes>0 then
        raise EPhoaStreamerError.Create(SPhStreamErr_CannotAlterRevision, IPhStatus_CannotAlterRevision);
      FRevisionNumber := Value;
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.SkipNestedChunks(OpenCode: TPhChunkCode);
  var
    wCode: TPhChunkCode;
    Datatype: TPhChunkDatatype;
    vValue: Variant;
  begin
    try
      if IsOpenChunk(OpenCode) then
        repeat
          if ReadChunkValue(wCode, Datatype, vValue, True, True)=rcrEOF then Break;
           // If nested-chunk-structure encountered, recurse it
          SkipNestedChunks(wCode);
        until IsCloseChunk(wCode, OpenCode);
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.ValidateRevision;
  begin
    try
       // Check that we are dealing with chunk-based file
      if not Chunked then raise EPhoaStreamerError.Create(SPhStreamErr_NotAChunkedFile, IPhStatus_NotAChunkedFile);
      {$IFDEF StrictRevision}
       // Check that RevisionNumber is 'normal' for proper handling
      if FRevisionNumber>IPhFileRevisionNumber then
        raise EPhoaStreamerError.Create(SPhStreamErr_FileRevNewer, IPhStatus_FileRevNewer);
      {$ENDIF StrictRevision}
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.ValidateSignature(const sReadSignature: String);
  begin
    try
      if sReadSignature<>SPhoAFileSignature then
        raise EPhoaStreamerError.Create(SPhStreamErr_InvalidSinature, IPhStatus_InvalidSignature);
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.Write(const Buffer; iSize: Integer);
  begin
    try
      CheckMode(psmWrite);
      if FStream.Write(Buffer, iSize)<>iSize then
        raise EPhoaStreamerError.CreateFmt(SPhStreamErr_CannotWrite, [iSize], IPhStatus_CannotWrite);
      Inc(FTransferredBytes, iSize);
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.WriteByte(b: Byte);
  begin
    Write(b, SizeOf(b));
  end;

  procedure TPhoaStreamer.WriteChunk(Code: TPhChunkCode);
  begin
    WriteChunk(Code, FindChunkStrict(Code)^.Datatype);
  end;

  procedure TPhoaStreamer.WriteChunk(Code: TPhChunkCode; Datatype: TPhChunkDatatype);
  begin
    WriteWord(Code);
    WriteByte(Byte(Datatype));
  end;

  procedure TPhoaStreamer.WriteChunkByte(Code: TPhChunkCode; b: Byte);
  var pe: PPhChunkEntry;
  begin
    try
       // Find chunk entry
      pe := FindChunkStrict(Code);
      if pe.Datatype<>pcdByte then WrongWriteChunkDatatype('Byte');
       // Write chunk/datatype code
      WriteChunk(Code, pcdByte);
       // Write chunk data
      WriteByte(b);
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.WriteChunkInt(Code: TPhChunkCode; i: Integer);
  var pe: PPhChunkEntry;
  begin
    try
       // Find chunk entry
      pe := FindChunkStrict(Code);
      if pe.Datatype<>pcdInt then WrongWriteChunkDatatype('Int');
       // Write chunk/datatype code
      WriteChunk(Code, pcdInt);
       // Write chunk data
      WriteInt(i);
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.WriteChunkString(Code: TPhChunkCode; const s: String);
  var pe: PPhChunkEntry;
  begin
    try
       // Find chunk entry
      pe := FindChunkStrict(Code);
      if not (pe.Datatype in [pcdStringB, pcdStringW, pcdStringI]) then WrongWriteChunkDatatype('StringB, StringW or StringI');
       // Write chunk/datatype code
      WriteChunk(Code, pe.Datatype);
       // Write chunk data
      case pe.Datatype of
        pcdStringB: WriteStringB(s);
        pcdStringW: WriteStringW(s);
        pcdStringI: WriteStringI(s);
      end;
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.WriteChunkWord(Code: TPhChunkCode; w: Word);
  var pe: PPhChunkEntry;
  begin
    try
       // Find chunk entry
      pe := FindChunkStrict(Code);
      if pe.Datatype<>pcdWord then WrongWriteChunkDatatype('Word');
       // Write chunk/datatype code
      WriteChunk(Code, pcdWord);
       // Write chunk data
      WriteWord(w);
    except
      FErrorsOccured := True;
      raise;
    end;
  end;

  procedure TPhoaStreamer.WriteHeader;
  var s: String;
  begin
     // Write signature
    s := SPhoAFileSignature;
    Write(s[1], Length(s));
     // Write Revision Number
    WriteInt(FRevisionNumber);
  end;

  procedure TPhoaStreamer.WriteInt(i: Integer);
  begin
    Write(i, SizeOf(i));
  end;

  procedure TPhoaStreamer.WriteStringB(const s: String);
  var b: Byte;
  begin
    b := Min(High(b), Length(s));
    WriteByte(b);
    if b>0 then Write(s[1], b);
  end;

  procedure TPhoaStreamer.WriteStringI(const s: String);
  var i: Integer;
  begin
    i := Length(s);
    WriteInt(i);
    if i>0 then Write(s[1], i);
  end;

  procedure TPhoaStreamer.WriteStringW(const s: String);
  var w: Word;
  begin
    w := Min(High(w), Length(s));
    WriteWord(w);
    if w>0 then Write(s[1], w);
  end;

  procedure TPhoaStreamer.WriteWord(w: Word);
  begin
    Write(w, SizeOf(w));
  end;

   //-------------------------------------------------------------------------------------------------------------------
   // TPhoaFiler
   //-------------------------------------------------------------------------------------------------------------------

  constructor TPhoaFiler.Create(AMode: TPhoaStreamingMode; const sFilename: String);
  var AStream: TStream;
  begin
    try
       // Determine the file to read from or write to and create a file stream
      FFilename := sFilename;
      if AMode=psmWrite then begin
        FWriteFilename := FFilename+'.tmp';
        AStream := TFileStream.Create(FWriteFilename, fmCreate);
      end else
        AStream := TFileStream.Create(FFilename, fmOpenRead or fmShareDenyWrite);
       // Create the streamer
      inherited Create(AStream, AMode, ExtractFilePath(FFilename));
    except
      ErrorsOccured := True;
      raise;
    end;
  end;

  destructor TPhoaFiler.Destroy;
  begin
    Stream.Free;
     // On successful writing, replace the original file with new (.tmp)
    if (Mode=psmWrite) and not ErrorsOccured then begin
      DeleteFile(PChar(FFilename));
      MoveFile(PChar(FWriteFilename), PChar(FFilename));
    end;
    inherited Destroy;
  end;

end.
