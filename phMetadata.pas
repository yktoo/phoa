//**********************************************************************************************************************
//  $Id: phMetadata.pas,v 1.6 2004-06-14 10:32:01 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
//**********************************************************************************************************************
//
// Image Metadata parsing routines
//
// The whole code Copyright ©Dmitry Kann, except where otherwise explicitly noted. The EXIF format is a Japan Electronic
//   Industry Development Association (JEIDA) standard, revised June 1998 as version 2.1, and is not covered by this
//   copyright nor by GPL 2 terms.
//
// Home sites:
//   http://devtools.narod.ru/
//   http://phoa.narod.ru/
//
// Based on info from http://www.ba.wakwak.com/~tsuruzoh/
//
// Change log:
//   Sep 08, 2003 - dale - The first open source release of the unit
//   Sep 17, 2003 - dale - Added some tags. Many tags became documented thanks to JEIDA specifications
//                         Added support for non-regular tags: Unicode, raw binary, Version etc.
//                         The code officially became covered with GPL 2 License, either supplied along with this unit,
//                         or available at http://www.gnu.org/copyleft/gpl.html
//                         PLEASE NOTE:
//                           The GPL 2 License does not permit incorporating this code into proprietary programs. If you
//                           find the unit useful and wish to compile it within a non-opensource code, please contact me
//                           at devtools@narod.ru
//
//   Jun 09, 2004 - dale - Removed dependency from the phUtils unit
//   Jun 11, 2004 - dale - Fixed a stupid error when an IFD with zero component count supplied. Fixed rational value
//                         representation (was presented as a reciprocal before)
//
// Disclaimer:
//   The software included comes with ABSOLUTELY NO WARRANTY. You may use it only at your own risk.
//
//**********************************************************************************************************************
unit phMetadata;

interface
uses SysUtils, Windows, Classes;

type
   // Pointer to Metadata Instance, a specific need in PhoA
  PImageMetadata = ^TImageMetadata;

   // Image Metadata retrieval object
  TImageMetadata = class(TObject)
  private
     // True if any call failed
    FFailed: Boolean;
     // True if any data were fetched from the stream
    FDataFetched: Boolean;
     // Prop storage
    FStatusCode: Integer;
    FEXIFData: TStrings;
     // Loading metadata from the stream
    procedure LoadFromStream(Stream: TStream);
     // Reads a two-byte data from the stream. If bMotorola=True, then swaps the LSB and MSB to provide an
     //   Intel-byte-ordered word
    function  FetchWord(Stream: TStream; bMotorola: Boolean): Word;
     // Reads a marker data size and returns or discards (skips) the marker data from the stream
    function  FetchMarkerData(Stream: TStream; bSkip: Boolean): String;
     // Parses EXIF marker data
    procedure ParseEXIFMarkerData(const sData: String);
     // Sets FStatusCode. Any code but IMS_OK causes FFailed to be set to True
    procedure SetStatusCode(iCode: Integer);
     // Processes an IFD at given offset. bMotorola denotes whether Motorola byte-order is used.
     //   Returns offset to the next IFD, if there's any, else 0
    function ProcessIFD(const sData: String; iOffset: Integer; bMotorola: Boolean): Integer;
  public
    constructor Create(const sFileName: String);
    destructor Destroy; override;
     // Props
    property EXIFData: TStrings read FEXIFData;
     // -- One of the IMS_xxx constants, see below. Can use metadata only if StatusCode = IMS_OK
    property StatusCode: Integer read FStatusCode;
  end;

   // An IFD entry
  PIFDEntry = ^TIFDEntry;
  TIFDEntry = packed record
    wTag:     Word;    // Tag ID
    wFmt:     Word;    // Format (datatype) code
    iCompCnt: Integer; // Component count
    iData:    Integer; // Data or offset to data (if data length>4)
  end;

   // The type of EXIF tag value
  TEXIFTagValueType = (
    xtvtRegular,     // Regular value: numbers are numbers, characters are characters etc.
    xtvtUserComment, // Used for UserComment ($9286): first 8 bytes specify the charset, the next are character codes, whether one- or two-byte (depending on charset)
    xtvtCharVersion, // Data are character codes, each char is separated with a period so they're version number
    xtvtUnicode,     // Data are double-byte Unicode chars
    xtvtBinary);     // Data are raw binary bytes

   // An EXIF tag
  PEXIFTag = ^TEXIFTag; 
  TEXIFTag = record
    iTag:   Integer;           // Image's EXIF tag ID
    VType:  TEXIFTagValueType; // Value type
    sName:  AnsiString;        // Tag name
    sVList: AnsiString;        // Value list in the format: 'value1=name1:value2=name2:...'
    sDesc:  AnsiString;        // Tag description
  end;

const
   // Metadata discrepancies/errors
  IMS_OK                       = 0000; // Metadata successfully parsed
  IMS_CannotOpenFile           = 0001; // Unable to open file (file doesn't exist, access denied etc)
  IMS_ReadFailure              = 0002; // Unable to read from file (EOF encountered, media error, access denied etc)
  IMS_NotAJPEGFile             = 0010; // The file doesn't appear to be a JPEG file
  IMS_NoMetadata               = 0011; // No metadata encountered in the file
  IMS_InvalidEXIFHeader        = 0100; // EXIF header doesn't match
  IMS_InvalidTIFFHeader        = 0101; // TIFF header (inside EXIF header) invalid or missing

  IMS_Unknown                  = 9999; // Unknown or internal error (possibly unhandled exception)

   // IFD Entry's data type
  iedUByte                     = 0001;
  iedAscii                     = 0002;
  iedUShort                    = 0003;
  iedULong                     = 0004;
  iedURational                 = 0005;
  iedSByte                     = 0006;
  iedUndef                     = 0007;
  iedSShort                    = 0008;
  iedSLong                     = 0009;
  iedSRational                 = 0010;
  iedSingleFloat               = 0011;
  iedDoubleFloat               = 0012;

   // IFD Entries' sizes
  aIFDEntSize: Array[1..12] of Integer = (1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8);

   // JPEG Markers
  JM_SOF0                      = $FFC0; // Start Of Frame N, N indicates compression process
  JM_SOF1                      = $FFC1;
  JM_SOF2                      = $FFC2;
  JM_SOF3                      = $FFC3;
  JM_DHT                       = $FFC4; // Define Huffman Table
  JM_SOF5                      = $FFC5;
  JM_SOF6                      = $FFC6;
  JM_SOF7                      = $FFC7;
  JM_SOF9                      = $FFC9;
  JM_SOF10                     = $FFCA;
  JM_SOF11                     = $FFCB;
  JM_SOF13                     = $FFCD;
  JM_DAC                       = $FFCC; // Define arithmetic coding conditioning
  JM_SOF14                     = $FFCE;
  JM_SOF15                     = $FFCF;
  JM_SOI                       = $FFD8; // Start Of Image (beginning of datastream)
  JM_EOI                       = $FFD9; // End Of Image (end of datastream)
  JM_SOS                       = $FFDA; // Start Of Scan (begins compressed data)
  JM_DQT                       = $FFDB; // Define Quantization Table
  JM_DNL                       = $FFDC; // Define Number of Lines
  JM_DRI                       = $FFDD; // Restart Interoperability Definition
  JM_DHP                       = $FFDE; // Define Hierarchical Progression
  JM_EXP                       = $FFDF; // Expand reference component
  JM_JFIF                      = $FFE0; // JFIF marker
  JM_EXIF                      = $FFE1; // EXIF marker
  JM_EXIFEXT                   = $FFE2; // EXIF extended marker
  JM_IPTC                      = $FFED; // IPTC (Photoshop datastream)
  JM_APP14                     = $FFEE; // Photoshop data: App14
  JM_COM                       = $FFFE; // Comment

   // EXIF section header
  SEXIFHeader                  = 'Exif'#00#00;
  STIFFIntelHeader             = 'II'#$2A#00;
  STIFFMotorolaHeader          = 'MM'#00#$2A;

   // EXIF Tags
  EXIF_TAG_SUBIFD              = $8769;
  EXIF_TAG_INTEROPIFD          = $a005;

  EXIF_TAG_DATETIME            = $0132;
  EXIF_TAG_DATETIME_ORIGINAL   = $9003;
  EXIF_TAG_DATETIME_DIGITIZED  = $9004;

  EXIF_TAG_ORIENTATION         = $0112;

   // Some tags are taken from http://lxr.php.net/source/php4/ext/EXIF/EXIF.c, other from
   //   http://www.ba.wakwak.com/~tsuruzoh/, and some from MSDN,
   //   http://msdn.microsoft.com/library/en-us/gdicpp/GDIPlus 
  aEXIFTags: Array[0..256] of TEXIFTag = (
    (iTag: $0001; VType: xtvtRegular;     sName: 'Interoperability Index';               sVList: ''; sDesc: ''),
    (iTag: $0002; VType: xtvtCharVersion; sName: 'Interoperability Version';             sVList: '';
      sDesc: 'Records the interoperability version. "01.00" means version 1.00'),
    (iTag: $000b; VType: xtvtRegular;     sName: 'ACD Comment';                          sVList: ''; sDesc: ''),
    (iTag: $00fe; VType: xtvtRegular;     sName: 'New Subfile Type';                     sVList: '';
      sDesc: 'Type of data in a subfile'),
    (iTag: $00ff; VType: xtvtRegular;     sName: 'Subfile Type';                         sVList: '';
      sDesc: 'Type of data in a subfile'),
    (iTag: $0100; VType: xtvtRegular;     sName: 'Image Width';                          sVList: '';
      sDesc: 'Original image width (number of pixels per row)'),
    (iTag: $0101; VType: xtvtRegular;     sName: 'Image Height';                         sVList: '';
      sDesc: 'Original image height (number of pixel rows)'),
    (iTag: $0102; VType: xtvtRegular;     sName: 'Bits Per Sample';                      sVList: '';
      sDesc:
        'When image format is no compression, this value shows the number of bits per component for each pixel. '+
        'Usually this value is "8,8,8"'),
    (iTag: $0103; VType: xtvtRegular;     sName: 'Compression';                          sVList: '6=JPEG:3=Uncompressed:1=TIFF';
      sDesc: 'Shows compression method used for the image data'),
    (iTag: $0106; VType: xtvtRegular;     sName: 'Photometric Interpretation';           sVList: '1=Monochrome:2=RGB:6=YCbCr';
      sDesc: 'Shows the color space of the image data components'),
    (iTag: $0107; VType: xtvtRegular;     sName: 'ThreshHolding';                        sVList: '';
      sDesc: 'Technique used to convert from gray pixels to black and white pixels'),
    (iTag: $0108; VType: xtvtRegular;     sName: 'PropertyTagCellWidth';                 sVList: '';
      sDesc: 'Width of the dithering or halftoning matrix'),
    (iTag: $0109; VType: xtvtRegular;     sName: 'PropertyTagCellHeight';                sVList: '';
      sDesc: 'Height of the dithering or halftoning matrix'),
    (iTag: $010a; VType: xtvtRegular;     sName: 'FillOrder';                            sVList: '';
      sDesc: 'Logical order of bits in a byte'),
    (iTag: $010d; VType: xtvtRegular;     sName: 'Document Name';                        sVList: '';
      sDesc: 'Specifies the name of the document from which the image was scanned'),
    (iTag: $010e; VType: xtvtRegular;     sName: 'Image Description';                    sVList: '';
      sDesc: 'Specifies the title of the image'),
    (iTag: $010f; VType: xtvtRegular;     sName: 'Make';                                 sVList: '';
      sDesc: 'Shows manufacturer of the equipment used to record the image'),
    (iTag: $0110; VType: xtvtRegular;     sName: 'Model';                                sVList: '';
      sDesc: 'Shows model name or model number of the equipment used to record the image'),
    (iTag: $0111; VType: xtvtRegular;     sName: 'Strip Offsets';                        sVList: '';
      sDesc:
        'When image format is no compression, this value shows offset to image data. In some case image data is '+
        'striped and this value is plural'),
    (iTag: $0112; VType: xtvtRegular;     sName: 'Orientation';
      sVList:
        '1=1 - Portrait (Top Left):2=2 - Portrait (Top Right):3=3 - Portrait (Bottom Right):4=4 - Portrait (Bottom Left):'+
        '5=5 - Landscape (Left Top):6=6 - Landscape (Right Top):7=7 - Landscape (Right Bottom):8=8 - Landscape (Left Bottom)';
      sDesc:
        'The orientation of the camera relative to the scene, when the image was captured. The relation is shown for '+
        'the "0th row" of image to visual position'),
    (iTag: $0115; VType: xtvtRegular;     sName: 'Samples Per Pixel';                    sVList: '';
      sDesc:
        'When image format is no compression, this value shows the number of components stored for each pixel. For '+
        'color image this value is 3'),
    (iTag: $0116; VType: xtvtRegular;     sName: 'Rows Per Strip';                       sVList: '';
      sDesc:
        'When image format is no compression and image has stored as strip, this value shows how many rows stored to '+
        'each strip. If image has not striped, this value is the same as Image Height'),
    (iTag: $0117; VType: xtvtRegular;     sName: 'Strip Byte Counts';                    sVList: '';
      sDesc:
        'When image format is no compression and stored as strip, this value shows how many bytes used for each strip '+
        'and this value is plural. If image has not striped, this value is single and means whole data size of the '+
        'image'),
    (iTag: $0118; VType: xtvtRegular;     sName: 'Min Sample Value';                     sVList: '';
      sDesc: 'For each color component, the minimum value assigned to that component'),
    (iTag: $0119; VType: xtvtRegular;     sName: 'Max Sample Value';                     sVList: '';
      sDesc: 'For each color component, the maximum value assigned to that component'),
    (iTag: $011a; VType: xtvtRegular;     sName: 'X-Resolution';                         sVList: '';
      sDesc:
        'Display/Print resolution of image in the image width (x) direction. Default value is 1/72 inch, but it has '+
        'no meaning because personal computer doesn''t use this value to display/print out'),
    (iTag: $011b; VType: xtvtRegular;     sName: 'Y-Resolution';                         sVList: '';
      sDesc:
        'Display/Print resolution of image in the image height (y) direction. Default value is 1/72 inch, but it has '+
        'no meaning because personal computer doesn''t use this value to display/print out'),
    (iTag: $011c; VType: xtvtRegular;     sName: 'Planar Configuration';                 sVList: '';
      sDesc:
        'When image format is no compression YCbCr, this value shows byte aligns of YCbCr data. If value is 1, '+
        'Y/Cb/Cr value is chunky format, contiguous for each subsampling pixel. If value is 2, Y/Cb/Cr value is '+
        'separated and stored to Y plane/Cb plane/Cr plane format'),
    (iTag: $011d; VType: xtvtRegular;     sName: 'Page Name';                            sVList: '';
      sDesc: 'Specifies the name of the page from which the image was scanned'),
    (iTag: $011e; VType: xtvtRegular;     sName: 'X-Position';                           sVList: '';
      sDesc:
        'Offset from the left side of the page to the left side of the image. The unit of measure is specified by '+
        'Resolution Unit'),
    (iTag: $011f; VType: xtvtRegular;     sName: 'Y-Position';                           sVList: '';
      sDesc:
        'Offset from the top edge of the page to the top edge of the image. The unit of measure is specified by '+
        'Resolution Unit'),
    (iTag: $0120; VType: xtvtRegular;     sName: 'Free Offsets';                         sVList: '';
      sDesc: 'For each string of contiguous unused bytes, the byte offset of that string'),
    (iTag: $0121; VType: xtvtRegular;     sName: 'Free Byte Counts';                     sVList: '';
      sDesc: 'For each string of contiguous unused bytes, the number of bytes in that string'),
    (iTag: $0122; VType: xtvtRegular;     sName: 'Gray Reponse Unit';
      sVList: '1=1/10:2=1/100:3=1/1000:4=1/10000:5=1/100000:6=1/1000000';
      sDesc: 'Precision of the number specified by Gray Response Curve'),
    (iTag: $0123; VType: xtvtRegular;     sName: 'Gray Reponse Curve';                   sVList: '';
      sDesc: 'For each possible pixel value in a grayscale image, the optical density of that pixel value'),
    (iTag: $0124; VType: xtvtRegular;     sName: 'T4 Options';                           sVList: '';
      sDesc: 'Set of flags that relate to T4 encoding'),
    (iTag: $0125; VType: xtvtRegular;     sName: 'T6 Options';                           sVList: '';
      sDesc: 'Set of flags that relate to T6 encoding'),
    (iTag: $0128; VType: xtvtRegular;     sName: 'Resolution Unit';                      sVList: '1=None Specified:2=Inch:3=Centimeter';
      sDesc: 'Unit of measure for the horizontal resolution and the vertical resolution'),
    (iTag: $0129; VType: xtvtRegular;     sName: 'Page Number';                          sVList: '';
      sDesc: 'Page number of the page from which the image was scanned'),
    (iTag: $012d; VType: xtvtRegular;     sName: 'Transfer Function';                    sVList: '';
      sDesc: 'Tables that specify transfer functions for the image'),
    (iTag: $0131; VType: xtvtRegular;     sName: 'Software';                             sVList: '';
      sDesc: 'Specifies the name and version of the software or firmware of the device used to generate the image'),
    (iTag: $0132; VType: xtvtRegular;     sName: 'Date & Time';                          sVList: '';
      sDesc:
        'Date/Time of image was created or last modified. If clock wasn''t set or digicam doesn''t have clock, the '+
        'field may be empty. Usually the same value as Original Date & Time'),
    (iTag: $013b; VType: xtvtRegular;     sName: 'Artist';                               sVList: '';
      sDesc: 'Specifies the name of the person who created the image'),
    (iTag: $013c; VType: xtvtRegular;     sName: 'Host Computer';                        sVList: '';
      sDesc: 'Specifies the computer and/or operating system used to create the image'),
    (iTag: $013d; VType: xtvtRegular;     sName: 'Predictor';                            sVList: '';
      sDesc: 'Type of prediction scheme that was applied to the image data before the encoding scheme was applied'),
    (iTag: $013e; VType: xtvtRegular;     sName: 'White Point';                          sVList: '';
      sDesc:
        'Defines chromaticity of white point of the image. If the image uses CIE Standard Illumination D65 (known as '+
        'international standard of "daylight"), the values are 3127/10000 & 3290/10000'),
    (iTag: $013f; VType: xtvtRegular;     sName: 'Primary Chromaticities';               sVList: '';
      sDesc:
        'Defines chromaticity for each of the three primary colors in the image. If the image uses CCIR '+
        'Recommendation 709 primaries, values are 640/1000, 330/1000, 300/1000, 600/1000, 150/1000, 0/1000'),
    (iTag: $0140; VType: xtvtRegular;     sName: 'Color Map';                            sVList: '';
      sDesc: 'Color palette (lookup table) for a palette-indexed image (consisting of 16-bit words)'),
    (iTag: $0141; VType: xtvtRegular;     sName: 'Half Tone Hints';                      sVList: '';
      sDesc: 'Information used by the halftone function'),
    (iTag: $0142; VType: xtvtRegular;     sName: 'Tile Width';                           sVList: '';
      sDesc: 'Number of pixel columns in each tile'),
    (iTag: $0143; VType: xtvtRegular;     sName: 'Tile Length';                          sVList: '';
      sDesc: 'Number of pixel rows in each tile'),
    (iTag: $0144; VType: xtvtRegular;     sName: 'Tile Offsets';                         sVList: '';
      sDesc: 'For each tile, the byte offset of that tile'),
    (iTag: $0145; VType: xtvtRegular;     sName: 'Tile Byte Counts';                     sVList: '';
      sDesc: 'For each tile, the number of bytes in that tile'),
    (iTag: $014a; VType: xtvtRegular;     sName: 'SubIFDs';                              sVList: ''; sDesc: ''),
    (iTag: $014c; VType: xtvtRegular;     sName: 'Ink Set';                              sVList: '';
      sDesc: 'Set of inks used in a separated image'),
    (iTag: $014d; VType: xtvtRegular;     sName: 'Ink Names';                            sVList: '';
      sDesc:
        'Sequence of concatenated, null-terminated, character strings that specify the names of the inks used in a '+
        'separated image'),
    (iTag: $014e; VType: xtvtRegular;     sName: 'Number Of Inks';                       sVList: '';
      sDesc: 'Number of inks'),
    (iTag: $0150; VType: xtvtRegular;     sName: 'Dot Range';                            sVList: '';
      sDesc: 'Color component values that correspond to a 0 percent dot and a 100 percent dot'),
    (iTag: $0151; VType: xtvtRegular;     sName: 'Target Printer';                       sVList: '';
      sDesc: 'Describes the intended printing environment'),
    (iTag: $0152; VType: xtvtRegular;     sName: 'Extra Sample';                         sVList: '';
      sDesc: 'Number of extra color components. For example, one extra component might hold an alpha value'),
    (iTag: $0153; VType: xtvtRegular;     sName: 'Sample Format';                        sVList: '';
      sDesc: 'For each color component, the numerical format (unsigned, signed, floating point) of that component'),
    (iTag: $0154; VType: xtvtRegular;     sName: 'SMin Sample Value';                    sVList: '';
      sDesc: 'For each color component, the minimum value of that component'),
    (iTag: $0155; VType: xtvtRegular;     sName: 'SMax Sample Value';                    sVList: '';
      sDesc: 'For each color component, the maximum value of that component'),
    (iTag: $0156; VType: xtvtRegular;     sName: 'Transfer Range';                       sVList: '';
      sDesc: 'Table of values that extends the range of the transfer function'),
    (iTag: $0157; VType: xtvtRegular;     sName: 'Clip Path';                            sVList: ''; sDesc: ''),
    (iTag: $0158; VType: xtvtRegular;     sName: 'X-Clip Path Units';                    sVList: ''; sDesc: ''),
    (iTag: $0159; VType: xtvtRegular;     sName: 'Y-Clip Path Units';                    sVList: ''; sDesc: ''),
    (iTag: $015a; VType: xtvtRegular;     sName: 'Indexed';                              sVList: ''; sDesc: ''),
    (iTag: $015b; VType: xtvtRegular;     sName: 'JPEG Tables';                          sVList: ''; sDesc: ''),
    (iTag: $015f; VType: xtvtRegular;     sName: 'OPI Proxy';                            sVList: ''; sDesc: ''),
    (iTag: $0200; VType: xtvtRegular;     sName: 'JPEG Proc';                            sVList: '';
      sDesc: 'When image format is JPEG - the JPEG compression process'),
    (iTag: $0201; VType: xtvtRegular;     sName: 'JPEG Interchange Format';              sVList: '';
      sDesc: 'When image format is JPEG - offset to JPEG bitstream'),
    (iTag: $0202; VType: xtvtRegular;     sName: 'JPEG Interchange Format Length';       sVList: '';
      sDesc: 'When image format is JPEG - length, in bytes, of the JPEG bitstream'),
    (iTag: $0203; VType: xtvtRegular;     sName: 'JPEG Restart Interval';                sVList: '';
      sDesc: 'When image format is JPEG - length of the restart interval'),
    (iTag: $0205; VType: xtvtRegular;     sName: 'JPEG Lossless Predictors';             sVList: '';
      sDesc:
        'When image format is JPEG - for each color component, a lossless predictor-selection value for that component'),
    (iTag: $0206; VType: xtvtRegular;     sName: 'JPEG Point Transforms';                sVList: '';
      sDesc: 'When image format is JPEG - for each color component, a point transformation value for that component'),
    (iTag: $0207; VType: xtvtRegular;     sName: 'JPEG QTables';                         sVList: '';
      sDesc:
        'When image format is JPEG - for each color component, the offset to the quantization table for that component'),
    (iTag: $0208; VType: xtvtRegular;     sName: 'JPEG DCTables';                        sVList: '';
      sDesc:
        'When image format is JPEG - for each color component, the offset to the DC Huffman table (or lossless '+
        'Huffman table) for that component'),
    (iTag: $0209; VType: xtvtRegular;     sName: 'JPEG ACTables';                        sVList: '';
      sDesc:
        'When image format is JPEG - for each color component, the offset to the AC Huffman table for that component'),
    (iTag: $0211; VType: xtvtRegular;     sName: 'YCbCr Coefficients';                   sVList: '';
      sDesc:
        'When image format is YCbCr, this value shows a constant to translate it to RGB format. As usual values are '+
        '0.299, 0.587, 0.114'),
    (iTag: $0212; VType: xtvtRegular;     sName: 'YCbCr Subsampling';                    sVList: '';
      sDesc:
        'When image format is YCbCr and uses subsampling (cropping of chroma data, all digicams do that), shows how '+
        'many chroma data subsampled. First value shows horizontal, next value shows vertical subsample rate'),
    (iTag: $0213; VType: xtvtRegular;     sName: 'YCbCr Positioning';                    sVList: '1=Center pixel:2=Datum point';
      sDesc:
        'When image format is YCbCr and uses subsampling (cropping of chroma data, all digicams do that), defines the '+
        'chroma sample point of subsampling pixel array'),
    (iTag: $0214; VType: xtvtRegular;     sName: 'Reference Black&White';                sVList: '';
      sDesc:
        'Shows reference value of black point and white point. In case of YCbCr format, first 2 show black/white of '+
        'Y, next 2 are Cb, last 2 are Cr. In case of RGB format, first 2 show black/white of R, next 2 are G, last 2 '+
        'are B'),
    (iTag: $02bc; VType: xtvtRegular;     sName: 'Extensible Metadata Platform';         sVList: ''; sDesc: ''),
    (iTag: $0301; VType: xtvtRegular;     sName: 'Gamma';                                sVList: '';
      sDesc:
        'Gamma value attached to the image. The gamma value is stored as a rational number (pair of long) with a '+
        'numerator of 100000. For example, a gamma value of 2.2 is stored as the pair (100000, 45455)'),
    (iTag: $0302; VType: xtvtRegular;     sName: 'ICC Profile Descriptor';               sVList: '';
      sDesc: 'Identifies an ICC profile'),
    (iTag: $0303; VType: xtvtRegular;     sName: 'SRGB Rendering Intent';
      sVList: '0=Perceptual:1=Relative colorimetric:2=Saturation:3=Absolute colorimetric';
      sDesc:
        'How the image should be displayed as defined by the International Color Consortium (ICC):'#13+
        ' • Perceptual intent, which is suitable for photographs, gives good adaptation to the display device gamut '+
        'at the expense of colorimetric accuracy.'#13+
        ' • Relative colorimetric intent is suitable for images (for example, logos) that require color appearance '+
        'matching that is relative to the display device white point.'#13+
        ' • Saturation intent, which is suitable for charts and graphs, preserves saturation at the expense of hue '+
        'and lightness.'#13+
        ' • Absolute colorimetric intent is suitable for proofs (previews of images destined for a different display '+
        'device) that require preservation of absolute colorimetry.'),
    (iTag: $0304; VType: xtvtRegular;     sName: 'Image Title';                          sVList: '';
      sDesc: 'Specifies the title of the image'),
    (iTag: $0320; VType: xtvtRegular;     sName: 'Image Title';                          sVList: '';
      sDesc: 'Specifies the title of the image'),
    (iTag: $1000; VType: xtvtRegular;     sName: 'Related Image File Format';            sVList: '';
      sDesc: 'Records the file format of image file'),
    (iTag: $1001; VType: xtvtRegular;     sName: 'Related Image Width';                  sVList: '';
      sDesc: 'Shows image width'),
    (iTag: $1002; VType: xtvtRegular;     sName: 'Related Image Height';                 sVList: '';
      sDesc: 'Shows image heigth'),
    (iTag: $5001; VType: xtvtRegular;     sName: 'Resolution X-Unit';                    sVList: '1=pixels per inch:2=pixels per centimeter';
      sDesc: 'Units in which to display horizontal resolution'),
    (iTag: $5002; VType: xtvtRegular;     sName: 'Resolution Y-Unit';                    sVList: '1=pixels per inch:2=pixels per centimeter';
      sDesc: 'Units in which to display vertical resolution'),
    (iTag: $5003; VType: xtvtRegular;     sName: 'Resolution X-Length Unit';
      sVList: '1=inches:2=centimeters:3=points:4=picas:5=columns';
      sDesc: 'Units in which to display the image width'),
    (iTag: $5004; VType: xtvtRegular;     sName: 'Resolution Y-Length Unit';
      sVList: '1=inches:2=centimeters:3=points:4=picas:5=columns';
      sDesc: 'Units in which to display the image height'),
    (iTag: $5005; VType: xtvtRegular;     sName: 'Print Flags';                          sVList: '';
      sDesc: 'Sequence of one-byte Boolean values that specify printing options'),
    (iTag: $5006; VType: xtvtRegular;     sName: 'Print Flags Version';                  sVList: ''; 
      sDesc: 'Print flags version'),
    (iTag: $5007; VType: xtvtRegular;     sName: 'Print Flags Crop';                     sVList: ''; 
      sDesc: 'Print flags center crop marks'),
    (iTag: $5008; VType: xtvtRegular;     sName: 'Print Flags Bleed Width';              sVList: ''; 
      sDesc: 'Print flags bleed width'),
    (iTag: $5009; VType: xtvtRegular;     sName: 'Print Flags Bleed Width Scale';        sVList: ''; 
      sDesc: 'Print flags bleed width scale'),
    (iTag: $500a; VType: xtvtRegular;     sName: 'Halftone LPI';                         sVList: ''; 
      sDesc: 'Ink''s screen frequency, in lines per inch'),
    (iTag: $500b; VType: xtvtRegular;     sName: 'Halftone LPI Unit';                    sVList: '1=lines per inch:2=lines per centimeter';
      sDesc: 'Units for the screen frequency'),
    (iTag: $500c; VType: xtvtRegular;     sName: 'Halftone Degree';                      sVList: ''; 
      sDesc: 'Angle for screen'),
    (iTag: $500d; VType: xtvtRegular;     sName: 'Halftone Shape';
      sVList: '0=Round:1=Ellipse:2=Line:3=Square:4=Cross:6=Diamond';
      sDesc: 'Shape of the halftone dots'),
    (iTag: $500e; VType: xtvtRegular;     sName: 'Halftone Misc';                        sVList: '';
      sDesc: 'Miscellaneous halftone information'),
    (iTag: $500f; VType: xtvtRegular;     sName: 'Halftone Screen';                      sVList: '1=Use printer''s default screens:2=Other';
      sDesc: 'Boolean value that specifies whether to use the printer''s default screens'),
    (iTag: $5010; VType: xtvtRegular;     sName: 'JPEG Quality';                         sVList: '';
      sDesc: 'Private tag used by the Adobe Photoshop format. Not for public use'),
    (iTag: $5011; VType: xtvtRegular;     sName: 'Grid Size';                            sVList: '';
      sDesc: 'Block of information about grids and guides'),
    (iTag: $5012; VType: xtvtRegular;     sName: 'Thumbnail Format';                     sVList: '0=Raw RGB:1=JPEG';
      sDesc: 'Format of the thumbnail image'),
    (iTag: $5013; VType: xtvtRegular;     sName: 'Thumbnail Width';                      sVList: '';
      sDesc: 'Width, in pixels, of the thumbnail image'),
    (iTag: $5014; VType: xtvtRegular;     sName: 'Thumbnail Height';                     sVList: '';
      sDesc: 'Height, in pixels, of the thumbnail image'),
    (iTag: $5015; VType: xtvtRegular;     sName: 'Thumbnail ColorDepth';                 sVList: '';
      sDesc: 'Bits per pixel (BPP) for the thumbnail image'),
    (iTag: $5016; VType: xtvtRegular;     sName: 'Thumbnail Planes';                     sVList: '';
      sDesc: 'Number of color planes for the thumbnail image'),
    (iTag: $5017; VType: xtvtRegular;     sName: 'Thumbnail Raw Bytes';                  sVList: '';
      sDesc: 'Byte offset between rows of pixel data'),
    (iTag: $5018; VType: xtvtRegular;     sName: 'Thumbnail Size';                       sVList: '';
      sDesc: 'Total size, in bytes, of the thumbnail image'),
    (iTag: $5019; VType: xtvtRegular;     sName: 'Thumbnail CompressedSize';             sVList: '';
      sDesc: 'Compressed size, in bytes, of the thumbnail image'),
    (iTag: $501a; VType: xtvtRegular;     sName: 'Color Transfer Function';              sVList: '';
      sDesc: 'Table of values that specify color transfer functions'),
    (iTag: $501b; VType: xtvtRegular;     sName: 'Thumbnail Data';                       sVList: '';
      sDesc: 'Raw thumbnail bits in JPEG or RGB format. Depends on Thumbnail Format'),
    (iTag: $5020; VType: xtvtRegular;     sName: 'Thumbnail Image Width';                sVList: '';
      sDesc: 'Number of pixels per row in the thumbnail image'),
    (iTag: $5021; VType: xtvtRegular;     sName: 'Thumbnail Image Height';               sVList: '';
      sDesc: 'Number of pixel rows in the thumbnail image'),
    (iTag: $5022; VType: xtvtRegular;     sName: 'Thumbnail Bits Per Sample';            sVList: '';
      sDesc: 'Number of bits per color component in the thumbnail image'),
    (iTag: $5023; VType: xtvtRegular;     sName: 'Thumbnail Compression';                sVList: '';
      sDesc: 'Compression method used for thumbnail image data'),
    (iTag: $5024; VType: xtvtRegular;     sName: 'Thumbnail Photometric Interpretation'; sVList: '';
      sDesc: 'How thumbnail pixel data will be interpreted'),
    (iTag: $5025; VType: xtvtRegular;     sName: 'Thumbnail Image Description';          sVList: '';
      sDesc: 'Specifies the title of the image'),
    (iTag: $5026; VType: xtvtRegular;     sName: 'Thumbnail Equipment Make';             sVList: '';
      sDesc: 'Specifies the manufacturer of the equipment used to record the thumbnail image'),
    (iTag: $5027; VType: xtvtRegular;     sName: 'Thumbnail Equipment Model';            sVList: '';
      sDesc: 'Specifies the model name or model number of the equipment used to record the thumbnail image'),
    (iTag: $5028; VType: xtvtRegular;     sName: 'Thumbnail Strip Offsets';              sVList: '';
      sDesc: 'For each strip in the thumbnail image, the byte offset of that strip'),
    (iTag: $5029; VType: xtvtRegular;     sName: 'Thumbnail Orientation';
      sVList:
        '1=Portrait (Top Left):2=Portrait (Top Right):3=Portrait (Bottom Right):4=Portrait (Bottom Left):'+
        '5=Landscape (Left Top):6=Landscape (Right Top):7=Landscape (Right Bottom):8=Landscape (Left Bottom)';
      sDesc: 'Thumbnail image orientation'),
    (iTag: $502a; VType: xtvtRegular;     sName: 'Thumbnail Samples Per Pixel';          sVList: '';
      sDesc: 'Number of color components per pixel in the thumbnail image'),
    (iTag: $502b; VType: xtvtRegular;     sName: 'Thumbnail Rows Per Strip';             sVList: '';
      sDesc: 'Number of rows per strip in the thumbnail image'),
    (iTag: $502c; VType: xtvtRegular;     sName: 'Thumbnail Strip Bytes Count';          sVList: '';
      sDesc: 'For each thumbnail image strip, the total number of bytes in that strip'),
    (iTag: $502d; VType: xtvtRegular;     sName: 'Thumbnail X-Resolution';               sVList: '';
      sDesc: 'Thumbnail resolution in the width direction. The resolution unit is given in Thumbnail Resolution Unit'),
    (iTag: $502e; VType: xtvtRegular;     sName: 'Thumbnail Y-Resolution';               sVList: '';
      sDesc: 'Thumbnail resolution in the height direction. The resolution unit is given in Thumbnail Resolution Unit'),
    (iTag: $502f; VType: xtvtRegular;     sName: 'Thumbnail Planar Configuration';       sVList: '';
      sDesc: 'Whether pixel components in the thumbnail image are recorded in chunky or planar format'),
    (iTag: $5030; VType: xtvtRegular;     sName: 'Thumbnail Resolution Unit';            sVList: '1=None Specified:2=Inch:3=Centimeter';
      sDesc: 'Unit of measure for the horizontal resolution and the vertical resolution of the thumbnail image'),
    (iTag: $5031; VType: xtvtRegular;     sName: 'Thumbnail Transfer Function';          sVList: ''; 
      sDesc: 'Tables that specify transfer functions for the thumbnail image'),
    (iTag: $5032; VType: xtvtRegular;     sName: 'Thumbnail Software Used';              sVList: '';
      sDesc:
        'Specifies the name and version of the software or firmware of the device used to generate the thumbnail image'),
    (iTag: $5033; VType: xtvtRegular;     sName: 'Thumbnail Date & Time';                sVList: ''; 
      sDesc: 'Date and time the thumbnail image was created or last modified'),
    (iTag: $5034; VType: xtvtRegular;     sName: 'Thumbnail Artist';                     sVList: ''; 
      sDesc: 'Specifies the name of the person who created the thumbnail image'),
    (iTag: $5035; VType: xtvtRegular;     sName: 'Thumbnail White Point';                sVList: ''; 
      sDesc: 'Chromaticity of the white point of the thumbnail image'),
    (iTag: $5036; VType: xtvtRegular;     sName: 'Thumbnail Primary Chromatics';         sVList: ''; 
      sDesc: 'For each of the three primary colors in the thumbnail image, the chromaticity of that color'),
    (iTag: $5037; VType: xtvtRegular;     sName: 'Thumbnail YCbCr Coefficients';         sVList: '';
      sDesc: 'Coefficients for transformation from RGB to YCbCr data for the thumbnail image'),
    (iTag: $5038; VType: xtvtRegular;     sName: 'Thumbnail YCbCr Subsampling';          sVList: ''; 
      sDesc: 'Sampling ratio of chrominance components in relation to the luminance component for the thumbnail image'),
    (iTag: $5039; VType: xtvtRegular;     sName: 'Thumbnail YCbCr Positioning';          sVList: ''; 
      sDesc: 'Position of chrominance components in relation to the luminance component for the thumbnail image'),
    (iTag: $503a; VType: xtvtRegular;     sName: 'Thumbnail Ref Black&White';            sVList: ''; 
      sDesc: 'Reference black point value and reference white point value for the thumbnail image'),
    (iTag: $503b; VType: xtvtRegular;     sName: 'Thumbnail Copyright';                  sVList: ''; 
      sDesc: 'Contains copyright information for the thumbnail image'),
    (iTag: $5090; VType: xtvtRegular;     sName: 'Luminance Table';                      sVList: ''; 
      sDesc:
        'Luminance table. The luminance table and the chrominance table are used to control JPEG quality. '+
        'A valid luminance or chrominance table has 64 entries. If an image has either a luminance table or a '+
        'chrominance table, then it must have both tables'),
    (iTag: $5091; VType: xtvtRegular;     sName: 'Chrominance Table';                    sVList: '';
      sDesc:
        'Chrominance table. The luminance table and the chrominance table are used to control JPEG quality. '+
        'A valid luminance or chrominance table has 64 entries. If an image has either a luminance table or a '+
        'chrominance table, then it must have both tables'),
    (iTag: $5100; VType: xtvtRegular;     sName: 'Frame Delay';                          sVList: ''; 
      sDesc: 'Time delay, in hundredths of a second, between two frames in an animated GIF image'),
    (iTag: $5101; VType: xtvtRegular;     sName: 'Loop Count';                           sVList: '';
      sDesc:
        'For an animated GIF image, the number of times to display the animation. A value of 0 specifies that the '+
        'animation should be displayed infinitely'),
    (iTag: $5102; VType: xtvtRegular;     sName: 'Global Palette';                       sVList: '';
      sDesc: 'Color palette for an indexed bitmap in a GIF image'),
    (iTag: $5103; VType: xtvtRegular;     sName: 'Index Background';                     sVList: '';
      sDesc: 'Index of the background color in the palette of a GIF image'),
    (iTag: $5104; VType: xtvtRegular;     sName: 'IndexTransparent';                     sVList: '';
      sDesc: 'Index of the transparent color in the palette of a GIF image'),
    (iTag: $5110; VType: xtvtRegular;     sName: 'Pixel Unit';                           sVList: '0=Unknown';
      sDesc: 'Unit for Pixel Per Unit (X) and Pixel Per Unit (Y)'),
    (iTag: $5111; VType: xtvtRegular;     sName: 'Pixel Per Unit (X)';                   sVList: ''; 
      sDesc: 'Pixels per unit in the x direction'),
    (iTag: $5112; VType: xtvtRegular;     sName: 'Pixel Per Unit (Y)';                   sVList: '';
      sDesc: 'Pixels per unit in the y direction'),
    (iTag: $5113; VType: xtvtRegular;     sName: 'Palette Histogram';                    sVList: ''; 
      sDesc: 'Palette histogram'),
    (iTag: $800d; VType: xtvtRegular;     sName: 'Image ID';                             sVList: ''; sDesc: ''),
    (iTag: $80e3; VType: xtvtRegular;     sName: 'Matteing';                             sVList: ''; sDesc: ''),
    (iTag: $80e4; VType: xtvtRegular;     sName: 'Data Type';                            sVList: ''; sDesc: ''),
    (iTag: $80e5; VType: xtvtRegular;     sName: 'Image Depth';                          sVList: ''; sDesc: ''),
    (iTag: $80e6; VType: xtvtRegular;     sName: 'Tile Depth';                           sVList: ''; sDesc: ''),
    (iTag: $828d; VType: xtvtRegular;     sName: 'CFA Repeat Pattern Dim';               sVList: ''; sDesc: ''),
    (iTag: $828e; VType: xtvtRegular;     sName: 'CFA Pattern';                          sVList: ''; sDesc: ''),
    (iTag: $828f; VType: xtvtRegular;     sName: 'Battery Level';                        sVList: ''; sDesc: ''),
    (iTag: $8298; VType: xtvtRegular;     sName: 'Copyright';                            sVList: '';
      sDesc: 'Contains copyright information'),
    (iTag: $829a; VType: xtvtRegular;     sName: 'Exposure Time (sec)';                  sVList: '';
      sDesc: 'Exposure time (reciprocal of shutter speed). Unit is second'),
    (iTag: $829d; VType: xtvtRegular;     sName: 'F-Number';                             sVList: '';
      sDesc: 'The actual F-Number (F-stop) of lens when the image was taken'),
    (iTag: $83bb; VType: xtvtRegular;     sName: 'IPTC/NAA';                             sVList: ''; sDesc: ''),
    (iTag: $84e3; VType: xtvtRegular;     sName: 'IT8 Raster Padding';                   sVList: ''; sDesc: ''),
    (iTag: $84e5; VType: xtvtRegular;     sName: 'IT8 Color Table';                      sVList: ''; sDesc: ''),
    (iTag: $8649; VType: xtvtRegular;     sName: 'Image Resource Information';           sVList: ''; sDesc: ''),
    (iTag: $8769; VType: xtvtRegular;     sName: 'EXIF SubIFD Offset';                   sVList: '';
      sDesc: 'File offset to EXIF SubIFD (Image File Directory)'),
    (iTag: $8773; VType: xtvtRegular;     sName: 'ICC Profile';                          sVList: '';
      sDesc: 'ICC profile embedded in the image'),
    (iTag: $8822; VType: xtvtRegular;     sName: 'Exposure Program';
      sVList:
        '0=Unidentified:1=Manual:2=Normal:3=Aperture priority:4=Shutter priority:'+
        '5=Creative (biased toward depth of field):6=Action (biased toward fast shutter speed):'+
        '7=Portrait mode (for close-up photos with the background out of focus):'+
        '8=Landscape mode (for landscape photos with the background in focus)';
      sDesc: 'Exposure program that the camera used when image was taken'),
    (iTag: $8824; VType: xtvtRegular;     sName: 'Spectral Sensitivity';                 sVList: '';
      sDesc:
        'Specifies the spectral sensitivity of each channel of the camera used. The string is compatible with the '+
        'standard developed by the ASTM Technical Committee'),
    (iTag: $8825; VType: xtvtRegular;     sName: 'GPS IFD Offset';                       sVList: '';
      sDesc: 'Offset to a block of GPS property items'),
    (iTag: $8827; VType: xtvtRegular;     sName: 'ISO Speed Ratings';                    sVList: '';
      sDesc: 'ISO speed and ISO latitude of the camera or input device as specified in ISO 12232'),
    (iTag: $8828; VType: xtvtRegular;     sName: 'OECF';                                 sVList: '';
      sDesc:
        'Optoelectronic conversion function (OECF) specified in ISO 14524. The OECF is the relationship between the '+
        'camera optical input and the image values'),
    (iTag: $8829; VType: xtvtRegular;     sName: 'Interlace';                            sVList: ''; sDesc: ''),
    (iTag: $882a; VType: xtvtRegular;     sName: 'Time Zone Offset';                     sVList: ''; sDesc: ''),
    (iTag: $882b; VType: xtvtRegular;     sName: 'Self Timer Mode';                      sVList: ''; sDesc: ''),
    (iTag: $9000; VType: xtvtCharVersion; sName: 'EXIF Version';                         sVList: '';
      sDesc:
        'Version of the EXIF standard supported. Nonexistence of this field is taken to mean nonconformance to the '+
        'standard. Conformance to the standard EXIF 2.1 is indicated by recording values 02.10'),
    (iTag: $9003; VType: xtvtRegular;     sName: 'Original Date & Time';                 sVList: '';
      sDesc:
        'Date/Time of original image taken. This value should not be modified by user program. If clock wasn''t set '+
        'or digicam doesn''t have clock, the field may be empty'),
    (iTag: $9004; VType: xtvtRegular;     sName: 'Date & Time Digitized';                sVList: '';
      sDesc:
        'Date/Time of when the image was digitized. If clock wasn''t set or digicam doesn''t have clock, the field '+
        'may be empty. Usually the same value as Original Date & Time'),
    (iTag: $9101; VType: xtvtRegular;     sName: 'Components Configuration';             sVList: '';
      sDesc:
        'Information specific to compressed data. The channels of each component are arranged in order from the first '+
        'component to the fourth. For uncompressed data, the data arrangement is given in the Photometric '+
        'Interpretation tag. However, because Photometric Interpretation can only express the order of Y, Cb, and Cr, '+
        'this tag is provided for cases when compressed data uses components other than Y, Cb, and Cr and to support '+
        'other sequences. Most of case "0x04,0x05,0x06,0x00" is used for RGB-format and "0x01,0x02,0x03,0x00" for '+
        'YCbCr-format. 0x00:does not exist, 0x01:Y, 0x02:Cb, 0x03:Cr, 0x04:Red, 0x05:Green, 0x06:Blue'),
    (iTag: $9102; VType: xtvtRegular;     sName: 'Compressed Bits Per Pixel';            sVList: '';
      sDesc: 'Information specific to compressed data. The average compression ratio of JPEG (rough estimate)'),
    (iTag: $9201; VType: xtvtRegular;     sName: 'Shutter Speed Value';                  sVList: '';
      sDesc:
        'Shutter speed. The unit is the Additive System of Photographic Exposure (APEX) value. To convert this value '+
        'to ordinary "Shutter Speed", calculate this value''s power of 2, then reciprocal. For example, if the value '+
        'is 4, shutter speed is 1/16 second'),
    (iTag: $9202; VType: xtvtRegular;     sName: 'Aperture Value';                       sVList: '';
      sDesc:
        'The actual aperture value of lens when the image was taken. The unit is the Additive System of Photographic '+
        'Exposure (APEX) value. To convert this value to ordinary F-Number (F-Stop), calculate this value''s power of '+
        'root 2 (=1.4142). For example, if the value is 5, F-Number is 1.4142^5 = F5.6'),
    (iTag: $9203; VType: xtvtRegular;     sName: 'Brightness Value';                     sVList: '';
      sDesc:
        'Brightness of taken subject. The unit is the Additive System of Photographic Exposure (APEX) value. To '+
        'calculate Exposure (Ev) from Brigtness Value (Bv), you must add Sensitivity Value (Sv). Ev=Bv+Sv; '+
        'Sv=log2(ISOSpeedRating/3.125); ISO100:Sv=5, ISO200:Sv=6, ISO400:Sv=7, ISO125:Sv=5.32'),
    (iTag: $9204; VType: xtvtRegular;     sName: 'Exposure Bias Value';                  sVList: '';
      sDesc:
        'Exposure bias (compensation) value of taking picture. The unit is the Additive System of Photographic '+
        'Exposure (APEX) value. Ordinarily it is given in the range -99.99 to 99.99'),
    (iTag: $9205; VType: xtvtRegular;     sName: 'Max Aperture Value';                   sVList: '';
      sDesc:
        'Maximum aperture value of lens (smallest F-number of the lens). You can convert to F-Number by calculating '+
        'power of root 2 (same as Aperture Value)'),
    (iTag: $9206; VType: xtvtRegular;     sName: 'Subject Distance (m)';                 sVList: '';
      sDesc: 'Distance to focus point, unit is meter'),
    (iTag: $9207; VType: xtvtRegular;     sName: 'Metering Mode';
      sVList: '0=Unknown:1=Average:2=Center weighted average:3=Spot:4=Multi-spot:5=Multi-segment:6=Partial:255=Other';
      sDesc: 'Exposure metering method'),
    (iTag: $9208; VType: xtvtRegular;     sName: 'Light Source';
      sVList:
        '0=Unidentified:1=Daylight:2=Fluorescent:3=Tungsten:10=Flash:17=Standard light A:18=Standard light B:'+
        '19=Standard light C:20=D55:21=D65:22=D75:255=Other';
      sDesc: 'Light source; actually this means white balance setting'),
    (iTag: $9209; VType: xtvtRegular;     sName: 'Flash';
      sVList:
        '0=Flash didn''t fire:1=Flash fired:5=Flash fired, strobe return light wasn''t detected:'+
        '7=Flash fired, strobe return light detected';
      sDesc: 'Flash usage while taking the picture'),
    (iTag: $920a; VType: xtvtRegular;     sName: 'Focal Length (mm)';                    sVList: '';
      sDesc:
        'Actual focal length of lens used to take image. Unit is millimeter. Conversion is not made to the focal '+
        'length of a 35 millimeter film camera'),
    (iTag: $920b; VType: xtvtRegular;     sName: 'Flash Energy';                         sVList: ''; sDesc: ''),
    (iTag: $920c; VType: xtvtRegular;     sName: 'Spatial Frequency Response';           sVList: ''; sDesc: ''),
    (iTag: $920d; VType: xtvtRegular;     sName: 'Noise';                                sVList: ''; sDesc: ''),
    (iTag: $920e; VType: xtvtRegular;     sName: 'Focal Plane X-Resolution';             sVList: ''; sDesc: ''),
    (iTag: $920f; VType: xtvtRegular;     sName: 'Focal Plane Y-Resolution';             sVList: ''; sDesc: ''),
    (iTag: $9210; VType: xtvtRegular;     sName: 'Focal Plane Resolution Unit';
      sVList: '1=None Specified:2=Inch:3=Centimeter';
      sDesc: ''),
    (iTag: $9211; VType: xtvtRegular;     sName: 'Image Number';                         sVList: ''; sDesc: ''),
    (iTag: $9212; VType: xtvtRegular;     sName: 'Security Classification';              sVList: ''; sDesc: ''),
    (iTag: $9213; VType: xtvtRegular;     sName: 'Image History';                        sVList: ''; sDesc: ''),
    (iTag: $9214; VType: xtvtRegular;     sName: 'Subject Location';                     sVList: ''; sDesc: ''),
    (iTag: $9215; VType: xtvtRegular;     sName: 'Exposure Index';                       sVList: ''; sDesc: ''),
    (iTag: $9216; VType: xtvtRegular;     sName: 'TIFF/EPStandard ID';                   sVList: ''; sDesc: ''),
    (iTag: $9217; VType: xtvtRegular;     sName: 'Sensing Method';                       sVList: ''; sDesc: ''),
    (iTag: $923f; VType: xtvtRegular;     sName: 'StoNits';                              sVList: ''; sDesc: ''),
    (iTag: $927c; VType: xtvtBinary;      sName: 'Maker Note';                           sVList: '';
      sDesc: 'Maker-dependent internal data'),
    (iTag: $9286; VType: xtvtUserComment; sName: 'User Comment';                         sVList: '';
      sDesc:
        'Comment tag. A tag used by EXIF users to write keywords or comments about the image besides those in '+
        'Image Description and without the character-code limitations of the Image Description tag'),
    (iTag: $9290; VType: xtvtRegular;     sName: 'Sub Sec Time';                         sVList: '';
      sDesc:
        'Some of digicam can take 2 to 30 pictures per second, but "Date & Time" tag cannot record the sub-second '+
        'time. This tag is used to record it'),
    (iTag: $9291; VType: xtvtRegular;     sName: 'Sub Sec Time Original';                sVList: '';
      sDesc:
        'Some of digicam can take 2 to 30 pictures per second, but "Original Date & Time" tag cannot record the '+
        'sub-second time. This tag is used to record it'),
    (iTag: $9292; VType: xtvtRegular;     sName: 'Sub Sec Time Digitized';               sVList: '';
      sDesc:
        'Some of digicam can take 2 to 30 pictures per second, but "Date & Time Digitized" tag cannot record the '+
        'sub-second time. This tag is used to record it'),
    (iTag: $953c; VType: xtvtRegular;     sName: 'Image Source Data';                    sVList: ''; sDesc: ''),
    (iTag: $9c9b; VType: xtvtUnicode;     sName: 'Title';                                sVList: ''; sDesc: ''),
    (iTag: $9c9c; VType: xtvtUnicode;     sName: 'Comments';                             sVList: ''; sDesc: ''),
    (iTag: $9c9d; VType: xtvtUnicode;     sName: 'Author';                               sVList: ''; sDesc: ''),
    (iTag: $9c9e; VType: xtvtUnicode;     sName: 'Keywords';                             sVList: ''; sDesc: ''),
    (iTag: $9c9f; VType: xtvtUnicode;     sName: 'Subject';                              sVList: ''; sDesc: ''),
    (iTag: $a000; VType: xtvtCharVersion; sName: 'FlashPix Version';                     sVList: '';
      sDesc:
        'FlashPix format version supported by an FPXR file. If the image data is based on FlashPix format Ver.1.0, '+
        'value is "01.00"'),
    (iTag: $a001; VType: xtvtRegular;     sName: 'Color Space';                          sVList: '0=sBW:1=sRGB:65535=Uncalibrated';
      sDesc:
        'Defines Color Space. DCF image must use sRGB color space. If the picture uses the other color space, value '+
        'is Uncalibrated. Image data recorded as Uncalibrated can be treated as sRGB when it is converted to FlashPix'),
    (iTag: $a002; VType: xtvtRegular;     sName: 'EXIF Image Width';                     sVList: '';
      sDesc:
        'Information specific to compressed data. When a compressed file is recorded, the valid width of the '+
        'meaningful image must be recorded in this tag, whether or not there is padding data or a restart marker. '+
        'This tag should not exist in an uncompressed file'),
    (iTag: $a003; VType: xtvtRegular;     sName: 'EXIF Image Height';                    sVList: '';
      sDesc:
        'Information specific to compressed data. When a compressed file is recorded, the valid height of the '+
        'meaningful image must be recorded in this tag whether or not there is padding data or a restart marker. '+
        'This tag should not exist in an uncompressed file. Because data padding is unnecessary in the vertical '+
        'direction, the number of lines recorded in this valid image height tag will be the same as that recorded in '+
        'the SOF'),
    (iTag: $a004; VType: xtvtRegular;     sName: 'Related Sound File';                   sVList: '';
      sDesc:
        'The name of an audio file related to the image data. The only relational information recorded is the EXIF '+
        'audio file name and extension (an ASCII string that consists of 8 characters plus a period (.), plus 3 '+
        'characters). The path is not recorded. When you use this tag, audio files must be recorded in conformance '+
        'with the EXIF audio format. Writers can also store audio data within APP2 as FlashPix extension stream data'),
    (iTag: $a005; VType: xtvtRegular;     sName: 'InteroperabilityIFD Offset';           sVList: '';
      sDesc: 'Extension of "ExifR98", details are unknown. This value is offset to IFD format data'),
    (iTag: $a20b; VType: xtvtRegular;     sName: 'Flash Energy';                         sVList: '';
      sDesc: 'Strobe energy, in Beam Candle Power Seconds (BCPS), at the time the image was captured'),
    (iTag: $a20c; VType: xtvtRegular;     sName: 'Spatial Frequency Response';           sVList: '';
      sDesc:
        'Camera or input device spatial frequency table and SFR values in the image width, image height, and '+
        'diagonal direction, as specified in ISO 12233'),
    (iTag: $a20e; VType: xtvtRegular;     sName: 'Focal Plane X-Resolution';             sVList: '';
      sDesc:
        'Pixel density at CCD''s position. If you have MegaPixel digicam and take a picture by lower resolution '+
        '(e.g. VGA mode), this value is re-sampled by picture resolution. In such case, Focal Plane Resolution is '+
        'not the same as CCD''s actual resolution'),
    (iTag: $a20f; VType: xtvtRegular;     sName: 'Focal Plane Y-Resolution';             sVList: ''; 
      sDesc:
        'Pixel density at CCD''s position. If you have MegaPixel digicam and take a picture by lower resolution '+
        '(e.g. VGA mode), this value is re-sampled by picture resolution. In such case, Focal Plane Resolution is '+
        'not the same as CCD''s actual resolution'),
    (iTag: $a210; VType: xtvtRegular;     sName: 'Focal Plane Resolution Unit';          sVList: '1=None Specified:2=Inch:3=Centimeter';
      sDesc: 'Unit of Focal Plane X-Resoluton & Focal Plane Y-Resolution'),
    (iTag: $a211; VType: xtvtRegular;     sName: 'Image Number';                         sVList: ''; sDesc: ''),
    (iTag: $a212; VType: xtvtRegular;     sName: 'Security Classification';              sVList: ''; sDesc: ''),
    (iTag: $a213; VType: xtvtRegular;     sName: 'Image History';                        sVList: '';
      sDesc: 'Specifies the history of the image'),
    (iTag: $a214; VType: xtvtRegular;     sName: 'Subject Location';                     sVList: '';
      sDesc:
        'Location of the main subject in the scene. The value of this tag represents the pixel at the center of the '+
        'main subject relative to the left edge. The first value indicates the column number, and the second value '+
        'indicates the row number'),
    (iTag: $a215; VType: xtvtRegular;     sName: 'Exposure Index';                       sVList: '';
      sDesc: 'Same as ISO Speed Ratings. Only Kodak''s digicams use this tag instead of ISO Speed Rating'),
    (iTag: $a216; VType: xtvtRegular;     sName: 'TIFF/EPStandard ID';                   sVList: ''; sDesc: ''),
    (iTag: $a217; VType: xtvtRegular;     sName: 'Sensing Method';
      sVList:
        '0=Unknown:1=Monochrome Area:2=One Chip Color Area:3=Two Chip Color Area:4=Three Chip Color Area:'+
        '5=Color Sequential Area:6=Monochrome Linear:7=Trilinear:8=Color Sequential Linear';
      sDesc: 'Shows type of image sensor unit'),
    (iTag: $a300; VType: xtvtRegular;     sName: 'File Source';                          sVList: '1=Unknown:3=Digital Still Camera';
      sDesc: 'Indicates the image source'),
    (iTag: $a301; VType: xtvtRegular;     sName: 'Scene Type';                           sVList: '0=Unknown:1=Directly Photographed';
      sDesc: 'Indicates the type of scene'),
    (iTag: $a302; VType: xtvtRegular;     sName: 'CFA Pattern';                          sVList: '';
      sDesc:
        'Indicates the Color Filter Array (CFA) geometric pattern of the image sensor when a one-chip color area '+
        'sensor is used. It does not apply to all sensing methods'),
    (iTag: $a401; VType: xtvtRegular;     sName: 'Custom Rendered';                      sVList: '0=Normal process:1=Custom process'; sDesc: ''),
    (iTag: $a402; VType: xtvtRegular;     sName: 'Exposure Mode';                        sVList: '0=Auto:1=Manual:2=Auto bracket'; sDesc: ''),
    (iTag: $a403; VType: xtvtRegular;     sName: 'White Balance';                        sVList: '0=Auto:1=Manual'; sDesc: ''),
    (iTag: $a404; VType: xtvtRegular;     sName: 'Digital Zoom Ratio';                   sVList: ''; sDesc: ''),
    (iTag: $a405; VType: xtvtRegular;     sName: 'Focal Length In 35mm Film';            sVList: ''; sDesc: ''),
    (iTag: $a406; VType: xtvtRegular;     sName: 'Scene Capture Type';
      sVList: '0=Standard:1=Landscape:2=Portrait:3=Night scene';
      sDesc: ''),
    (iTag: $a407; VType: xtvtRegular;     sName: 'Gain Control';
      sVList: '0=None:1=Low gain up:2=High gain up:3=Low gain down:4=High gain down';
      sDesc: ''),
    (iTag: $a408; VType: xtvtRegular;     sName: 'Contrast';                             sVList: '0=Normal:1=Soft:2=Hard'; sDesc: ''),
    (iTag: $a409; VType: xtvtRegular;     sName: 'Saturation';                           sVList: '0=Normal:1=Low:2=High'; sDesc: ''),
    (iTag: $a40a; VType: xtvtRegular;     sName: 'Sharpness';                            sVList: '0=Normal:1=Soft:2=Hard'; sDesc: ''),
    (iTag: $a40b; VType: xtvtRegular;     sName: 'Device Setting Description';           sVList: ''; sDesc: ''),
    (iTag: $a40c; VType: xtvtRegular;     sName: 'Subject Distance Range';
      sVList: '0=Unknown:1=Macro:2=Close view:3=Distant view';
      sDesc: ''),
    (iTag: $a420; VType: xtvtRegular;     sName: 'Image Unique ID';                      sVList: '0=Close view:1=Distant view'; sDesc: ''));

   // Returns pointer to EXIF tag entry, nil if not found
  function FindEXIFTag(iTag: Integer): PEXIFTag;

implementation

   // Intel-to-Motorola (and back) byte-order data conversion

  function IMTranslateWord(w: Word): Word;
  begin
    Result := (w shl 8) or (w shr 8);
  end;

  function IMTranslateInt(i: Integer): Integer;
  begin
    Result := (i shl 24) or (i shl 8 and $00ff0000) or (i shr 8 and $0000ff00) or (i shr 24);
  end;

  function IMTranslateRational(i64: Int64): Int64;
  begin
    Result := Int64(IMTranslateInt(i64 shr 32)) shl 32 or IMTranslateInt(i64);
  end;

   // Misc routines

  function FindEXIFTag(iTag: Integer): PEXIFTag;
  var i: Integer;
  begin
    for i := 0 to High(aEXIFTags) do begin
      Result := @aEXIFTags[i];
      if Result^.iTag=iTag then Exit;
    end;
    Result := nil;
  end;
  
  function GetFirstWord(const s, sDelimiters: String): String;
  var i: Integer;
  begin
    i := 1;
    while (i<=Length(s)) and (Pos(s[i], sDelimiters)=0) do Inc(i);
    Result := Copy(s, 1, i-1);
  end;

  function ExtractFirstWord(var s: String; const sDelimiters: String): String;
  begin
    Result := GetFirstWord(s, sDelimiters);
    Delete(s, 1, Length(Result)+1);
  end;

  procedure AccumulateStr(var s: String; const sSeparator, sAdd: String);
  begin
    if sAdd<>'' then begin
      if s<>'' then s := s+sSeparator;
      s := s+sAdd;
    end;
  end;

   //===================================================================================================================
   // TImageMetadata
   //===================================================================================================================

  constructor TImageMetadata.Create(const sFileName: String);
  var fs: TFileStream;
  begin
    inherited Create;
    FStatusCode := IMS_Unknown;
    FEXIFData := TStringList.Create;
     // Create a file stream to load data from
    try
      fs := TFileStream.Create(sFileName, fmOpenRead or fmShareDenyWrite);
    except
       // Handle any exception translating it into the appropriate StatusCode
      SetStatusCode(IMS_CannotOpenFile);
      fs := nil; // Satisfy the compiler
    end;
     // If file is open successfully
    if not FFailed then
      try
        LoadFromStream(fs);
      finally
        fs.Free;
      end;
  end;

  destructor TImageMetadata.Destroy;
  begin
    FEXIFData.Free;
    inherited Destroy;
  end;

  function TImageMetadata.FetchMarkerData(Stream: TStream; bSkip: Boolean): String;
  var wSize: Word;
  begin
    Result := '';
     // Data size includes size-bytes' length itself
    if not FFailed then begin
      wSize := FetchWord(Stream, True)-2;
      try
        if bSkip then
          Stream.Seek(wSize, soCurrent)
        else begin
          SetLength(Result, wSize);
          Stream.ReadBuffer(Result[1], wSize);
        end;
      except
        SetStatusCode(IMS_ReadFailure);
      end;
    end;
  end;

  function TImageMetadata.FetchWord(Stream: TStream; bMotorola: Boolean): Word;
  begin
    try
      Stream.ReadBuffer(Result, SizeOf(Result));
    except
      SetStatusCode(IMS_ReadFailure);
    end;
    if not FFailed and bMotorola then Result := IMTranslateWord(Result);
  end;

  procedure TImageMetadata.LoadFromStream(Stream: TStream);
  var wMarker: Word;
  begin
     // Parse markers consequently
    Stream.Seek(0, soBeginning);
    while not FFailed do begin
      wMarker := FetchWord(Stream, True);
      if not FFailed then begin
         // A JPEG file *must* start with SOI marker
        if not FDataFetched then begin
          if wMarker<>JM_SOI then SetStatusCode(IMS_NotAJPEGFile);
         // Not a first marker, go on
        end else
          case wMarker of
             // EOI or SOS encountered, get out
            JM_EOI, JM_SOS: Break;
             // EXIF data to be parsed out
            JM_EXIF: begin
              ParseEXIFMarkerData(FetchMarkerData(Stream, False));
              Exit;
            end;
             // All other tags are skipped
            else FetchMarkerData(Stream, True);
          end;
        FDataFetched := True;
      end;
    end;
     // If no metadata found
    if not FFailed then SetStatusCode(IMS_NoMetadata);
  end;

  procedure TImageMetadata.ParseEXIFMarkerData(const sData: String);
  var
    sTIFFHeader, sRaw: String;
    bMotorolaOrder: Boolean;
    iIFDOffset: Integer;
  begin
     // Check EXIF header
    if (Length(sData)<6+8) or (Copy(sData, 1, 6)<>SEXIFHeader) then
      SetStatusCode(IMS_InvalidEXIFHeader)
    else begin
       // Check TIFF header / Get TIFF byte order
      sTIFFHeader := Copy(sData, 7, 4);
      if sTIFFHeader=STIFFIntelHeader then
        bMotorolaOrder := False
      else if sTIFFHeader=STIFFMotorolaHeader then
        bMotorolaOrder := True
      else begin
        SetStatusCode(IMS_InvalidTIFFHeader);
        Exit;
      end;
       // Get IFD Offset
      Move(sData[11], iIFDOffset, 4);
      if bMotorolaOrder then iIFDOffset := IMTranslateInt(iIFDOffset);
       // Process chained IFDs. Offset is computed relative to the TIFF header
      sRaw := Copy(sData, 7, MaxInt);
      while iIFDOffset<>0 do iIFDOffset := ProcessIFD(sRaw, iIFDOffset, bMotorolaOrder);
       // Set status to indicate success
      if not FFailed then SetStatusCode(IMS_OK);
    end;
  end;

  function TImageMetadata.ProcessIFD(const sData: String; iOffset: Integer; bMotorola: Boolean): Integer;
  var
    iDataLen, iEntDataSize, iComponentSize, i: Integer;
    wEntCount, wEntry: Word;
    IE: TIFDEntry;
    pt: PEXIFTag;
    sCompValue, sTagValue: String;
    i64Component: Int64;

     // Retrieves data component at specified Index (0..iCompCount-1). Returns True if retrieval succeeded
    function GetComponent(iIndex: Integer; out i64CompValue: Int64): Boolean;
    var ptr: Pointer;
    begin
      i64CompValue := 0;
       // Value smaller than or equal to 4 bytes size is stored directly in IE.iData
      if iEntDataSize<=4 then
        ptr := @IE.iData
       // For longer data iData is the offset. Check data size 
      else if IE.iData+iComponentSize>iDataLen then begin
        Result := False;
        Exit;
      end else
        ptr := @sData[IE.iData+1];
       // Move pointer to specified index
      Inc(Integer(ptr), iComponentSize*iIndex);
       // Get data, convert from Motorola byte-order if required (only data retrieved through offset)
      Move(ptr^, i64CompValue, iComponentSize);
      if bMotorola and (iEntDataSize>4) then
        case IE.wFmt of
          {2} iedUShort, iedSShort:       i64CompValue := IMTranslateWord(Word(i64CompValue));
          {4} iedULong, iedSLong:         i64CompValue := IMTranslateInt(Integer(i64CompValue));
          {8} iedURational, iedSRational: i64CompValue := IMTranslateRational(i64CompValue);
        end;
      Result := True;
    end;

     // Translates numeric Value to string
    function GetComponentAsString(i64Value: Int64): String;
    var
      sValue: Single absolute i64Value;
      dValue: Double absolute i64Value;
    begin
      Result := '';
      case IE.wFmt of
        iedUByte, iedUndef: Result := IntToStr(Byte(i64Value));
        iedAscii:           if i64Value and $ff<>0 then Result := Char(i64Value); // Skip terminating char (#0)
        iedUShort:          Result := IntToStr(Word(i64Value));
        iedULong:           Result := IntToStr(Cardinal(i64Value));
        iedURational:       Result := Format('%d/%d', [Cardinal(i64Value), Cardinal(i64Value shr 32)]);
        iedSByte:           Result := IntToStr(ShortInt(i64Value));
        iedSShort:          Result := IntToStr(SmallInt(i64Value));
        iedSLong:           Result := IntToStr(Integer(i64Value));
        iedSRational:       Result := Format('%d/%d', [Integer(i64Value), Integer(i64Value shr 32)]);
        iedSingleFloat:     Result := FloatToStr(sValue);
        iedDoubleFloat:     Result := FloatToStr(dValue);
      end;
    end;

     // Decodes component value according to decoding list
    function DecodeComponentValue(const sValue: String): String;
    var s, sExpr: String;
    begin
      s := pt^.sVList;
      repeat
        sExpr := ExtractFirstWord(s, ':');
        if sValue=ExtractFirstWord(sExpr, '=') then begin
          Result := sExpr;
          Exit;
        end;
      until s='';
      Result := sValue;
    end;

     // Translates a buffer string containing Unicode characters to Ansi using the current system charset
    function UnicodeBufToAnsi(const sBuffer: String): String;
    var
      ws: WideString;
      iLen: Integer;
    begin
      iLen := Length(sBuffer) div 2;
       // Move wide chars to ws
      SetLength(ws, iLen);
      Move(sBuffer[1], ws[1], iLen*2);
       // Translate wide chars to Ansi chars
      SetLength(Result, iLen);
      WideCharToMultiByte(CP_ACP, 0, @ws[1], iLen, @Result[1], iLen, nil, nil);
       // Trim control chars & null-terminator
      Result := Trim(Result); 
    end;

     // Processes data of xtvtUserComment format
    function FormatUserComment(const sBuffer: String): String;
    var sCode, sValue: String;
    begin
       // The first 8 bytes indicate charset. The rest are value
      sCode  := Copy(sTagValue, 1, 7);
      sValue := Copy(sTagValue, 9, MaxInt);
       // Make the only distinction for Unicode here
      if AnsiSameText(sCode, 'UNICODE') then Result := UnicodeBufToAnsi(sValue) else Result := Trim(sValue);
    end;

     // Formats raw binary data as hexadecimal ASCII representation (without any delimiters)
    function FormatRawBinary(const sBuffer: String): String;
    const acHexChars: Array[$0..$f] of Char = '0123456789abcdef';
    var
      i: Integer;
      b: Byte;
    begin
      i := Length(sBuffer);
      SetLength(Result, i*2);
      for i := 1 to i do begin
        b := Byte(sBuffer[i]);
        Result[i*2-1] := acHexChars[b div 16];
        Result[i*2]   := acHexChars[b mod 16];
      end;
    end;

  begin
    Result := 0;
    iDataLen := Length(sData);
     // Get entries count. If offset points outside the data, just skip this IFD
    if iDataLen-iOffset<2 then Exit;
    Move(sData[iOffset+1], wEntCount, 2);
    if bMotorola then wEntCount := IMTranslateWord(wEntCount);
    if wEntCount=0 then Exit;
     // Check data size. If offset points outside the data, just skip this IFD
    if iDataLen-iOffset<2+wEntCount*12+4 then Exit;
     // Iterate thru IFD's entries
    Inc(iOffset, 2);
    for wEntry := 0 to wEntCount-1 do begin
      Move(sData[iOffset+1], IE, SizeOf(IE));
       // Convert from Motorola byte-order if necessary
      if bMotorola then
        with IE do begin
          wTag     := IMTranslateWord(wTag);
          wFmt     := IMTranslateWord(wFmt);
          iCompCnt := IMTranslateInt(iCompCnt);
          iData    := IMTranslateInt(iData);
        end;
       // Try to find tag
      pt := FindEXIFTag(IE.wTag);
       // Check tag, component count and datatype
      if (pt<>nil) and (IE.iCompCnt>0) and (IE.wFmt>=1) and (IE.wFmt<=12) then begin
         // Compute data size
        iComponentSize := aIFDEntSize[IE.wFmt];
        iEntDataSize := IE.iCompCnt*iComponentSize;
         // Compose tag value
        sTagValue := '';
        for i := 0 to IE.iCompCnt-1 do begin
          if not GetComponent(i, i64Component) then begin
            sTagValue := '(error)';
            Break;
          end;
          case pt^.VType of
             // Regular value: translate it on-the-fly
            xtvtRegular: begin
              sCompValue := GetComponentAsString(i64Component);
               // If component is to be decoded
              if pt^.sVList<>'' then begin
                sTagValue := DecodeComponentValue(sCompValue);
                Break;
              end;
               // ASCII strings are simply concatenated, other data values are separated with comma
              if IE.wFmt=iedAscii then sTagValue := sTagValue+sCompValue else AccumulateStr(sTagValue, ',', sCompValue);
               // If the component is an IFD offset, then process this IFD
              if (IE.wTag=EXIF_TAG_SUBIFD) or (IE.wTag=EXIF_TAG_INTEROPIFD) then ProcessIFD(sData, i64Component, bMotorola);
            end;
             // Other values - to be processed later, accumulate them as binary string
            else sTagValue := sTagValue+Char(i64Component);
          end;
        end;
         // Post-process the non-regular value
        case pt^.VType of
          xtvtUserComment: sTagValue := FormatUserComment(sTagValue);
          xtvtCharVersion: sTagValue := Copy(sTagValue, 1, 2)+'.'+Copy(sTagValue, 3, MaxInt);
          xtvtUnicode:     sTagValue := UnicodeBufToAnsi(sTagValue);
          xtvtBinary:      sTagValue := FormatRawBinary(sTagValue);
        end;
         // Register value
        FEXIFData.AddObject(sTagValue, Pointer(IE.wTag));
      end;
      Inc(iOffset, 12);
    end;
     // The last four bytes are always offset to the next IFD, or zero if no one
    Move(sData[iOffset+1], Result, SizeOf(Result));
    if bMotorola then Result := IMTranslateInt(Result);
  end;

  procedure TImageMetadata.SetStatusCode(iCode: Integer);
  begin
    FStatusCode := iCode;
    if iCode<>IMS_OK then FFailed := True; 
  end;

end.
