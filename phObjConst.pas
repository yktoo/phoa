//**********************************************************************************************************************
//  $Id: phObjConst.pas,v 1.1 2004-10-12 12:38:09 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phObjConst;

interface

uses phIntf;

const
   // Размеры и качество эскиза
  IThumbWidth_Default              = 150;
  IThumbWidth_Min                  = 32;
  IThumbWidth_Max                  = 1000;

  IThumbHeight_Default             = 150;
  IThumbHeight_Min                 = 32;
  IThumbHeight_Max                 = 1000;

  IThumbQuality_Default            = 40;
  IThumbQuality_Max                = 100;

   // Соответствие между именами свойств изображений из phIntf и элементами TPicProperty
  aPicPropertyNames: Array[TPicProperty] of String = (
    SPhoaPicProp_ID,            // ppID
    SPhoaPicProp_FileName,      // ppFileName
    SPhoaPicProp_FullFileName,  // ppFullFileName
    SPhoaPicProp_FilePath,      // ppFilePath
    SPhoaPicProp_FileSize,      // ppFileSize
    SPhoaPicProp_FileSizeBytes, // ppFileSizeBytes
    SPhoaPicProp_PicWidth,      // ppPicWidth
    SPhoaPicProp_PicHeight,     // ppPicHeight
    SPhoaPicProp_PicDims,       // ppPicDims
    SPhoaPicProp_Format,        // ppFormat
    SPhoaPicProp_Date,          // ppDate
    SPhoaPicProp_Time,          // ppTime
    SPhoaPicProp_Place,         // ppPlace
    SPhoaPicProp_FilmNumber,    // ppFilmNumber
    SPhoaPicProp_FrameNumber,   // ppFrameNumber
    SPhoaPicProp_Author,        // ppAuthor
    SPhoaPicProp_Description,   // ppDescription
    SPhoaPicProp_Notes,         // ppNotes
    SPhoaPicProp_Media,         // ppMedia
    SPhoaPicProp_Keywords,      // ppKeywords
    SPhoaPicProp_Rotation,      // ppRotation
    SPhoaPicProp_Flips);        // ppFlips

   // Таблицы перекодировки
   // TPicProperty <-> Chunked sorting prop
  aXlat_PicPropertyToChunkSortingProp: Array[TPicProperty] of Word = (
     0,  // ppID
     1,  // ppFileName
     2,  // ppFullFileName
     3,  // ppFilePath
     4,  // ppFileSize
     5,  // ppFileSizeBytes
     6,  // ppPicWidth
     7,  // ppPicHeight
     8,  // ppPicDims
     9,  // ppFormat
    10,  // ppDate
    11,  // ppTime
    12,  // ppPlace
    13,  // ppFilmNumber
    14,  // ppFrameNumber
    15,  // ppAuthor
    16,  // ppDescription
    17,  // ppNotes
    18,  // ppMedia
    19,  // ppKeywords
    20,  // ppRotation
    21); // ppFlips
  aXlat_ChunkSortingPropToPicProperty: Array[0..21] of TPicProperty = (
    ppID,            //  0  ID
    ppFileName,      //  1  Picture filename
    ppFullFileName,  //  2  Picture filename with path
    ppFilePath,      //  3  Picture file path
    ppFileSize,      //  4  Picture file size
    ppFileSizeBytes, //  5  Picture file size in bytes
    ppPicWidth,      //  6  Image width
    ppPicHeight,     //  7  Image height
    ppPicDims,       //  8  Image dimensions
    ppFormat,        //  9  Pixel format
    ppDate,          // 10  Date
    ppTime,          // 11  Time
    ppPlace,         // 12  Place
    ppFilmNumber,    // 13  Film number
    ppFrameNumber,   // 14  Frame number
    ppAuthor,        // 15  Author
    ppDescription,   // 16  Description
    ppNotes,         // 17  Notes
    ppMedia,         // 18  Media
    ppKeywords,      // 19  Keywords
    ppRotation,      // 20  Rotation
    ppFlips);        // 21  Flips

   // Свойства, поддерживаемые ревизией 2
  GBPropsRev2: TPicGroupByProperties = [
    gbpFilePath, gbpDateByYear, gbpDateByMonth, gbpDateByDay, gbpPlace, gbpFilmNumber, gbpKeywords];

   // TPicGroupByProperty <-> Chunked sorting prop
  aXlat_GBPropertyToChunkGroupingProp: Array[TPicGroupByProperty] of Word = (
     0,  // gbpFilePath
     1,  // gbpDateByYear
     2,  // gbpDateByMonth
     3,  // gbpDateByDay
     4,  // gbpTimeHour
     5,  // gbpTimeMinute
     6,  // gbpPlace
     7,  // gbpFilmNumber
     8,  // gbpAuthor
     9,  // gbpMedia
    10); // gbpKeywords
  aXlat_ChunkGroupingPropToGBProperty: Array[0..10] of TPicGroupByProperty = (
   gbpFilePath,    //  0  Picture file path
   gbpDateByYear,  //  1  Date year
   gbpDateByMonth, //  2  Date month
   gbpDateByDay,   //  3  Date day
   gbpTimeHour,    //  4  Time hour
   gbpTimeMinute,  //  5  Time minute
   gbpPlace,       //  6  Place
   gbpFilmNumber,  //  7  Film number
   gbpAuthor,      //  8  Author
   gbpMedia,       //  9  Media name/code
   gbpKeywords);   // 10  Keywords

   // TPicGroupByProperty версии 2 <-> Byte
  aXlat_GBPropToGBProp2: Array[TPicGroupByProperty] of Byte = (
    0,   // gbpFilePath
    1,   // gbpDateByYear
    2,   // gbpDateByMonth
    3,   // gbpDateByDay
    255, // gbpTimeHour
    255, // gbpTimeMinute
    4,   // gbpPlace
    5,   // gbpFilmNumber
    255, // gbpAuthor
    255, // gbpMedia
    6);  // gbpKeywords
  aXlat_GBProp2ToGBProp: Array[0..6] of TPicGroupByProperty = (
   gbpFilePath,      // 0  Picture file path
   gbpDateByYear,    // 1  Date year
   gbpDateByMonth,   // 2  Date month
   gbpDateByDay,     // 3  Date day
   gbpPlace,         // 4  Place
   gbpFilmNumber,    // 5  Film number
   gbpKeywords);     // 6  Keywords

   // Наименования констант с наименованиями папок для неклассифицированных изображений в зависимости от вида группировки
  asUnclassifiedConsts: Array[TPicGroupByProperty] of String[24] = (
    '',                         // gbpFilePath - такого не бывает
    'SUnclassified_Date',       // gbpDateByYear
    'SUnclassified_Date',       // gbpDateByMonth
    'SUnclassified_Date',       // gbpDateByDay
    'SUnclassified_Time',       // gbpTimeHour
    'SUnclassified_Time',       // gbpTimeMinute
    'SUnclassified_Place',      // gbpPlace
    'SUnclassified_FilmNumber', // gbpFilmNumber
    'SUnclassified_Author',     // gbpAuthor
    'SUnclassified_Media',      // gbpMedia
    'SUnclassified_Keywords');  // gbpKeywords

implementation

end.
 