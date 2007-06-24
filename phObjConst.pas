//**********************************************************************************************************************
//  $Id: phObjConst.pas,v 1.6 2007-06-24 17:48:10 dale Exp $
//===================================================================================================================---
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phObjConst;

interface

uses phIntf, phPhoa;

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

  awsPicRotationText: Array[TPicRotation] of WideString = (
    '0°',    // pr0
    '90°',   // pr90
    '180°',  // pr180
    '270°'); // pr270

  awsPicFlipText: Array[TPicFlip] of WideString = (
    'H',  // pflHorz                                          
    'V'); // pflVert                                          

   // Таблицы перекодировки
   // TPicProperty <-> Chunked sorting prop
  aXlat_PicPropertyToChunkSortingProp: Array[TPicProperty] of Word = (
    IPhSortingProp_ID,            // ppID
    IPhSortingProp_FileName,      // ppFileName
    IPhSortingProp_FullFileName,  // ppFullFileName
    IPhSortingProp_FilePath,      // ppFilePath
    IPhSortingProp_FileSize,      // ppFileSize
    IPhSortingProp_FileSizeBytes, // ppFileSizeBytes
    IPhSortingProp_PicWidth,      // ppPicWidth
    IPhSortingProp_PicHeight,     // ppPicHeight
    IPhSortingProp_PicDims,       // ppPicDims
    IPhSortingProp_ThumbWidth,    // ppThumbWidth
    IPhSortingProp_ThumbHeight,   // ppThumbHeight
    IPhSortingProp_ThumbDims,     // ppThumbDims
    IPhSortingProp_Format,        // ppFormat
    IPhSortingProp_Date,          // ppDate
    IPhSortingProp_Time,          // ppTime
    IPhSortingProp_Place,         // ppPlace
    IPhSortingProp_FilmNumber,    // ppFilmNumber
    IPhSortingProp_FrameNumber,   // ppFrameNumber
    IPhSortingProp_Author,        // ppAuthor
    IPhSortingProp_Description,   // ppDescription
    IPhSortingProp_Notes,         // ppNotes
    IPhSortingProp_Media,         // ppMedia
    IPhSortingProp_Keywords,      // ppKeywords
    IPhSortingProp_Rotation,      // ppRotation
    IPhSortingProp_Flips);        // ppFlips
  aXlat_ChunkSortingPropToPicProperty: Array[0..24] of TPicProperty = (
    ppID,            //  0  IPhSortingProp_ID           
    ppFileName,      //  1  IPhSortingProp_FileName
    ppFullFileName,  //  2  IPhSortingProp_FullFileName
    ppFilePath,      //  3  IPhSortingProp_FilePath
    ppFileSize,      //  4  IPhSortingProp_FileSize
    ppFileSizeBytes, //  5  IPhSortingProp_FileSizeBytes
    ppPicWidth,      //  6  IPhSortingProp_PicWidth
    ppPicHeight,     //  7  IPhSortingProp_PicHeight
    ppPicDims,       //  8  IPhSortingProp_PicDims
    ppFormat,        //  9  IPhSortingProp_Format
    ppDate,          // 10  IPhSortingProp_Date
    ppTime,          // 11  IPhSortingProp_Time
    ppPlace,         // 12  IPhSortingProp_Place
    ppFilmNumber,    // 13  IPhSortingProp_FilmNumber
    ppFrameNumber,   // 14  IPhSortingProp_FrameNumber
    ppAuthor,        // 15  IPhSortingProp_Author
    ppDescription,   // 16  IPhSortingProp_Description
    ppNotes,         // 17  IPhSortingProp_Notes
    ppMedia,         // 18  IPhSortingProp_Media
    ppKeywords,      // 19  IPhSortingProp_Keywords
    ppRotation,      // 20  IPhSortingProp_Rotation
    ppFlips,         // 21  IPhSortingProp_Flips
    ppThumbWidth,    // 22  IPhSortingProp_ThumbWidth
    ppThumbHeight,   // 23  IPhSortingProp_ThumbHeight
    ppThumbDims);    // 24  IPhSortingProp_ThumbDims

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
  asUnclassifiedConsts: Array[TPicGroupByProperty] of AnsiString = (
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
 
