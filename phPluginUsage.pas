//**********************************************************************************************************************
//  $Id: phPluginUsage.pas,v 1.1 2005-02-14 19:34:08 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPluginUsage;

interface
uses Windows, SysUtils, Classes, phPlugin;

   // Сканирует каталог плагинов и регистрирует найденные плагины
  procedure ScanForPlugins;

   // Возвращает количество зарегистрированных классов плагинов
  function PluginGetClassCount: Integer;
   // Возвращает класс плагина с индексом Index
  function PluginGetClass(iIndex: Integer): IPhoaPluginClass;

implementation
uses ConsVars;

var
   // Список классов зарегистрированных плагинов
  PluginClasses: IInterfaceList;

   // Процедура регистрации плагина
  procedure RegisterPlugin(const sPluginLib: String);
  var
    hLib: HINST;
    GetClassCountProc: TPhoaPluginGetClassCountProc;
    GetClassProc: TPhoaPluginGetClassProc;
    i, iCount: Integer;
    PluginClass: IPhoaPluginClass;
  begin
     // Грузим библиотеку
    hLib := LoadLibrary(PChar(sPluginLib));
    if hLib=0 then RaiseLastOSError;
     // Пытаемся получить процедуры
    GetClassCountProc := GetProcAddress(hLib, 'PhoaPluginGetClassCount');
    GetClassProc      := GetProcAddress(hLib, 'PhoaPluginGetClass');
    if Assigned(GetClassCountProc) and Assigned(GetClassProc) then begin
       // Получаем количество классов в библиотеке
      iCount := 0;
      GetClassCountProc(iCount);
       // Регистрируем классы
      for i := 0 to iCount-1 do begin
        PluginClass := nil;
        GetClassProc(i, PluginClass);
        if PluginClass<>nil then PluginClasses.Add(PluginClass);
      end;
    end else
      FreeLibrary(hLib);
  end;

   // Рекурсивная процедура сканирования плагинов
  procedure ScanPluginDir(const sDir: String);
  var
    sPath: String;
    SRec: TSearchRec;
  begin
    sPath := IncludeTrailingPathDelimiter(sDir);
    if FindFirst(sPath+'*.*', faAnyFile, SRec)=0 then
      try
        repeat
           // Файл, вероятно плагин
          if SRec.Attr and faDirectory=0 then begin
            if UpperCase(ExtractFileExt(SRec.Name))='.DLL' then RegisterPlugin(sPath+SRec.Name);
           // Каталог
          end else if SRec.Name[1]<>'.' then
            ScanPluginDir(sPath+SRec.Name);
        until FindNext(SRec)<>0;
      finally
        FindClose(SRec);
      end;
  end;

  procedure ScanForPlugins;
  begin
     // Сканируем каталог плагинов
    ScanPluginDir(sApplicationPath+SRelativePluginPath);
  end;

  function PluginGetClassCount: Integer;
  begin
    Result := PluginClasses.Count;
  end;

  function PluginGetClass(iIndex: Integer): IPhoaPluginClass;
  begin
    Result := IPhoaPluginClass(PluginClasses[iIndex]);
  end;

initialization
  PluginClasses := TInterfaceList.Create;
finalization
  PluginClasses := nil;
end.
 
