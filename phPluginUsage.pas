//**********************************************************************************************************************
//  $Id: phPluginUsage.pas,v 1.3 2005-02-19 13:30:16 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPluginUsage;

interface
uses Windows, SysUtils, Classes, phIntf, phAppIntf, phMutableIntf, phNativeIntf, phPlugin;

type

   //===================================================================================================================
   // Интерфейс списка загруженных плагин-модулей
   //===================================================================================================================

  IPhoaPluginModuleList = interface(IInterface)
    ['{A96A3A43-3A4B-4514-86FF-40876AA9B508}']
     // Prop handlers
    function  GetCount: Integer;
    function  GetItems(Index: Integer): IPhoaPluginModule; 
     // Props
     // -- Количество записей в списке
    property Count: Integer read GetCount;
     // -- Модули по индексу
    property Items[Index: Integer]: IPhoaPluginModule read GetItems; default;
  end;

var
   // Глобальный список данных зарегистрированных плагин-модулей
  PluginModules: IPhoaPluginModuleList;

   // Создаёт PluginModules и сканирует каталог плагинов, регистрируя в нём PluginModules найденные
  procedure PluginsInitialize(App: IPhotoAlbumApp);
   // Выгружает все плагины
  procedure PluginsFinalize;

implementation
uses ConsVars;

type

   //===================================================================================================================
   // Данные плагин-модуля
   //===================================================================================================================

  P_PluginModuleInfo = ^T_PluginModuleInfo;
  T_PluginModuleInfo = record
    hLib: HINST;               // Handle загруженной библиотеки
    Module: IPhoaPluginModule; // Загруженный модуль
  end;

   //===================================================================================================================
   // Список данных о плагин-модулях
   //===================================================================================================================

  T_PluginModuleInfoList = class(TList)
  private
     // Приложение
    FApp: IPhotoAlbumApp;
     // Prop handlers
    function GetItems(Index: Integer): P_PluginModuleInfo;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(AApp: IPhotoAlbumApp);
    function  Add(hLib: HINST; Module: IPhoaPluginModule): Integer;
     // Регистрирует плагин. Возвращает True, если указанный файл действительно является плагин-библиотекой
    function  RegisterPluginLib(const sPluginLib: String): Boolean;
     // Props
     // -- Пункты по индексу
    property Items[Index: Integer]: P_PluginModuleInfo read GetItems; default;
  end;

  function T_PluginModuleInfoList.Add(hLib: HINST; Module: IPhoaPluginModule): Integer;
  var p: P_PluginModuleInfo;
  begin
    New(p);
    Result := inherited Add(p);
    p.hLib   := hLib;
    p.Module := Module;
  end;

  constructor T_PluginModuleInfoList.Create(AApp: IPhotoAlbumApp);

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
              if UpperCase(ExtractFileExt(SRec.Name))='.DLL' then RegisterPluginLib(sPath+SRec.Name);
             // Каталог
            end else if SRec.Name[1]<>'.' then
              ScanPluginDir(sPath+SRec.Name);
          until FindNext(SRec)<>0;
        finally
          FindClose(SRec);
        end;
    end;

  begin
    inherited Create;
    FApp := AApp;
     // Сканируем каталог плагинов
    ScanPluginDir(sApplicationPath+SRelativePluginPath);
  end;

  function T_PluginModuleInfoList.GetItems(Index: Integer): P_PluginModuleInfo;
  begin
    Result := Get(Index);
  end;

  procedure T_PluginModuleInfoList.Notify(Ptr: Pointer; Action: TListNotification);
  begin
    if Action=lnDeleted then begin
       // Освобождаем интерфейс модуля
      P_PluginModuleInfo(Ptr).Module := nil;
       // Выгружаем библиотеку
      FreeLibrary(P_PluginModuleInfo(Ptr).hLib);
       // Освобождаем память
      Dispose(P_PluginModuleInfo(Ptr));
    end;
  end;

  function T_PluginModuleInfoList.RegisterPluginLib(const sPluginLib: String): Boolean;
  var
    hLib: HINST;
    GetModuleProc: TPhoaGetPluginModuleProc;
  begin
    Result := False;
     // Грузим библиотеку
    hLib := LoadLibrary(PChar(sPluginLib));
    if hLib<>0 then begin
       // Пытаемся получить процедуру
      GetModuleProc := GetProcAddress(hLib, 'PhoaGetPluginModule');
       // Если удалось - регистрируем модуль
      if Assigned(GetModuleProc) then begin
        Add(hLib, GetModuleProc(FApp));
        Result := True;
       // Иначе выгружаем билиотеку
      end else
        FreeLibrary(hLib);
    end;
  end;

   //===================================================================================================================
   // TPhoaPluginModuleList - реализация IPhoaPluginModuleList
   //===================================================================================================================
type
  TPhoaPluginModuleList = class(TInterfacedObject, IPhoaPluginModuleList)
  private
     // Собственно список
    FList: T_PluginModuleInfoList;
     // IPhoaPluginModuleList
    function  GetCount: Integer;
    function  GetItems(Index: Integer): IPhoaPluginModule;
  public
    constructor Create(AApp: IPhotoAlbumApp);
    destructor Destroy; override;
  end;

  constructor TPhoaPluginModuleList.Create(AApp: IPhotoAlbumApp);
  begin
    inherited Create;
    FList := T_PluginModuleInfoList.Create(AApp);
  end;

  destructor TPhoaPluginModuleList.Destroy;
  begin
    FList.Free;
    inherited Destroy;
  end;

  function TPhoaPluginModuleList.GetCount: Integer;
  begin
    Result := FList.Count;
  end;

  function TPhoaPluginModuleList.GetItems(Index: Integer): IPhoaPluginModule;
  begin
    Result := FList[Index].Module;
  end;

   //===================================================================================================================

  procedure PluginsInitialize(App: IPhotoAlbumApp);
  begin
    PluginModules := TPhoaPluginModuleList.Create(App);
  end;

  procedure PluginsFinalize;
  begin
    FreeAndNil(PluginModules);
  end;

end.
 
