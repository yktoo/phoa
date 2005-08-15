//**********************************************************************************************************************
//  $Id: phPluginUsage.pas,v 1.7 2005-08-15 11:25:11 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPluginUsage;

interface
uses Windows, SysUtils, Classes, phIntf, phAppIntf, phMutableIntf, phNativeIntf;

type

   //===================================================================================================================
   // Интерфейс списка загруженных плагин-модулей
   //===================================================================================================================

  IPhoaPluginModules = interface(IInterface)
    ['{A96A3A43-3A4B-4514-86FF-40876AA9B508}']
     // Вызывает AppInitialized для каждого плагина
    procedure AppInitialized(App: IPhoaApp);
     // Вызывает AppFinalizing для каждого плагина
    procedure AppFinalizing;
     // Создаёт плагины указанного вида и сохраняет их в списке Plugins
    procedure CreatePlugins(Kinds: TPhoaPluginKinds);
     // Удаляет плагины указанного вида из списка Plugins
    procedure ReleasePlugins(Kinds: TPhoaPluginKinds);
     // Prop handlers
    function  GetModuleCount: Integer;
    function  GetModules(Index: Integer): IPhoaPluginModule;
    function  GetPluginCount: Integer;
    function  GetPlugins(Index: Integer): IPhoaPlugin;
     // Props
     // -- Количество записей в списке
    property ModuleCount: Integer read GetModuleCount;
     // -- Модули по индексу
    property Modules[Index: Integer]: IPhoaPluginModule read GetModules; default;
     // -- Количество созданных плагинов
    property PluginCount: Integer read GetPluginCount;
     // -- Созданные плагины по индексу
    property Plugins[Index: Integer]: IPhoaPlugin read GetPlugins;
  end;

var
   // Глобальный список данных зарегистрированных плагин-модулей
  PluginModules: IPhoaPluginModules;

   // Создаёт PluginModules и сканирует каталог плагинов, регистрируя в нём найденные
  procedure PluginsInitialize;
   // Выгружает все плагины
  procedure PluginsFinalize;

implementation
uses ConsVars, phMsgBox, udAbout;

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
     // Prop handlers
    function GetItems(Index: Integer): P_PluginModuleInfo;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create;
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

  constructor T_PluginModuleInfoList.Create;

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
    ShowProgressInfo('SMsg_LoadingPlugin', [ExtractFileName(sPluginLib)]);
    hLib := LoadLibrary(PChar(sPluginLib));
    if hLib<>0 then begin
       // Пытаемся получить процедуру
      GetModuleProc := GetProcAddress(hLib, 'PhoaGetPluginModule');
       // Если удалось - регистрируем модуль
      if Assigned(GetModuleProc) then
        try
          Add(hLib, GetModuleProc);
          Result := True;
        except
          on e: Exception do PhoaError('SErrCreatingPluginModule', [sPluginLib, e.Message]);
        end;
       // При неудаче выгружаем билиотеку
      if not Result then FreeLibrary(hLib);
    end else
      PhoaError('SErrLoadingPluginModule', [sPluginLib, SysErrorMessage(GetLastError)]);
  end;

   //===================================================================================================================
   // TPhoaPluginModules - реализация IPhoaPluginModules
   //===================================================================================================================
type
  TPhoaPluginModules = class(TInterfacedObject, IPhoaPluginModules)
  private
     // Список модулей
    FModuleList: T_PluginModuleInfoList;
     // Список созданный плагинов
    FPlugins: IInterfaceList; 
     // IPhoaPluginModules
    procedure AppInitialized(App: IPhoaApp);
    procedure AppFinalizing;
    procedure CreatePlugins(Kinds: TPhoaPluginKinds);
    procedure ReleasePlugins(Kinds: TPhoaPluginKinds);
    function  GetModuleCount: Integer;
    function  GetModules(Index: Integer): IPhoaPluginModule;
    function  GetPluginCount: Integer;
    function  GetPlugins(Index: Integer): IPhoaPlugin;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  procedure TPhoaPluginModules.AppFinalizing;
  var i: Integer;
  begin
    for i := 0 to FModuleList.Count-1 do FModuleList[i].Module.AppFinalizing;
  end;

  procedure TPhoaPluginModules.AppInitialized(App: IPhoaApp);
  var i: Integer;
  begin
    for i := 0 to FModuleList.Count-1 do FModuleList[i].Module.AppInitialized(App);
  end;

  constructor TPhoaPluginModules.Create;
  begin
    inherited Create;
    FModuleList := T_PluginModuleInfoList.Create;
    FPlugins    := TInterfaceList.Create;
  end;

  procedure TPhoaPluginModules.CreatePlugins(Kinds: TPhoaPluginKinds);
  var
    iModule, iClass: Integer;
    Module: IPhoaPluginModule;
    PClass: IPhoaPluginClass;
    Plugin: IPhoaPlugin;
  begin
     // Цикл по модулям
    for iModule := 0 to FModuleList.Count-1 do begin
      Module := FModuleList[iModule].Module;
       // Цикл по классам плагинов модуля
      for iClass := 0 to Module.PluginClassCount-1 do begin
        PClass := Module.PluginClasses[iClass];
         // Если вид плагина подходит, создаём и регистрируем плагин
        if PClass.Kind in Kinds then begin
          Plugin := PClass.CreatePlugin;
          FPlugins.Add(Plugin);
        end;
      end;
    end;
  end;

  destructor TPhoaPluginModules.Destroy;
  begin
    FPlugins := nil;
    FModuleList.Free;
    inherited Destroy;
  end;

  function TPhoaPluginModules.GetModuleCount: Integer;
  begin
    Result := FModuleList.Count;
  end;

  function TPhoaPluginModules.GetModules(Index: Integer): IPhoaPluginModule;
  begin
    Result := FModuleList[Index].Module;
  end;

  function TPhoaPluginModules.GetPluginCount: Integer;
  begin
    Result := FPlugins.Count;
  end;

  function TPhoaPluginModules.GetPlugins(Index: Integer): IPhoaPlugin;
  begin
    Result := IPhoaPlugin(FPlugins[Index]);
  end;

  procedure TPhoaPluginModules.ReleasePlugins(Kinds: TPhoaPluginKinds);
  var i: Integer;
  begin
    for i := FPlugins.Count-1 downto 0 do
      if IPhoaPlugin(FPlugins[i]).PluginClass.Kind in Kinds then FPlugins.Delete(i);
  end;

   //===================================================================================================================

  procedure PluginsInitialize;
  begin
    PluginModules := TPhoaPluginModules.Create;
  end;

  procedure PluginsFinalize;
  begin
    PluginModules := nil;
  end;

end.
 
