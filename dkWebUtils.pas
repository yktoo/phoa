//**********************************************************************************************************************
//  $Id: dkWebUtils.pas,v 1.2 2004/09/11 17:59:54 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  DK Software types, constants
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org
//**********************************************************************************************************************
unit dkWebUtils;

interface
uses Windows, SysUtils;

type
   // DK Software Web information interface
  IDKWeb = interface(IInterface)
    ['{D00820EA-9CB7-4511-9CB2-A412F72BAD26}']
     // Открывает основной web-сайт
    procedure Open_MainWebsite;
     // Открывает главную страницу сайта
    procedure Open_Index;
     // Открывает страницу поддержки продукта
    procedure Open_Support;
     // Открывает страницу проверки обновлений текущей версии
    procedure Open_VerCheck;
     // Открывает страницу с информацией о продукте
    procedure Open_ViewInfo;
     // Prop handlers
    function GetMainSiteURI: String; 
    function GetProductSID: String;
    function GetVersionSID: String;
     // Props
     // -- URI главного сайта
    property MainSiteURI: String read GetMainSiteURI;
     // -- Строковый идентификатор продукта, присваивается при создании
    property ProductSID: String read GetProductSID;
     // -- Строковый идентификатор версии продукта, присваивается при создании
    property VersionSID: String read GetVersionSID;
  end;


const
   // Web-related strings
  SWeb_MainSite                        = 'http://www.dk-soft.org/';
  SWeb_RedirURI                        = SWeb_MainSite+'redir.php';
  SWeb_Param_RedirAction               = 'action';
  SWeb_Param_Product                   = 'product';
  SWeb_Param_Version                   = 'version';
  SWeb_RedirAction_Index               = 'index';
  SWeb_RedirAction_Support             = 'support';
  SWeb_RedirAction_VersionCheck        = 'vercheck';
  SWeb_RedirAction_ViewInfo            = 'viewinfo';

   // Склеивает параметры массива aParamsAndVals (формата ['param1', 'value1', 'param2', 'value2', ...]) в виде
   //   HTML-строки параметров и возвращает эту строку. Если параметру не хватило значения или значение пустое, в
   //   итоговой строке не пишет знака '='. Если cPrefix=#0, возвращает строку как есть, иначе вставляет cPrefix в
   //   начало строки, но только если строка получилась не пустая
  function  DKMakeParamStr(const aParamsAndVals: Array of String; cPrefix: Char = #0): String;
   // Открывает заданный URI
  procedure DKShellOpen(const sURI: String); overload;
  procedure DKShellOpen(const sURI: String; const aParams: Array of const); overload;

   // Создаёт и возвращает экземпляр IDKWeb
  function DKCreateDKWeb(const sProductSID, sVersionSID: String): IDKWeb;

implementation
uses ShellAPI;

  function DKMakeParamStr(const aParamsAndVals: Array of String; cPrefix: Char = #0): String;
  var
    i, idx: Integer;
    sParam, sValue: String;
  begin
    Result := '';
    for i := 0 to (Length(aParamsAndVals) div 2)-1 do begin
      idx := i*2;
       // Получаем параметр
      sParam := aParamsAndVals[idx];
       // Если параметр не пустой
      if sParam<>'' then begin
         // Получаем значение
        if idx<High(aParamsAndVals) then sValue := aParamsAndVals[idx+1] else sValue := '';
         // Формируем выражение параметра
        if sValue<>'' then sParam := sParam+'='+sValue;
         // Добавляем к общей строке
        if Result<>'' then Result := Result+'&';
        Result := Result+sParam;
      end;
    end;
     // Добавляем префикс, если нужно
    if (cPrefix<>#0) and (Result<>'') then Result := cPrefix+Result;
  end;

  procedure DKShellOpen(const sURI: String);
  begin
    ShellExecute(0, nil, PChar(sURI), nil, nil, SW_SHOWNORMAL);
  end;

  procedure DKShellOpen(const sURI: String; const aParams: Array of const);
  begin
    DKShellOpen(Format(sURI, aParams));
  end;

type
   //===================================================================================================================
   // TDKWeb - реализация TDKWeb
   //===================================================================================================================

  TDKWeb = class(TInterfacedObject, IDKWeb)
  private
     // Prop storage
    FProductSID: String;
    FVersionSID: String;
     // Открывает web-страницу редиректа с указанными параметрами
    procedure RedirAction(const sRedirAction: String; const aParamsAndVals: Array of String);
     // IDKWeb
    procedure Open_MainWebsite;
    procedure Open_Index;
    procedure Open_Support;
    procedure Open_VerCheck;
    procedure Open_ViewInfo;
    function  GetMainSiteURI: String; 
    function  GetProductSID: String;
    function  GetVersionSID: String;
  public
    constructor Create(const sProductSID, sVersionSID: String);
  end;

  constructor TDKWeb.Create(const sProductSID, sVersionSID: String);
  begin
    inherited Create;
    FProductSID := sProductSID;
    FVersionSID := sVersionSID;
  end;

  function TDKWeb.GetMainSiteURI: String;
  begin
    Result := SWeb_MainSite;
  end;

  function TDKWeb.GetProductSID: String;
  begin
    Result := FProductSID;
  end;

  function TDKWeb.GetVersionSID: String;
  begin
    Result := FVersionSID;
  end;

  procedure TDKWeb.Open_Index;
  begin
    RedirAction(SWeb_RedirAction_Index, []);
  end;

  procedure TDKWeb.Open_MainWebsite;
  begin
    DKShellOpen(SWeb_MainSite);
  end;

  procedure TDKWeb.Open_Support;
  begin
    RedirAction(SWeb_RedirAction_Support, [SWeb_Param_Product, FProductSID]);
  end;

  procedure TDKWeb.Open_VerCheck;
  begin
    RedirAction(SWeb_RedirAction_VersionCheck, [SWeb_Param_Product, FProductSID, SWeb_Param_Version, FVersionSID]);
  end;

  procedure TDKWeb.Open_ViewInfo;
  begin
    RedirAction(SWeb_RedirAction_ViewInfo, [SWeb_Param_Product, FProductSID]);
  end;

  procedure TDKWeb.RedirAction(const sRedirAction: String; const aParamsAndVals: array of String);
  begin
    DKShellOpen(
      SWeb_RedirURI+
      DKMakeParamStr([SWeb_Param_RedirAction, sRedirAction], '?')+
      DKMakeParamStr(aParamsAndVals, '&'));
  end;

   //===================================================================================================================

  function DKCreateDKWeb(const sProductSID, sVersionSID: String): IDKWeb;
  begin
    Result := TDKWeb.Create(sProductSID, sVersionSID);
  end;

end.
