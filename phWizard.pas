//**********************************************************************************************************************
//  $Id: phWizard.pas,v 1.4 2004-06-01 13:27:52 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs;

type
   // Exception
  EPhoaWizardError = class(Exception);

   // Метод смены страницы мастера
  TPageChangeMethod = (pcmBackBtn, pcmNextBtn, pcmForced);

   // Интерфейс хост-страницы
  IWizardHostForm = interface(IInterface)
    ['{265DFB55-F29D-40F7-BAC7-3440D04D17ED}']
     // Вызывается в процессе смены страницы. Должна вернуть True, чтобы позволить смену
    function  PageChanging(ChangeMethod: TPageChangeMethod; var iNewPageID: Integer): Boolean;
     // Вызывается после отображения [другой] страницы
    procedure PageChanged(ChangeMethod: TPageChangeMethod; iPrevPageID: Integer);
     // Вызывается при изменении состояния страниц
    procedure StatusChanged;
     // Prop handlers
    function  GetHostControl: TWinControl; 
    function  GetNextPageID: Integer;
    function  GetStorageForm: TForm;
     // Props
     // -- Хост-контрол для размещения страниц
    property HostControl: TWinControl read GetHostControl;
     // -- Должна возвращать ID следующей страницы, или 0, если нет больше страниц
    property NextPageID: Integer read GetNextPageID;
     // -- Форма мастера
    property StorageForm: TForm read GetStorageForm;
  end;

  TWizardController = class;

  TWizardPage = class(TFrame)
     // Событие изменения данных страницы, вызывает OnStatusChange
    procedure PageDataChange(Sender: TObject);
  private
     // Prop storage
    FController: TWizardController;
    FID: Integer;
    FPageTitle: String;
     // Prop handlers
    function GetStorageForm: TForm;
    function GetIndex: Integer;
  protected
     // Вызывает Controller.OnStatusChange
    procedure StatusChanged;
     // Инициализация/финализация страницы. В базовом классе восстанавливают/сохраняют панели инструментов
    procedure InitializePage; virtual;
    procedure FinalizePage; virtual;
     // Вызывается перед отображением страницы. В базовом классе не делает ничего
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); virtual;
     // Вызывается после отображения страницы. В базовом классе не делает ничего
    procedure AfterDisplay(ChangeMethod: TPageChangeMethod); virtual;
     // Вызывается перед скрытием страницы. В базовом классе не делает ничего
    procedure BeforeHide(ChangeMethod: TPageChangeMethod); virtual;
     // Вызывается после скрытия страницы. В базовом классе не делает ничего
    procedure AfterHide(ChangeMethod: TPageChangeMethod); virtual;
     // Вызывается при нажатии пользователем кнопки Далее. Должна вернуть True, чтобы позволить смену страницы, иначе
     //   должна сама объяснить пользователю причину отказа
    function  NextPage: Boolean; virtual;
     // Prop handlers
    function  GetDataValid: Boolean; virtual;
    function  GetRegistrySection: String; virtual;
  public
    constructor Create(Controller: TWizardController; iID, iHelpContext: Integer; const sPageTitle: String); reintroduce; virtual;
    destructor Destroy; override;
     // Props
     // -- Должна возвращать True, если страница содержит допустимые данные. В базовом классе всегда True
    property DataValid: Boolean read GetDataValid;
     // -- Уникальный идентификатор страницы в списке
    property ID: Integer read FID;
     // -- Индекс страницы в списке контроллера
    property Index: Integer read GetIndex;
     // -- Владелец страницы
    property Controller: TWizardController read FController;
     // -- Текст, выводимый в заглавии страницы
    property PageTitle: String read FPageTitle;
     // -- Секция реестра для сохранения визуальны настроек страницы. Если пустая (как в базовом классе), сохранения не
     //    требуется
    property RegistrySection: String read GetRegistrySection;
     // -- Форма мастера. Получается через Controller
    property StorageForm: TForm read GetStorageForm;
  end;

  TWizardPageClass = class of TWizardPage;

   // Список страниц мастера
  TWizardController = class(TList)
  private
     // История смены страниц
    FPageIDHistory: Array of Integer;
     // Prop storage
    FHostFormIntf: IWizardHostForm;
    FKeepHistory: Boolean;
    function  GetItems(Index: Integer): TWizardPage;
    function  GetItemsByID(iID: Integer): TWizardPage;
    function  GetVisiblePageID: Integer;
    function  GetVisiblePage: TWizardPage;
    function  GetHistoryEmpty: Boolean;
    procedure SetKeepHistory(Value: Boolean);
  public
    constructor Create(AHostFormIntf: IWizardHostForm);
    destructor Destroy; override;
    procedure Add(Page: TWizardPage);
    procedure Remove(Page: TWizardPage);
    procedure Delete(Index: Integer);
    procedure Clear; override;
     // Создаёт и возвращает страницу заданного класса, дописывая её к концу списка
    function  CreatePage(PClass: TWizardPageClass; iID, iHelpContext: Integer; const sPageTitle: String): TWizardPage; virtual;
     // Ищет страницу. Возвращает -1, если не найдена
    function  IndexOf(Page: TWizardPage): Integer;
     // Ищет страницу по ID. Возвращает -1, если не найдена
    function  IndexOfID(iID: Integer): Integer;
     // Устанавливает текущую страницу. bCommit указывает, должна ли страница сохранять данные
    function  SetVisiblePageID(iNewID: Integer; ChangeMethod: TPageChangeMethod): Boolean;
     // Показывает предыдущую показанную страницу
    procedure SetPrevPageFromHistory;
     // Показывает следующую страницу
    procedure SetNextPage;
     // Props
     // -- True, если история смены страниц пуста
    property HistoryEmpty: Boolean read GetHistoryEmpty;
     // -- Интерфейс формы мастера
    property HostFormIntf: IWizardHostForm read FHostFormIntf;
     // -- Страницы по индексу
    property Items[Index: Integer]: TWizardPage read GetItems; default;
     // -- Страницы по ID. Генерирует Exception, если нет такой страницы
    property ItemsByID[iID: Integer]: TWizardPage read GetItemsByID;
     // -- Хранить ли историю смены страниц. По умолчанию True
    property KeepHistory: Boolean read FKeepHistory write SetKeepHistory;
     // -- ID отображаемой страницы в мастере. 0, если нет
    property VisiblePageID: Integer read GetVisiblePageID;
     // -- Отображаемая страница в мастере. nil, если нет
    property VisiblePage: TWizardPage read GetVisiblePage;
  end;

   // Raises EPhoaWizardError
  procedure PhoaWizardError(const sMsg: String); overload;
  procedure PhoaWizardError(const sMsg: String; const aParams: Array of const); overload;

implementation
{$R *.dfm}
uses VCLUtils, ConsVars, TB2Dock;

  procedure PhoaWizardError(const sMsg: String);
  begin
    raise EPhoaWizardError.Create(sMsg);
  end;

  procedure PhoaWizardError(const sMsg: String; const aParams: Array of const);
  begin
    raise EPhoaWizardError.CreatefMT(sMsg, aParams);
  end;

   //===================================================================================================================
   // TWizardPage
   //===================================================================================================================

  procedure TWizardPage.AfterDisplay(ChangeMethod: TPageChangeMethod);
  begin
    { does nothing }
  end;

  procedure TWizardPage.AfterHide(ChangeMethod: TPageChangeMethod);
  begin
    { does nothing }
  end;

  procedure TWizardPage.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  begin
    { does nothing }
  end;

  procedure TWizardPage.BeforeHide(ChangeMethod: TPageChangeMethod);
  begin
    { does nothing }
  end;

  constructor TWizardPage.Create(Controller: TWizardController; iID, iHelpContext: Integer; const sPageTitle: String);
  begin
    inherited Create(Controller.HostFormIntf.StorageForm);
    FID         := iID;
    HelpContext := iHelpContext;
    FPageTitle  := sPageTitle;
    FController := Controller;
    FController.Add(Self);
  end;

  destructor TWizardPage.Destroy;
  begin
    FController.Remove(Self);
    inherited Destroy;
  end;

  procedure TWizardPage.FinalizePage;
  begin
     // Сохраняем панели инструментов
    if RegistrySection<>'' then TBRegSavePositions(Self, HKEY_CURRENT_USER, SRegRoot+'\'+SRegWizPagesRoot+'\'+RegistrySection+SRegWizPages_Toolbars);
  end;

  function TWizardPage.GetDataValid: Boolean;
  begin
    Result := True;
  end;

  function TWizardPage.GetIndex: Integer;
  begin
    Result := FController.IndexOf(Self);
  end;

  function TWizardPage.GetRegistrySection: String;
  begin
    Result := '';
  end;

  function TWizardPage.GetStorageForm: TForm;
  begin
    Result := FController.HostFormIntf.StorageForm;
  end;

  procedure TWizardPage.InitializePage;
  begin
     // Восстанавливаем панели инструментов
    if RegistrySection<>'' then TBRegLoadPositions(Self, HKEY_CURRENT_USER, SRegRoot+'\'+SRegWizPagesRoot+'\'+RegistrySection+SRegWizPages_Toolbars);
  end;

  function TWizardPage.NextPage: Boolean;
  begin
    Result := True;
  end;

  procedure TWizardPage.PageDataChange(Sender: TObject);
  begin
    StatusChanged;
  end;

  procedure TWizardPage.StatusChanged;
  begin
    FController.HostFormIntf.StatusChanged;
  end;

   //===================================================================================================================
   // TWizardController
   //===================================================================================================================

  procedure TWizardController.Add(Page: TWizardPage);
  begin
    inherited Add(Page);
  end;

  procedure TWizardController.Clear;
  begin
    while Count>0 do Delete(Count-1);
    inherited Clear;
  end;

  constructor TWizardController.Create(AHostFormIntf: IWizardHostForm);
  begin
    inherited Create;
    FKeepHistory  := True;
    FHostFormIntf := AHostFormIntf;
  end;

  function TWizardController.CreatePage(PClass: TWizardPageClass; iID, iHelpContext: Integer; const sPageTitle: String): TWizardPage;
  begin
    Result := PClass.Create(Self, iID, iHelpContext, sPageTitle);
    Result.Parent := FHostFormIntf.HostControl;
    Result.Align := alClient;
    Result.InitializePage;
  end;

  procedure TWizardController.Delete(Index: Integer);
  begin
    GetItems(Index).Free;
  end;

  destructor TWizardController.Destroy;
  var i: Integer;
  begin
    for i := 0 to Count-1 do GetItems(i).FinalizePage;
    inherited Destroy;
  end;

  function TWizardController.GetHistoryEmpty: Boolean;
  begin
    Result := High(FPageIDHistory)<0;
  end;

  function TWizardController.GetItems(Index: Integer): TWizardPage;
  begin
    Result := TWizardPage(inherited Items[Index]);
  end;

  function TWizardController.GetItemsByID(iID: Integer): TWizardPage;
  var idx: Integer;
  begin
    idx := IndexOfID(iID);
    if idx<0 then PhoaWizardError('Invalid wizard page ID (%d)', [iID]);
    Result := GetItems(idx);
  end;

  function TWizardController.GetVisiblePage: TWizardPage;
  var i: Integer;
  begin
    for i := 0 to Count-1 do begin
      Result := GetItems(i);
      if Result.Visible then Exit;
    end;
    Result := nil;
  end;

  function TWizardController.GetVisiblePageID: Integer;
  var p: TWizardPage;
  begin
    p := GetVisiblePage;
    if p=nil then Result := 0 else Result := p.ID;
  end;

  function TWizardController.IndexOf(Page: TWizardPage): Integer;
  begin
    Result := inherited IndexOf(Page);
  end;

  function TWizardController.IndexOfID(iID: Integer): Integer;
  begin
    for Result := 0 to Count-1 do
      if GetItems(Result).ID=iID then Exit;
    Result := -1;
  end;

  procedure TWizardController.Remove(Page: TWizardPage);
  begin
    inherited Remove(Page);
  end;

  procedure TWizardController.SetKeepHistory(Value: Boolean);
  begin
    if FKeepHistory<>Value then begin
      FKeepHistory := Value;
       // Если не надо хранить, стираем историю
      if not Value and (High(FPageIDHistory)>=0) then begin
        FPageIDHistory := nil;
        FHostFormIntf.StatusChanged;
      end;
    end;
  end;

  procedure TWizardController.SetNextPage;
  var
    CurPage: TWizardPage;
    iNextPageID: Integer;
  begin
    CurPage := GetVisiblePage;
     // Если текущая страница и разрешает смену
    if (CurPage=nil) or CurPage.NextPage then begin
      iNextPageID := FHostFormIntf.NextPageID;
      if iNextPageID>0 then SetVisiblePageID(iNextPageID, pcmNextBtn);
    end;
  end;

  procedure TWizardController.SetPrevPageFromHistory;
  var i, iLastPageID: Integer;
  begin
     // Проверяем наличие записей в истории
    i := High(FPageIDHistory);
    if i=-1 then PhoaWizardError('Page history is empty');
     // Сохраняем ID последней страницы
    iLastPageID := FPageIDHistory[i];
     // Стираем последнюю запись истории
    SetLength(FPageIDHistory, i);
     // Показываем предыдущую страницу (после удаления её из истории, чтобы обновление статуса произошло ПОСЛЕ этого)
    SetVisiblePageID(iLastPageID, pcmBackBtn);
  end;

  function TWizardController.SetVisiblePageID(iNewID: Integer; ChangeMethod: TPageChangeMethod): Boolean;
  var
    CurPage, NewPage: TWizardPage;
    i, iPrevPageID: Integer;
  begin
    Result := False;
    NewPage := ItemsByID[iNewID];
    CurPage := VisiblePage;
    iPrevPageID := GetVisiblePageID;
     // Если страница меняется
    if iPrevPageID<>iNewID then begin
      StartWait;
      try
         // Если текущая страница и обработчик OnPageChanging разрешают смену
        if FHostFormIntf.PageChanging(ChangeMethod, iNewID) then begin
           // Уведомляем текущую страницу
          if CurPage<>nil then CurPage.BeforeHide(ChangeMethod);
           // Уведомляем новую страницу
          NewPage.BeforeDisplay(ChangeMethod);
           // Устанавливаем новую страницу
          for i := 0 to Count-1 do
            with GetItems(i) do Visible := ID=iNewID;
             // Регистрируем страницу в истории
          if FKeepHistory and (iPrevPageID>0) and (ChangeMethod=pcmNextBtn) then begin
            i := High(FPageIDHistory)+1;
            SetLength(FPageIDHistory, i+1);
            FPageIDHistory[i] := iPrevPageID;
          end;
           // Устанавливаем HelpContext
          FHostFormIntf.StorageForm.HelpContext := NewPage.HelpContext;
           // Уведомляем об смене страницы
          FHostFormIntf.PageChanged(ChangeMethod, iPrevPageID);
           // Уведомляем прежнюю страницу
          if CurPage<>nil then CurPage.AfterHide(ChangeMethod);
           // Уведомляем новую страницу
          NewPage.AfterDisplay(ChangeMethod);
           // Уведомляем об изменении статуса
          FHostFormIntf.StatusChanged;
          Result := True;
        end;
      finally
        StopWait;
      end;
    end;
  end;

end.
