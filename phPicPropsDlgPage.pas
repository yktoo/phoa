//**********************************************************************************************************************
//  $Id: phPicPropsDlgPage.pas,v 1.9 2004-10-19 15:03:31 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPicPropsDlgPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  phIntf, phMutableIntf, phNativeIntf, phObj, phOps, ConsVars,
  udPicProps, ImgList,
  phWizard;

type
   // Базовый класс странички диалога свойств изображений
  TPicPropsDialogPage = class(TWizardPage)
  private
     // Диалог свойств изображений (владелец страницы)
    FDialog: TdPicProps;
     // Prop handlers 
    function  GetApp: IPhotoAlbumApp;
    function  GetEditedPics: IPhotoAlbumPicList;
    function  GetFileImageIndex(Index: Integer): Integer;
    function  GetFileImages: TImageList;
  protected
    procedure InitializePage; override;
     // Уведомляет форму диалога об изменении
    procedure Modified;
     // Установка/снятие блокировки изменения Modified
    procedure BeginUpdate;
    procedure EndUpdate;
     // Свойства, получаемые через родительский диалог
     // -- Приложение
    property App: IPhotoAlbumApp read GetApp;
     // -- Ссылки на редактируемые изображения по индексу
    property EditedPics: IPhotoAlbumPicList read GetEditedPics;
     // -- ImageIndices файлов редактируемых изображений
    property FileImageIndex[Index: Integer]: Integer read GetFileImageIndex;
     // -- ImageList со значками файлов
    property FileImages: TImageList read GetFileImages;
  public
     // Вызывается диалогом при нажатии кнопки ОК. Должна вернуть True, чтобы позволить закрытие, иначе должна сама
     //   объяснить пользователю причину отказа. В базовом классе всегда возвращает True
    function  CanApply: Boolean; virtual;
     // Вызывается диалогом при нажатии кнопки ОК. Должна вернуть имя параметра - дочерней операции, а также набор её
     //   параметров для применения изменений. В базовом классе не делает ничего
    procedure Apply(var sOpParamName: String; var OpParams: IPhoaOperationParams); virtual;
  end;

implementation
{$R *.dfm}

  procedure TPicPropsDialogPage.Apply(var sOpParamName: String; var OpParams: IPhoaOperationParams);
  begin
    { does nothing }
  end;

  procedure TPicPropsDialogPage.BeginUpdate;
  begin
    FDialog.BeginUpdate;
  end;

  function TPicPropsDialogPage.CanApply: Boolean;
  begin
    Result := True;
  end;

  procedure TPicPropsDialogPage.EndUpdate;
  begin
    FDialog.EndUpdate;
  end;

  function TPicPropsDialogPage.GetApp: IPhotoAlbumApp;
  begin
    Result := FDialog.App;
  end;

  function TPicPropsDialogPage.GetEditedPics: IPhotoAlbumPicList;
  begin
    Result := FDialog.EditedPics;
  end;

  function TPicPropsDialogPage.GetFileImageIndex(Index: Integer): Integer;
  begin
    Result := FDialog.FileImageIndex[Index];
  end;

  function TPicPropsDialogPage.GetFileImages: TImageList;
  begin
    Result := FDialog.ilFiles;
  end;

  procedure TPicPropsDialogPage.InitializePage;
  begin
    inherited InitializePage;
    FDialog := StorageForm as TdPicProps;
  end;

  procedure TPicPropsDialogPage.Modified;
  begin
    FDialog.Modified := True;
  end;

end.
