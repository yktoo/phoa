//**********************************************************************************************************************
//  $Id: phPicPropsDlgPage.pas,v 1.6 2004-10-11 11:41:24 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit phPicPropsDlgPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, phIntf, phObj, ConsVars,
  udPicProps, ImgList,
  phWizard;

type
   // Базовый класс странички диалога свойств изображений
  TPicPropsDialogPage = class(TWizardPage)
  private
     // Диалог свойств изображений (владелец страницы)
    FDialog: TdPicProps;
     // Prop handlers 
    function  GetEditedPics: IPhotoAlbumPicList;
    function  GetFileImageIndex(Index: Integer): Integer;
    function  GetPhoA: TPhotoAlbum;
    function  GetFileImages: TImageList;
  protected
    procedure InitializePage; override;
     // Уведомляет форму диалога об изменении
    procedure Modified;
     // Установка/снятие блокировки изменения Modified
    procedure BeginUpdate;
    procedure EndUpdate;
     // Свойства, получаемые через родительский диалог
     // -- Ссылки на редактируемые изображения по индексу
    property EditedPics: IPhotoAlbumPicList read GetEditedPics;
     // -- ImageIndices файлов редактируемых изображений
    property FileImageIndex[Index: Integer]: Integer read GetFileImageIndex;
     // -- ImageList со значками файлов
    property FileImages: TImageList read GetFileImages;
     // -- Фотоальбом
    property PhoA: TPhotoAlbum read GetPhoA;
  public
     // Вызывается диалогом при нажатии кнопки ОК. Должна вернуть True, чтобы позволить закрытие, иначе должна сама
     //   объяснить пользователю причину отказа. В базовом классе всегда возвращает True
    function  CanApply: Boolean; virtual;
     // Вызывается диалогом при нажатии кнопки ОК. Должна добавить операции для применения изменений в предоставляемый
     // список. В базовом классе не делает ничего 
    procedure Apply(FOperations: TPhoaOperations); virtual;
  end;

implementation
{$R *.dfm}

  procedure TPicPropsDialogPage.Apply(FOperations: TPhoaOperations);
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

  function TPicPropsDialogPage.GetPhoA: TPhotoAlbum;
  begin
    Result := FDialog.PhoA;
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
