//**********************************************************************************************************************
//  $Id: phPicPropsDlgPage.pas,v 1.2 2004-04-15 12:54:10 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 Dmitry Kann, http://phoa.narod.ru
//**********************************************************************************************************************
unit phPicPropsDlgPage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars, phObj, udPicProps, ImgList,
  phWizard;

type
   // Базовый класс странички диалога свойств изображений
  TPicPropsDialogPage = class(TWizardPage)
  private
    FDialog: TdPicProps;
    function GetEditedPicCount: Integer;
    function GetEditedPics(Index: Integer): TPhoaPic;
    function GetFileImageIndex(Index: Integer): Integer;
    function GetPhoA: TPhotoAlbum;
    function GetFileImages: TImageList;
    function GetEditedPicArray: TPicArray;
  protected
    procedure InitializePage; override;
     // Уведомляет форму диалога об изменении
    procedure Modified;
     // Свойства, получаемые через родительский диалог
     // -- Количество редактируемых изображений
    property EditedPicCount: Integer read GetEditedPicCount;
     // -- Массив ссылок на редактируемые изображения
    property EditedPicArray: TPicArray read GetEditedPicArray;
     // -- Ссылки на редактируемые изображения по индексу
    property EditedPics[Index: Integer]: TPhoaPic read GetEditedPics;
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

  function TPicPropsDialogPage.CanApply: Boolean;
  begin
    Result := True;
  end;

  function TPicPropsDialogPage.GetEditedPicArray: TPicArray;
  begin
    Result := FDialog.EditedPics;
  end;

  function TPicPropsDialogPage.GetEditedPicCount: Integer;
  begin
    Result := High(FDialog.EditedPics)+1;
  end;

  function TPicPropsDialogPage.GetEditedPics(Index: Integer): TPhoaPic;
  begin
    Result := FDialog.EditedPics[Index];
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
