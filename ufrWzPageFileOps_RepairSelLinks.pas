//**********************************************************************************************************************
//  $Id: ufrWzPageFileOps_RepairSelLinks.pas,v 1.8 2004-10-06 15:28:52 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright 2002-2004 DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPageFileOps_RepairSelLinks;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, phIntf, ConsVars,
  phWizard, StdCtrls, ExtCtrls, Mask, VirtualTrees,
  DKLang;

type
  TfrWzPageFileOps_RepairSelLinks = class(TWizardPage)
    tvMain: TVirtualStringTree;
    pBottom: TPanel;
    cbDeleteUnmatched: TCheckBox;
    dklcMain: TDKLanguageController;
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  private
  protected
    procedure InitializePage; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
    function  NextPage: Boolean; override;
  end;

implementation
{$R *.dfm}
uses phUtils, udFileOpsWizard, phObj, Main;

  procedure TfrWzPageFileOps_RepairSelLinks.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var Wiz: TdFileOpsWizard;
  begin
    inherited BeforeDisplay(ChangeMethod);
    Wiz := TdFileOpsWizard(StorageForm);
     // Инициализируем опции
    tvMain.Clear;
    tvMain.RootNodeCount      := Wiz.SelectedPics.Count;
    cbDeleteUnmatched.Checked := Wiz.Repair_DeleteUnmatchedPics;
  end;

  procedure TfrWzPageFileOps_RepairSelLinks.InitializePage;
  begin
    inherited InitializePage;
    tvMain.NodeDataSize := SizeOf(Integer);
  end;

  function TfrWzPageFileOps_RepairSelLinks.NextPage: Boolean;
  var Wiz: TdFileOpsWizard;
  begin
    Result := inherited NextPage;
    if Result then begin
       // Сохраняем опции
      Wiz := TdFileOpsWizard(StorageForm);
      Wiz.Repair_DeleteUnmatchedPics := cbDeleteUnmatched.Checked;
    end;
  end;

  procedure TfrWzPageFileOps_RepairSelLinks.tvMainBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
  begin
     // Для корневого элемента (изображения) устанавливаем цвет фона clBtnFace
    if Sender.NodeParent[Node]=nil then
      with TargetCanvas do begin
        Brush.Color := clBtnFace;
        FillRect(CellRect);
      end;
  end;

  procedure TfrWzPageFileOps_RepairSelLinks.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
     // У изображений без найденного соответствия рисуем знак запрещения
    if (Column=0) and (Sender.NodeParent[Node]=nil) and (Sender.ChildCount[Node]=0) then ImageIndex := iiNo;
  end;

  procedure TfrWzPageFileOps_RepairSelLinks.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  var
    s: String;
    Pic: IPhotoAlbumPic;
    FL: TFileLink;
  begin
    s := '';
     // Исходное изображение
    if Sender.NodeParent[Node]=nil then begin
      Pic := PPhoaPic(Sender.GetNodeData(Node))^;
      case Column of
        0: s := Pic.Props[ppFileName];
        1: s := Pic.Props[ppFilePath];
        2: s := HumanReadableSize(Pic.FileSize);
      end;
     // Найденный файл
    end else begin
      FL := PFileLink(Sender.GetNodeData(Node))^;
      case Column of
        0: s := FL.FileName;
        1: s := FL.FilePath;
        2: s := HumanReadableSize(FL.FileSize);
        3: s := DateTimeToStr(FL.FileTime);
      end;
    end;
    CellText := AnsiToUnicodeCP(s, cMainCodePage);
  end;

  procedure TfrWzPageFileOps_RepairSelLinks.tvMainInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var
    Wiz: TdFileOpsWizard;
    i: Integer;
    Pic: TPhoaPic;
    FL: TFileLink;
  begin
    Wiz := TdFileOpsWizard(StorageForm);
     // *** Исходное изображение
    if ParentNode=nil then begin
       // Node.Data = ссылка на изображение из SelectedPics
      Pic := TPhoaPic(Wiz.SelectedPics[Node.Index].Handle);
      PPhoaPic(Sender.GetNodeData(Node))^ := Pic;
       // Добавляем дочерние узлы файлов, ссылающихся на изображение. Их Node.Data = ссылка на файл из Repair_FileLinks
      for i := 0 to Wiz.Repair_FileLinks.Count-1 do begin
        FL := Wiz.Repair_FileLinks[i];
        if FL.Pics.IndexOfID(Pic.ID)>=0 then tvMain.AddChild(Node, FL);
      end;
       // Ставим изображению CheckBox
      if Node.ChildCount>0 then begin
        tvMain.CheckType[Node]  := ctCheckBox;
        tvMain.CheckState[Node] := csCheckedNormal;
      end;
       // Разворачиваем все узлы
      Include(InitialStates, ivsExpanded);
     // *** Найденный файл
    end else begin
       // Устанавливаем ему радиокнопку
      Sender.CheckType[Node]  := ctRadioButton;
//!!!      Sender.CheckState[Node] := ctRadioButton;
    end;
  end;

end.

