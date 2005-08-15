//**********************************************************************************************************************
//  $Id: ufrWzPage_Log.pas,v 1.13 2005-08-15 11:25:11 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
unit ufrWzPage_Log;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ConsVars, 
  phWizard, VirtualTrees, ImgList, TB2Item, TBX, Menus, ActnList, StdCtrls, ExtCtrls, DKLang;

type
  TfrWzPage_Log = class(TWizardPage)
    tvMain: TVirtualStringTree;
    pmMain: TTBXPopupMenu;
    ipmSaveToFile: TTBXItem;
    pBottom: TPanel;
    cbErrorsOnly: TCheckBox;
    bSaveToFile: TButton;
    alMain: TActionList;
    aSaveToFile: TAction;
    aDisplayErrorsOnly: TAction;
    ipmDisplayErrorsOnly: TTBXItem;
    ipmSep: TTBXSeparatorItem;
    lInfo: TLabel;
    aCopy: TAction;
    aFind: TAction;
    ipmFind: TTBXItem;
    ipmCopy: TTBXItem;
    ipmSep2: TTBXSeparatorItem;
    fdMain: TFindDialog;
    dklcMain: TDKLanguageController;
    procedure tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvMainBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure aaSaveToFile(Sender: TObject);
    procedure aaDisplayErrorsOnly(Sender: TObject);
    procedure aaCopy(Sender: TObject);
    procedure aaFind(Sender: TObject);
    procedure fdMainFind(Sender: TObject);
  private
     // Возвращает True, если узел отображает строку об ошибке
    function  IsNodeError(Node: PVirtualNode): Boolean;
     // Возвращает ссылку на протокол (через предметный интерфейс host-формы)
    function  GetLog: TStrings;
     // Осуществляет поиск записи протокола
    procedure DoFind(const sPattern: String; bDown, bMatchCase: Boolean);
  protected
    procedure DoCreate; override;
    procedure BeforeDisplay(ChangeMethod: TPageChangeMethod); override;
  end;

implementation
{$R *.dfm}
uses phUtils, Main, Clipbrd, phSettings, phMsgBox;

  procedure TfrWzPage_Log.aaCopy(Sender: TObject);
  var n: PVirtualNode;
  begin
    n := tvMain.FocusedNode;
    if n<>nil then Clipboard.AsText := GetLog[n.Index];
  end;

  procedure TfrWzPage_Log.aaDisplayErrorsOnly(Sender: TObject);
  var
    n: PVirtualNode;
    b: Boolean;
  begin
    tvMain.BeginUpdate;
    try
      b := aDisplayErrorsOnly.Checked;
      n := tvMain.GetFirst;
      while n<>nil do begin
        tvMain.IsVisible[n] := not b or IsNodeError(n);
        n := tvMain.GetNext(n);
      end;
    finally
    	tvMain.EndUpdate;
    end;
  end;

  procedure TfrWzPage_Log.aaFind(Sender: TObject);
  begin
    fdMain.Execute;
  end;

  procedure TfrWzPage_Log.aaSaveToFile(Sender: TObject);
  begin
    with TSaveDialog.Create(Self) do
      try
        DefaultExt  := 'txt';
        Options     := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
        if Execute then GetLog.SaveToFile(FileName);
      finally
        Free;
      end;
  end;

  procedure TfrWzPage_Log.BeforeDisplay(ChangeMethod: TPageChangeMethod);
  var
    Log: TStrings;
    i, iErrCount: Integer;
  begin
    if ChangeMethod=pcmNextBtn then begin
      Log := GetLog;
      tvMain.RootNodeCount := Log.Count;
       // Считаем количество ошибок
      iErrCount := 0;
      for i := 0 to Log.Count-1 do
        if Log[i][2]='!' then Inc(iErrCount);
       // Отображаем информацию
      lInfo.Caption := ConstVal(iif(iErrCount=0, 'SLogInfoNoErrors', 'SLogInfoWithErrors'), [Log.Count, iErrCount]);
    end;
  end;

  procedure TfrWzPage_Log.DoCreate;
  begin
    inherited DoCreate;
    ApplyTreeSettings(tvMain);
  end;

  procedure TfrWzPage_Log.DoFind(const sPattern: String; bDown, bMatchCase: Boolean);
  var
    n: PVirtualNode;
    GetProc: TGetNextNodeProc;
    Log: TStrings;
    sUpPattern: String;

    function Matches(const s: String): Boolean;
    var sMatch: String;
    begin
      if bMatchCase then sMatch := s else sMatch := AnsiUpperCase(s);
      Result := Pos(sUpPattern, sMatch)>0;
    end;

  begin
     // Кэшируем данные для поиска
    Log := GetLog;
    if bMatchCase then sUpPattern := sPattern else sUpPattern := AnsiUpperCase(sPattern);
     // Определяем процедуру движения по узлам
    if bDown then GetProc := tvMain.GetNextVisible else GetProc := tvMain.GetPreviousVisible;
     // Цикл по узлам
    n := tvMain.FocusedNode;
    if n<>nil then
      repeat n := GetProc(n);
      until (n=nil) or Matches(Copy(Log[n.Index], 5, MaxInt));
     // Проверяем: нашли или нет
    if n=nil then PhoaInfo(False, 'SInfo_SearchStringNotFound') else ActivateVTNode(tvMain, n);
  end;

  procedure TfrWzPage_Log.fdMainFind(Sender: TObject);
  begin
    DoFind(fdMain.FindText, frDown in fdMain.Options, frWholeWord in fdMain.Options);
  end;

  function TfrWzPage_Log.GetLog: TStrings;
  begin
    Result := (StorageForm as IPhoaWizardPageHost_Log).Log[ID];
  end;

  function TfrWzPage_Log.IsNodeError(Node: PVirtualNode): Boolean;
  begin
    Result := GetLog[Node.Index][2]='!';
  end;

  procedure TfrWzPage_Log.tvMainBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
  begin
    if IsNodeError(Node) then begin
      EraseAction := eaColor;
      ItemColor   := $d0d0ff;
    end;
  end;

  procedure TfrWzPage_Log.tvMainGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
  begin
    if Column=1 then ImageIndex := iif(IsNodeError(Node), iiError, iiOK);
  end;

  procedure TfrWzPage_Log.tvMainGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  begin
    case Column of
       // Порядковый номер
      0: CellText := IntToStr(Node.Index+1);
       // Текст записи протокола
      1: CellText := PhoaAnsiToUnicode(Copy(GetLog[Node.Index], 5, MaxInt));
    end;
  end;

end.
