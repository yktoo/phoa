//**********************************************************************************************************************
//  $Id: phActionImpl.pas,v 1.1 2005-02-26 12:35:51 dale Exp $
//----------------------------------------------------------------------------------------------------------------------
//  PhoA image arranging and searching tool
//  Copyright DK Software, http://www.dk-soft.org/
//**********************************************************************************************************************
// Typical but still abstract IPhoaAction implementation

unit phActionImpl;

interface
uses phIntf, phAppIntf, phPlugin;

   //===================================================================================================================
   // TPhoaPluginAction - an abstract IPhoaAction implementation. Be sure to override Execute (and GetEnabled if
   //   necessary) methods
   //===================================================================================================================
type
  TPhoaPluginAction = class(TInterfacedObject, IPhoaAction)
  private
     // Prop storage
    FCaption: String;
    FCategory: String;
    FChangeNotificationProc: TPhoaActionChangeNotificationProc;
    FHint: String;
    FName: String;
    FPlugin: IPhoaPlugin;
    FTag: Integer;
     // IPhoaAction
    procedure SetChangeNotification(Proc: TPhoaActionChangeNotificationProc); stdcall;
    function  GetCaption: String; stdcall;
    function  GetCaptionW: WideString; stdcall;
    function  GetCategory: String; stdcall;
    function  GetCategoryW: WideString; stdcall;
    function  GetHint: String; stdcall;
    function  GetHintW: WideString; stdcall;
    function  GetName: String; stdcall;
    function  GetNameW: WideString; stdcall;
    function  GetTag: Integer; stdcall;
    procedure SetCaption(const Value: String); stdcall;
    procedure SetCaptionW(const Value: WideString); stdcall;
    procedure SetCategory(const Value: String); stdcall;
    procedure SetCategoryW(const Value: WideString); stdcall;
    procedure SetHint(const Value: String); stdcall;
    procedure SetHintW(const Value: WideString); stdcall;
    procedure SetTag(Value: Integer); stdcall;
  protected
    function  Execute: LongBool; virtual; stdcall; abstract;
    function  GetEnabled: LongBool; virtual; stdcall;
    procedure SetEnabled(Value: LongBool); virtual; stdcall;
     // Called whenever something was changed to the action. By default calls change notification callback routine
    procedure DoChangeNotification; virtual;
  public
    constructor Create(APlugin: IPhoaPlugin); overload;
    constructor Create(APlugin: IPhoaPlugin; const sName, sCategory, sCaption, sHint: String); overload;
     // Props
     // -- An optional plugin instance associated with the action, passed to the constructor
    property Plugin: IPhoaPlugin read FPlugin;
  end;

implementation

  constructor TPhoaPluginAction.Create(APlugin: IPhoaPlugin);
  begin
    inherited Create;
    FPlugin := APlugin;
  end;

  constructor TPhoaPluginAction.Create(APlugin: IPhoaPlugin; const sName, sCategory, sCaption, sHint: String);
  begin
    Create(APlugin);
    FName     := sName;
    FCategory := sCategory;
    FCaption  := sCaption;
    FHint     := sHint;
  end;

  procedure TPhoaPluginAction.DoChangeNotification;
  begin
    if Assigned(FChangeNotificationProc) then FChangeNotificationProc(Self);
  end;

  function TPhoaPluginAction.GetCaption: String;
  begin
    Result := FCaption;
  end;

  function TPhoaPluginAction.GetCaptionW: WideString;
  begin
     // Warning: user-default code page ANSI-to-Unicode conversion occurs!
    Result := FCaption; 
  end;

  function TPhoaPluginAction.GetCategory: String;
  begin
    Result := FCategory;
  end;

  function TPhoaPluginAction.GetCategoryW: WideString;
  begin
     // Warning: user-default code page ANSI-to-Unicode conversion occurs!
    Result := FCategory;
  end;

  function TPhoaPluginAction.GetEnabled: LongBool;
  begin
     // Always return True by default. Override this to accommodate application state changes
    Result := True;
  end;

  function TPhoaPluginAction.GetHint: String;
  begin
    Result := FHint;
  end;

  function TPhoaPluginAction.GetHintW: WideString;
  begin
     // Warning: user-default code page ANSI-to-Unicode conversion occurs!
    Result := FHint;
  end;

  function TPhoaPluginAction.GetName: String;
  begin
    Result := FName;
  end;

  function TPhoaPluginAction.GetNameW: WideString;
  begin
     // Warning: user-default code page ANSI-to-Unicode conversion occurs!
    Result := FName;
  end;

  function TPhoaPluginAction.GetTag: Integer;
  begin
    Result := FTag;
  end;

  procedure TPhoaPluginAction.SetCaption(const Value: String);
  begin
    if FCaption<>Value then begin
      FCaption := Value;
      DoChangeNotification;
    end;
  end;

  procedure TPhoaPluginAction.SetCaptionW(const Value: WideString);
  begin
     // Warning: user-default code page Unicode-to-ANSI conversion occurs!
    SetCaption(Value);
  end;

  procedure TPhoaPluginAction.SetCategory(const Value: String);
  begin
    if FCategory<>Value then begin
      FCategory := Value;
      DoChangeNotification;
    end;
  end;

  procedure TPhoaPluginAction.SetCategoryW(const Value: WideString);
  begin
     // Warning: user-default code page Unicode-to-ANSI conversion occurs!
    SetCategory(Value);
  end;

  procedure TPhoaPluginAction.SetChangeNotification(Proc: TPhoaActionChangeNotificationProc);
  begin
    FChangeNotificationProc := Proc;
  end;

  procedure TPhoaPluginAction.SetEnabled(Value: LongBool);
  begin
     // Simply notify the application if Value differs. Override to perform appropriate actions
    if Value<>GetEnabled then DoChangeNotification;
  end;

  procedure TPhoaPluginAction.SetHint(const Value: String);
  begin
    if FHint<>Value then begin
      FHint := Value;
      DoChangeNotification;
    end;
  end;

  procedure TPhoaPluginAction.SetHintW(const Value: WideString);
  begin
     // Warning: user-default code page Unicode-to-ANSI conversion occurs!
    SetHint(Value);
  end;

  procedure TPhoaPluginAction.SetTag(Value: Integer);
  begin
    FTag := Value;
  end;

end.
