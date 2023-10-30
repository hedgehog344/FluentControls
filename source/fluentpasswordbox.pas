// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentPasswordBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics,

  Codebot.System,
  CustomTextBox,
  FluentIconButton;

type

  { TCustomPasswordBox }

  TCustomPasswordBox = class(TCustomTextBox)
  private
    FPasswordChar: string;
    FHidePassword: boolean;
    FButton: TCustomFluentIconButton;

    procedure ButtonMouseDown(Sender: TObject; Button: TMouseButton;
                          {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ButtonMouseUp(Sender: TObject; Button: TMouseButton;
                          {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure DoArrangeButton(aValue: boolean);
    procedure DoSetPasswordChar(isHide: boolean);
    procedure DoVisibleButton(aValue: boolean);
    function GetPassword: string;
    procedure SetHidePassword(AValue: boolean);
    procedure SetPassword(AValue: string);
    procedure SetPasswordChar(AValue: string);
  protected
    procedure DoChangeEdit; override;
    function GetRightSpace: integer; override;
    function GetLeftSpace: integer; override;
    procedure SetBiDiMode(AValue: TBiDiMode); override;
    procedure FontChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ChildHandlesCreated; override;
    property PasswordChar: string read FPasswordChar write SetPasswordChar;
    property HidePassword: boolean read FHidePassword write SetHidePassword default true;
    property Text;
    property Password: string read GetPassword write SetPassword;
  end;

  { TPasswordBox }

  TFluentPasswordBox = class(TCustomPasswordBox)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property Enabled;
    property Font;
    property Header;
    property MaxLength;

    property PasswordChar;
    property PlaceholderText;
    property HidePassword;
    property Password;




    property ParentBiDiMode;
    property ParentFont;
    property Visible;

    property OnChange;
    property Color;
    property Cursor;
    property Hint;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property ParentColor;
    property ParentShowHint;

    property ShowHint;

    property TabOrder;
    property TabStop;


  end;

implementation

{ TCustomPasswordBox }

{$IfDef Windows}
uses
  Windows, LazUTF8, Codebot.Graphics.Types;

function GetWParamFromString(s: string): WPARAM;
var
  ws: UnicodeString;
begin
  ws:= UTF8ToUTF16(s);
  Result:=WPARAM(ws[1]);
end;

procedure TCustomPasswordBox.DoSetPasswordChar(isHide: boolean);
var
  param: WPARAM = 0;
begin
  if isHide then
  begin
    if FPasswordChar = '' then
      param:=$25CF //default '●'
    else
      param:= GetWParamFromString(FPasswordChar);
  end;
  SendMessageW(Edit.Handle, EM_SETPASSWORDCHAR, param, 0);
  Invalidate;
end;

{$Else}

procedure TCustomPasswordBox.DoSetPasswordChar(isHide: boolean);
var
  ch: char = #0;
begin
  if isHide then
  begin
    if FPasswordChar = '' then
      ch:= '●'
    else
      ch:= FPasswordChar[1];
  end
  Edit.PasswordChar:= ch;
  Invalidate;
end;
{$EndIf}

procedure TCustomPasswordBox.SetPasswordChar(AValue: string);
begin
  if FPasswordChar=AValue then Exit;
  FPasswordChar:=AValue; //'●'
  if FHidePassword then
    DoSetPasswordChar(true);
end;

function TCustomPasswordBox.GetPassword: string;
begin
  Result:= Text;
end;

procedure TCustomPasswordBox.SetHidePassword(AValue: boolean);
begin
  if FHidePassword=AValue then Exit;
  FHidePassword:=AValue;
  DoSetPasswordChar(FHidePassword);
  DoVisibleButton(FHidePassword and (Length(Edit.Text)>0));
end;

procedure TCustomPasswordBox.SetPassword(AValue: string);
begin
  Text:= AValue;
end;

procedure TCustomPasswordBox.DoArrangeButton(aValue: boolean);
var
  p: integer;
begin
  if aValue = FButton.Visible then exit;
  if aValue then
  begin
    FButton.Width:= Scale96ToScreen(28);
    FButton.Height:= Scale96ToScreen(24);
    FButton.Top:= (Height - FButton.Height) div 2;
    P:= Scale96ToScreen(4);
    if IsRightToLeft then
      FButton.Left:= P
    else
      FButton.Left:= ClientWidth - FButton.Width - P;
  end;
end;

procedure TCustomPasswordBox.DoVisibleButton(aValue: boolean);
begin
  DoArrangeButton(aValue);
  FButton.Visible:= aValue;
  DoArrangeCtrls;
end;

procedure TCustomPasswordBox.DoChangeEdit;
begin
  if FHidePassword then
    DoVisibleButton(Length(Edit.Text)> 0);
  inherited DoChangeEdit;
end;

function TCustomPasswordBox.GetRightSpace: integer;
begin
  Result:=inherited GetRightSpace;
  if FButton.Visible and (not IsRightToLeft)  then
    Result+= FButton.Width;
end;

function TCustomPasswordBox.GetLeftSpace: integer;
begin
  Result:=inherited GetLeftSpace;
  if IsRightToLeft and FButton.Visible then
    Result+= FButton.Width;
end;

procedure TCustomPasswordBox.SetBiDiMode(AValue: TBiDiMode);
begin
  inherited SetBiDiMode(AValue);
  if FButton.Visible then
    DoVisibleButton(true);
end;

procedure TCustomPasswordBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FButton.PathIcon.ForegroundColor:= Lighten(Font.Color, 80/255).Color;
end;

procedure TCustomPasswordBox.ButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FHidePassword then
    DoSetPasswordChar(false);
end;

procedure TCustomPasswordBox.ButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FHidePassword then
    DoSetPasswordChar(true);
end;

constructor TCustomPasswordBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPasswordChar:='●';
  FHidePassword:= true;
  FButton:= TCustomFluentIconButton.Create(self);
  FButton.PathIcon.CodePoint:= $E7B3; // RedEye
  FButton.PathIcon.ForegroundColor:= $505050;
  FButton.Color:= clWhite;
  FButton.OnMouseDown:= @ButtonMouseDown;
  FButton.OnMouseUp:= @ButtonMouseUp;
  FButton.Parent:= self;
  FButton.Visible:= false;
end;

procedure TCustomPasswordBox.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  if FHidePassword then DoSetPasswordChar(true);
  FButton.Anchors:=[akRight];

end;

end.

