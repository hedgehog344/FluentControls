// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Types,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentGraphicControl;

type

  TDialogResult = (drNone, drPrimary, drSecondary, drClose);
  TFluentButtonStyle = (fbsStandard, fbsAccent);


  { TBaseFluentButton }

  TBaseFluentButton = class(TFluentGraphicControl)
  protected
    procedure RenderText(R: TRectF);
    function GetRenderFont: IFont;
    function GetTextColor: TColorB; virtual; abstract;
    procedure DoChangeState(NewState: TRenderState); virtual; abstract;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                {%H-}WithThemeSpace: Boolean); override;
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TFluentHyperlinkButton }

  TFluentHyperlinkButton = class(TBaseFluentButton)
  private
    procedure RenderBack(R: TRectF);
  protected
    procedure Render; override;
    function GetTextColor: TColorB; override;
    procedure DoChangeState(NewState: TRenderState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Cursor default crHandPoint;
    property Enabled;
    property Font;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnChangeBounds;
    property ShowHint;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
  end;

  { TCustomFluentButton }

  TCustomFluentButton = class(TBaseFluentButton)
  private
    FDialogResult: TDialogResult;
    FButtonStyle: TFluentButtonStyle;
    function GetBorderColorFromParent: TColorB;
    procedure RenderBorder(R: TRectF);
    procedure RenderBack(R: TRectF);
    procedure SetButtonStyle(AValue: TFluentButtonStyle);
  protected
    procedure Render; override;
    function GetTextColor: TColorB; override;
    procedure DoChangeState(NewState: TRenderState); override;
    procedure DoChangeToggleState; virtual;
  end;

  { TFluentButton }

  TFluentButton = class(TCustomFluentButton)
  public
    property DialogResult: TDialogResult read FDialogResult write FDialogResult;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Enabled;
    property Font;
    property ButtonStyle: TFluentButtonStyle read FButtonStyle write SetButtonStyle default fbsStandard;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnChangeBounds;
    property ShowHint;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
  end;

  { TFluentToggleButton }

  TFluentToggleButton = class(TCustomFluentButton)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    procedure SetChecked(AValue: boolean);
  protected
    procedure DoChangeToggleState; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Enabled;
    property Font;
    property Checked: boolean read FChecked write SetChecked default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnChangeBounds;
    property ShowHint;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
  end;

implementation

uses
  Graphics, Utils;

{ TFluentHyperlinkButton }

procedure TFluentHyperlinkButton.RenderBack(R: TRectF);
var
  b: byte;
  C: TColorB;
begin
  if (FRenderState = rsNormal) or (FRenderState = rsDisabled) then exit;
  if FRenderState = rsHover then b:=9 else b:=6; // pressed
  C:= Darken(ParentCurrentColor, b/255);
  Surface.FillRoundRect(NewBrush(C), R, Radius);
end;

procedure TFluentHyperlinkButton.Render;
var
  R: TRectF;
begin
  R:= ClientRect;
  RenderBack(R);
  RenderText(R);
  inherited Render;
end;

function TFluentHyperlinkButton.GetTextColor: TColorB;
var
  C: TColorB;
begin
  if Font.Color = clDefault then
    C:= clHighlight
  else
    C:= ColorToRGB(Font.Color);
  if Enabled then
  begin
    if FRenderState = rsHover then Result:= C.Darken(175/255)
    else if FRenderState = rsPressed then Result:= C.Darken(70/255)
    else Result:= C.Darken(115/255);
  end
  else Result:= C.Desaturate(1.0);
end;

procedure TFluentHyperlinkButton.DoChangeState(NewState: TRenderState);
begin
  if FRenderState= NewState then exit;
  FRenderState:= NewState;
  Invalidate;
end;

constructor TFluentHyperlinkButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Cursor:= crHandPoint;
end;

{ TBaseFluentButton }

constructor TBaseFluentButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRenderState:= rsNormal;
  FShowText:= true;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

function TBaseFluentButton.GetRenderFont: IFont;
begin
  Result:= GetIFont;
  Result.Color:= GetTextColor;
end;

procedure TBaseFluentButton.RenderText(R: TrectF);
begin
  Surface.TextOut(GetRenderFont, Caption, R, drCenter);
end;

class function TBaseFluentButton.GetControlClassDefaultSize: TSize;
begin
  Result.cx:= 120;
  Result.cy:= 32;
end;

procedure TBaseFluentButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  Size: TPointI;
begin
  Size:= CalculateTextSize(Caption, GetIFont);
  PreferredWidth:= 2*Scale96ToScreen(2*DefMarginSize) + Size.X;
  PreferredHeight:= 2*Scale96ToScreen(DefMarginSize) + Size.Y;
end;

procedure TBaseFluentButton.MouseEnter;
begin
  inherited MouseEnter;
  DoChangeState(rsHover);
end;

procedure TBaseFluentButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FRenderState = rsHover) and (Button = mbLeft) then
    DoChangeState(rsPressed);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TBaseFluentButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if ClientRect.Contains(Point(X, Y)) and (FRenderState = rsPressed) then
      DoChangeState(rsHover)
    else
      DoChangeState(rsNormal);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TBaseFluentButton.MouseLeave;
begin
  DoChangeState(rsNormal);
  inherited MouseLeave;
end;

{ TFluentToggleButton }

procedure TFluentToggleButton.SetChecked(AValue: boolean);
begin
  if FChecked=AValue then Exit;
  DoChangeToggleState;
  Invalidate;
end;

procedure TFluentToggleButton.DoChangeToggleState;
begin
  FChecked:= not FChecked;
  if FChecked then
    FButtonStyle:=fbsAccent
  else
    FButtonStyle:= fbsStandard;
  if Assigned(FOnChange) then FOnChange(self);
end;

constructor TFluentToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonStyle:= fbsStandard;
  FChecked:= false;
end;

{ TCustomFluentButton }

procedure TCustomFluentButton.DoChangeToggleState;
begin
 //
end;

procedure TCustomFluentButton.DoChangeState(NewState: TRenderState);
begin
  if FRenderState= NewState then exit;
  if (FRenderState=rsPressed) and (NewState=rsHover) then
  begin
    DoChangeToggleState;
  end;
  FRenderState:= NewState;
  Invalidate;
end;

function TCustomFluentButton.GetBorderColorFromParent: TColorB;
begin
  Result:= Darken(ParentCurrentColor, 15/255);
end;

procedure TCustomFluentButton.RenderBorder(R: TRectF);
var
  C: TColorB;
  isPressed: boolean;
begin
  isPressed:= FRenderState > rsHover;
  if FButtonStyle = fbsAccent then
  begin
    if isPressed then exit;
    C:= GetAccentColorB.Darken(15/255);
  end
  else
  begin
    C:= Darken(ParentCurrentColor, 15/255);
    if isPressed then C:= C.Fade(0.6);
  end;

  Surface.StrokeRoundRect(NewPen(C), R, Radius);

  if not isPressed then
  begin
    Surface.MoveTo(R.Left + Radius, R.Bottom );
    Surface.LineTo(R.Right - Radius, R.Bottom );
    Surface.Stroke(NewPen(C.Darken(15/100)));
  end;
end;

procedure TCustomFluentButton.RenderBack(R: TrectF);
var
  C: TColorB;
begin
  C:= GetFillColor(FButtonStyle=fbsAccent);
  Surface.FillRoundRect(NewBrush(C), R, Radius);
end;

function TCustomFluentButton.GetTextColor: TColorB;
begin
  if Font.Color = clDefault then
  begin
    if FButtonStyle = fbsAccent then
      Result:= clHighlightText
    else
      Result:= clWindowText;
  end
  else
    Result:= ColorToRGB(Font.Color);

  if (not Enabled) or (FRenderState = rsPressed) then
    Result:= Result.Fade(0.6);
end;

procedure TCustomFluentButton.SetButtonStyle(AValue: TFluentButtonStyle);
begin
  if FButtonStyle=AValue then Exit;
  FButtonStyle:=AValue;
  Invalidate;
end;

procedure TCustomFluentButton.Render;
var
  R: TRectF;
begin
  R:= ClientRect;
  RenderBack(R);
  RenderBorder(R);
  RenderText(R);
  inherited Render;
end;

end.

