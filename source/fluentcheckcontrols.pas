// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentCheckControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Types,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentGraphicControl,
  FluentAnimation;

type

  { TCustomFluentCheckCtrl }

  TCustomFluentCheckCtrl = class(TFluentGraphicControl)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    FAnimationController: TAnimationController;
    FAnimateValue: double;
    FBoxSize: integer;
    FBorderWidth: Float;

    procedure DoSetChecked(AValue: boolean); virtual;
    procedure SetChecked(AValue: boolean);
  protected
    procedure Render; override;

    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                {%H-}WithThemeSpace: Boolean); override;
    procedure DoChangeState(NewState: TRenderState); virtual; abstract;
    procedure RenderCircleBox(const R: TRectF); virtual; abstract;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
          X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure RenderText(R: TrectF);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Checked: boolean read FChecked write SetChecked default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TCustomFluentCheckBox }

  TCustomFluentCheckBox = class(TCustomFluentCheckCtrl)
  private
    procedure DoRenderBox(R: TrectF);
    procedure DoRenderBoxChecked(const R: TRectF);
    procedure OnAnimate(aValue: Double);
    procedure DoRenderBorderChecked(const R: TRectF; C: TColorB);
    procedure DoRenderMark(R: TRectF);
    procedure StopAnimation;
  protected
    procedure DoChangeState(NewState: TRenderState); override;
    procedure RenderCircleBox(const R: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TCustomFluentRadioButton }

  TCustomFluentRadioButton = class(TCustomFluentCheckCtrl)
  private

    procedure DoRenderCircleChecked(R: TRectF);
    procedure DoRenderWhiteCircle(R: TRectF);
    procedure DoSetChecked(AValue: boolean); override;
    procedure OnAnimate(aValue: Double);
    procedure DoRenderBorder(R: TrectF; const C: TColorB);
    procedure DoRenderCircle(R: TRectF);
    procedure StartAnimation(NewState: TRenderState; StartValue: double);
    procedure UnCheckOtherRadioBtn;
  protected
    procedure DoChangeState(NewState: TRenderState); override;
    procedure RenderCircleBox(const R: TRectF); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TFluentCheckBox }

  TFluentCheckBox = class(TCustomFluentCheckBox)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Enabled;
    property Font;
    property Checked;
    property OnChange;
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

  { TFluentRadioButton }

  TFluentRadioButton = class(TCustomFluentRadioButton)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Enabled;
    property Font;
    property Checked;
    property OnChange;

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
  Utils;

const
  DefBoxSize = 20;
  DefDarkenBox:     array [TRenderState] of byte = (6, 15, 24, 24);
  DefDarkenBorder:  array [TRenderState] of byte = (116, 118, 64, 64);
  WhiteCircleKoeff: array [TRenderState] of Float = (9/20, 7/20, 49/100, 9/20);


{ TCustomFluentCheckCtrl }

procedure TCustomFluentCheckCtrl.Render;
var
  R: TRectF;
begin
  R:= ClientRect;
  RenderCircleBox(R);
  RenderText(R);
  inherited Render;
end;

procedure TCustomFluentCheckCtrl.DoSetChecked(AValue: boolean);
begin
  if FChecked=AValue then Exit;
  FChecked:=AValue;
  if Assigned(FOnChange) then FOnChange(self);
  Invalidate;
end;

procedure TCustomFluentCheckCtrl.SetChecked(AValue: boolean);
begin
  DoSetChecked(AValue);
end;

class function TCustomFluentCheckCtrl.GetControlClassDefaultSize: TSize;
begin
  Result.cx:= 170;
  Result.cy:= 24;
end;

procedure TCustomFluentCheckCtrl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  Size: TPointI;
  bs, m: integer;
begin
  Size:= CalculateTextSize(Caption, GetIFont);
  bs:= Scale96ToScreen(DefBoxSize);
  m:= Scale96ToScreen(DefMarginSize);
  PreferredWidth:= m + bs + Size.X;
  PreferredHeight:= 5*Size.Y div 3;
  if PreferredHeight< bs then PreferredHeight:= bs;
end;

procedure TCustomFluentCheckCtrl.MouseEnter;
begin
  DoChangeState(rsHover);
  inherited MouseEnter;
end;

procedure TCustomFluentCheckCtrl.MouseLeave;
begin
  inherited MouseLeave;
  DoChangeState(rsNormal);
end;

procedure TCustomFluentCheckCtrl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (FRenderState = rsHover) and (Button = mbLeft) then
    DoChangeState(rsPressed);
end;

procedure TCustomFluentCheckCtrl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button <> mbLeft then exit;
  if ClientRect.Contains(Point(X, Y)) then
    DoChangeState(rsHover)
  else
    DoChangeState(rsNormal);
end;

procedure TCustomFluentCheckCtrl.RenderText(R: TrectF);
var
  S: string;
  F: IFont;
  m: single;
begin
  S:= Caption;
  if S='' then exit;
  F:= GetIFont;
  if FRenderState= rsDisabled then
    F.Color:= Lighten(F.Color, 0.4);
  m:= Scale96ToForm(DefMarginSize);
  if IsRightToLeft then
  begin
    R.Width:= R.Width - FBoxSize - m;
    Surface.TextOut(F, S, R, drRight)
  end
  else
  begin
    R.Left:= R.X+ FBoxSize+m;
    Surface.TextOut(F, S, R, drLeft);
  end;
end;

constructor TCustomFluentCheckCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationController:= TAnimationController.Create;
  FChecked:= false;
  FShowText:= true;
  FRenderState:= rsNormal;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomFluentCheckCtrl.Destroy;
begin
  FAnimationController.Free;
  inherited Destroy;
end;

{ TCustomFluentCheckBox }

procedure TCustomFluentCheckBox.OnAnimate(aValue: Double);
begin
  FAnimateValue:= aValue;
  Invalidate;
end;

procedure TCustomFluentCheckBox.StopAnimation;
begin
  FAnimationController.StopAnimation;
  FAnimateValue:= 0.5;
end;

procedure TCustomFluentCheckBox.DoChangeState(NewState: TRenderState);
begin
  if FRenderState= NewState then exit;
  if (FRenderState=rsPressed) and (NewState=rsHover) then
  begin
    FChecked:= not FChecked;
    if Assigned(FOnChange) then FOnChange(self);
    if FChecked then
    begin
      FAnimationController.StartAnimation(nil, 120, 0, 0.5, @OnAnimate, false);
      FRenderState:= rsHover;
      exit;
    end;
  end;
  StopAnimation;
  FRenderState:= NewState;
  Invalidate;
end;

procedure TCustomFluentCheckBox.DoRenderMark(R: TRectF);
var
  pt1, pt2, pt3: TPointF;
  C: TColorB;
  isClip: boolean;
begin

  pt1.X:= R.Left + R.Width*0.245;
  pt1.Y:= R.Top +  R.Height*0.512;

  pt2.X:= R.Left + R.Width*0.42;
  pt2.Y:= R.Top +  R.Height*0.687;

  pt3.X:= R.Left + R.Width*0.745;
  pt3.Y:= R.Top +  R.Height*0.362;

  isClip:= FAnimateValue<0.5;
  if isClip then
  begin
    Surface.Rectangle(TRectF.Create(pt1.X, R.Top, R.Width*FAnimateValue, R.Height));
    Surface.Path.Clip;
  end;

  Surface.MoveTo(pt1.X, pt1.Y);
  Surface.LineTo(pt2.X, pt2.Y);
  Surface.LineTo(pt3.X, pt3.Y);

  if (FRenderState = rsDisabled) or (FRenderState = rsPressed) then
    C:= DefWhiteColor.Fade(0.7)
  else
    C:= DefWhiteColor;

  Surface.Stroke(NewPen(C, Scale96ToForm(round(130))/100));

  if isClip then Surface.Path.Unclip;
end;

procedure TCustomFluentCheckBox.DoRenderBorderChecked(const R: TRectF;
  C: TColorB);
begin
  if (FRenderState = rsNormal) or (FRenderState = rsDisabled) then exit;
  Surface.StrokeRoundRect(NewPen(C.Darken(9/255), 1), R, Radius);
end;

procedure TCustomFluentCheckBox.DoRenderBox(R: TrectF);
var
  C: TColorB;
begin
  R.Inflate(-FBorderWidth/2, -FBorderWidth/2);
  Surface.RoundRectangle(R, Radius);
  C:= Darken(ParentCurrentColor, DefDarkenBox[FRenderState]/255);
  Surface.Fill(NewBrush(C), true);
  C:= Darken(ParentCurrentColor, DefDarkenBorder[FRenderState]/255);
  Surface.Stroke(NewPen(C, FBorderWidth));
end;

procedure TCustomFluentCheckBox.DoRenderBoxChecked(const R: TRectF);
var
  C: TColorB;
begin
  C:= GetFillColorAccent(FRenderState);
  Surface.FillRoundRect(NewBrush(C), R, Radius);
  DoRenderBorderChecked(R, C);
  DoRenderMark(R);
end;

procedure TCustomFluentCheckBox.RenderCircleBox(const R: TRectF);
var
  BR: TRectF;
begin
  FBoxSize:= Scale96ToForm(DefBoxSize);
  FBorderWidth:= FBoxSize/DefBoxSize;
  if FBorderWidth<1 then FBorderWidth:=1;
  BR:= TRectF.Create(FBoxSize, FBoxSize);
  BR.Y:= R.Y+ (R.Height - FBoxSize)/2;
  if IsRightToLeft then
    BR.X:= R.Right - FBoxSize
  else
    BR.X:= R.X;
  if FChecked then DoRenderBoxChecked(BR)
  else DoRenderBox(BR);
end;

constructor TCustomFluentCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimateValue:=0.5;
end;

{ TCustomFluentRadioButton }

procedure TCustomFluentRadioButton.OnAnimate(aValue: Double);
begin
  FAnimateValue:= aValue;
  Invalidate;
end;

procedure TCustomFluentRadioButton.RenderCircleBox(const R: TrectF);
var
  BR: TRectF;
begin
  FBoxSize:= Scale96ToForm(DefBoxSize);
  FBorderWidth:= FBoxSize/DefBoxSize;
  if FBorderWidth<1 then FBorderWidth:=1;
  BR:= TRectF.Create(FBoxSize, FBoxSize);
  BR.Y:= R.Y+ (R.Height - FBoxSize)/2;
  if IsRightToLeft then
    BR.X:= R.Right - FBoxSize
  else
    BR.X:= R.X;
  if FChecked then
    DoRenderCircleChecked(BR)
  else
    DoRenderCircle(BR);
end;

procedure TCustomFluentRadioButton.DoRenderCircleChecked(R: TRectF);
var
  C: TColorB;
begin
  C:= GetFillColorAccent(FRenderState);
  Surface.Ellipse(R);
  Surface.Fill(NewBrush(C));
  if FRenderState <> rsNormal then
    DoRenderBorder(R, C.Darken(30/255));
  DoRenderWhiteCircle(R);
end;

procedure TCustomFluentRadioButton.DoRenderBorder(R: TrectF; const C: TColorB);
begin
  R.Inflate(-FBorderWidth/2, -FBorderWidth/2);
  Surface.Ellipse(R);
  Surface.Stroke(NewPen(C, FBorderWidth));
end;

procedure TCustomFluentRadioButton.DoRenderWhiteCircle(R: TRectF);
var
  d: float;
begin
  d:= -0.5*FAnimateValue*FBoxSize;
  R.Inflate(d, d);
  Surface.Ellipse(R);
  Surface.Fill(NewBrush(DefWhiteColor));
end;

procedure TCustomFluentRadioButton.DoRenderCircle(R: TRectF);
var
  C, PC: TColorB;
begin
  PC:=ParentCurrentColor;
  C:= Darken(PC, DefDarkenBox[FRenderState]/255);
  Surface.Ellipse(R);
  Surface.Fill(NewBrush(C));
  DoRenderBorder(R, Darken(PC, DefDarkenBorder[FRenderState]/255));
  if FRenderState = rsPressed then
    DoRenderWhiteCircle(R);
end;



procedure TCustomFluentRadioButton.DoChangeState(NewState: TRenderState);

  procedure DoAnimate;
  begin
    StartAnimation(NewState, WhiteCircleKoeff[FRenderState]);
    FRenderState:= NewState;
  end;

begin
  if FRenderState= NewState then exit;
  if FChecked then
  begin
    DoAnimate;
    exit;
  end;
  if (NewState=rsHover) and (FRenderState=rsPressed) then
  begin
    DoSetChecked(true);
    DoAnimate;
    exit;
  end;

  if (NewState=rsPressed) then
  begin

    StartAnimation(NewState, 0.95);
    FRenderState:= NewState;
    exit;
  end;

  FAnimationController.StopAnimation;
  FRenderState:= NewState;
  Invalidate;
end;

constructor TCustomFluentRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimateValue:= WhiteCircleKoeff[rsNormal];
end;

procedure TCustomFluentRadioButton.UnCheckOtherRadioBtn;
var
  i, n: integer;
  Control: TControl;
begin
  if Parent=nil then exit;
  n:= Parent.ControlCount;
  for i:=0 to n-1 do
  begin
    Control:= Parent.Controls[i];
    if (Control <> self) and (Control is TCustomFluentRadioButton) then
      TCustomFluentRadioButton(Control).Checked:= false;
  end;
end;

procedure TCustomFluentRadioButton.DoSetChecked(AValue: boolean);
begin
  inherited DoSetChecked(AValue);
  if AValue then UncheckOtherRadioBtn;
end;

procedure TCustomFluentRadioButton.StartAnimation(NewState: TRenderState; StartValue: double);
begin
  FAnimationController.StartAnimation(nil, 120, StartValue, WhiteCircleKoeff[NewState], @OnAnimate, false);
end;

end.

