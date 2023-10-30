// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentGraphicControl,
  FluentAnimation;

type

  TProgressStyle = (psIndeterminate, psDeterminate, psPaused, psError);

  { TBaseProgressControl }

  TBaseProgressControl = class(TFluentGraphicControl)
  private
    FMax: Double;
    FMin: Double;
    FValue: Double;
    FRenderRect: TRectF;
    FPenColor: TColorB;
    FStrokeWidth: Float;
    FProgressStyle: TProgressStyle;

    FAnimationValue: Double;
    FAnimationController: TAnimationController;

    procedure DoCalcStrokeColor;
    function NewRenderPen: IPen;
    procedure SetMax(NewValue: Double);
    procedure SetMin(NewValue: Double);
    procedure SetProgressStyle(AValue: TProgressStyle);
    procedure SetValue(NewValue: Double);
    procedure SetNewValueAnimation;
    procedure OnAnimateValue(AValue: Double);
  protected
    procedure Render; override; final;
    procedure DoInitRender(const R: TRect); virtual; abstract;
    procedure DoWarningErrorRender; virtual; abstract;
    procedure DoDeterminateRender; virtual; abstract;
    procedure DoIndeterminateRender; virtual; abstract;
    property Max: Double read FMax write SetMax;
    property Min: Double read FMin write SetMin;
    property Value: Double read FValue write SetValue;
    property ProgressStyle: TProgressStyle
     read FProgressStyle write SetProgressStyle default psDeterminate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TFluentProgressRing }

  TFluentProgressRing = class(TBaseProgressControl)
  private
    FBackgroundColor: TColor;
    procedure DoBackgroundRender;
    procedure DoCircleRender;
    procedure SetBackgroundColor(AValue: TColor);
  protected
    procedure DoInitRender(const R: TRect); override;
    procedure DoWarningErrorRender; override;
    procedure DoDeterminateRender; override;
    procedure DoIndeterminateRender; override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property BorderSpacing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Max;
    property Min;
    property ParentShowHint;
    property PopupMenu;
    property ProgressStyle;
    property Value;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnShowHint;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TFluentProgressBar }

  TFluentProgressBar = class(TBaseProgressControl)

  private
    procedure DoLineRender(X1, X2: Float);
  protected
    procedure DoInitRender(const R: TRect); override;
    procedure DoWarningErrorRender; override;
    procedure DoDeterminateRender; override;
    procedure DoIndeterminateRender; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         {%H-}WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Max;
    property Min;
    property ParentShowHint;
    property PopupMenu;
    property ProgressStyle;
    property Value;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnShowHint;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses
  Utils;

const
  DefPausedColor: TColorB =  (Blue: 0;   Green: $5D; Red: $9D; Alpha: $FF); // TColor($005D9D);     // brown
  DefErrorColor: TColorB  =  (Blue: $1C; Green: $3B; Red: $C4; Alpha: $FF); // TColor($1C2BC4);     // red


{ TFluentProgressRing }

procedure TFluentProgressRing.DoInitRender(const R: TRect);
begin
  FRenderRect:= QuadratRectI(R);
  FStrokeWidth:= (FRenderRect.Width + FRenderRect.Height)/22;
  FRenderRect.Inflate(-FStrokeWidth/2, -FStrokeWidth/2);
  if (FProgressStyle = psPaused) or (FProgressStyle = psError) then exit;
  if FBackgroundColor<> clNone then DoBackgroundRender;
end;

procedure TFluentProgressRing.DoBackgroundRender;
var
  C: TColorB;
begin
  if FBackgroundColor= clDefault then C:= clLightGray
  else C:= ColorToRGB(FBackgroundColor);
  DoCircleRender;
  Surface.Stroke(NewPen(C, FStrokeWidth));
end;

procedure TFluentProgressRing.DoCircleRender;
begin
  Surface.Ellipse(FRenderRect);
end;

procedure TFluentProgressRing.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor=AValue then Exit;
  FBackgroundColor:=AValue;
  Invalidate;
end;

procedure TFluentProgressRing.DoWarningErrorRender;
begin
  DoCircleRender;
end;

procedure TFluentProgressRing.DoDeterminateRender;
begin
  if FAnimationValue = FMax then DoCircleRender
  else if FAnimationValue > FMin then
    Surface.ArcTo(FRenderRect, 0, 2*pi*(FAnimationValue - FMin)/(FMax-FMin));
end;


procedure TFluentProgressRing.DoIndeterminateRender;
var
  A, L: Float;
begin
  if csDesigning in ComponentState then
  begin
    Surface.ArcTo(FRenderRect, pi, pi+3*pi/4);
    exit;
  end;

  A:= pi*FAnimationValue/2;
  L:= pi*sin(A)/2;
  if L>0.025 then Surface.ArcTo(FRenderRect, 6*A - L, 6*A + L);
end;

class function TFluentProgressRing.GetControlClassDefaultSize: TSize;
begin
  Result.cx:= 32;
  Result.cy:= 32;
end;

constructor TFluentProgressRing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor:= clNone;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

{ TFluentProgressBar }

procedure TFluentProgressBar.DoInitRender(const R: TRect);
begin
  FRenderRect:= R;
  FStrokeWidth:= Scale96ToScreen(3);
  FRenderRect.Inflate(-FStrokeWidth/2, 0);
end;

procedure TFluentProgressBar.DoLineRender(X1, X2: Float);
var
  Y: Float;
begin
  Y:= FRenderRect.MidLeft.Y;
  Surface.MoveTo(FRenderRect.X + MaxFloat(X1, 0), Y);
  Surface.LineTo(FRenderRect.X + MinFloat(X2, FRenderRect.Width), Y);
end;

procedure TFluentProgressBar.DoWarningErrorRender;
begin
  DoLineRender(0, FRenderRect.Width);
end;

procedure TFluentProgressBar.DoDeterminateRender;
var
  X, W: Float;
begin
  W:= FRenderRect.Width;
  X:= (FAnimationValue - FMin)*W/(FMax - FMin);
  if X < W then
  begin
    DoLineRender(X, W);
    Surface.Stroke(NewPen(DefGrayColor, FStrokeWidth/2));
  end;
  if X> 0 then
  begin
    DoLineRender(0, X);
  end;
end;

procedure TFluentProgressBar.DoIndeterminateRender;
const
  kLong =  0.6;
  kShort = 0.4;

var
  shortLength, longLength: float;
  X, V: float;

  procedure DrawShortLine(time: float);
  begin
    V:= 2*shortLength;
    X:= 2*V*time;
    if time<0.51 then DoLineRender(0, X*time)
    else DoLineRender(X - V, X - V + shortLength);
  end;

  procedure DrawLongLine(time: float);
  const
    time1024= 10/24;
  begin
    if time>0.9 then exit;
    V:= 4*longLength;
    X:= V*time;
    if time > time1024 then X-= V*sqr(time - time1024);
    DoLineRender(X- longLength, X);
  end;

begin
  if csDesigning in ComponentState then
  begin
    DoLineRender(0.2*FRenderRect.Width, 0.6*FRenderRect.Width);
    exit;
  end;

  shortLength:= FRenderRect.Width*kShort;
  longLength:= FRenderRect.Width*kLong;

  if FAnimationValue< 1.126 then DrawShortLine(FAnimationValue);
  if FAnimationValue> 1 then DrawLongLine(FAnimationValue - 1.0);
end;

class function TFluentProgressBar.GetControlClassDefaultSize: TSize;
begin
  Result.cy:= 24;
  Result.cx:= 180;
end;

procedure TFluentProgressBar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:= Scale96ToScreen(GetControlClassDefaultSize.cy);
end;

constructor TFluentProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

{ TBaseProgressControl }

procedure TBaseProgressControl.SetProgressStyle(AValue: TProgressStyle);
begin
  if FProgressStyle=AValue then Exit;
  FProgressStyle:=AValue;
  FAnimationController.StopAnimation;
  if (csDesigning in ComponentState) or (FProgressStyle <> psIndeterminate) then
    FAnimationValue:= FValue
  else
  begin
    FAnimationValue:= 0;
    FAnimationController.StartAnimation(nil, 2000, 0, 2, @OnAnimateValue, true);
    exit;
  end;
  Invalidate;
end;

procedure TBaseProgressControl.SetMax(NewValue: Double);
begin
  if FMax=NewValue then Exit;
  if NewValue< FMin then exit;
  FMax:=NewValue;
  if FValue> FMax then FValue:= FMax;
  if FProgressStyle = psDeterminate then
  begin
    FAnimationController.StopAnimation;
    FAnimationValue:= FValue;
    Invalidate;
  end;
end;

procedure TBaseProgressControl.SetMin(NewValue: Double);
begin
  if FMin=NewValue then Exit;
  if NewValue> FMax then exit;
  FMin:=NewValue;
  if FValue< FMin then FValue:= FMin;
  if FProgressStyle = psDeterminate then
  begin
    FAnimationController.StopAnimation;
    FAnimationValue:= FValue;
    Invalidate;
  end;
end;

procedure TBaseProgressControl.OnAnimateValue(AValue: Double);
begin
  FAnimationValue:= AValue;
  Repaint;
  //Invalidate;
end;

procedure TBaseProgressControl.SetNewValueAnimation;
var
  Duration: int64;
begin
  Duration:= abs(trunc(300*(FValue - FAnimationValue)/(FMax - FMin)));
  if Duration>50 then
    FAnimationController.StartAnimation(nil, Duration, FAnimationValue, FValue, @OnAnimateValue, false)
  else
  begin
    FAnimationController.StopAnimation;
    FAnimationValue:= FValue;
    Invalidate;
  end;
end;

procedure TBaseProgressControl.SetValue(NewValue: Double);
begin
  FAnimationValue:= FValue;
  if NewValue < FMin then
    NewValue:= FMin
  else if NewValue > FMax then
    NewValue:= FMax;
  if FValue = NewValue then
    Exit;
  FValue:= NewValue;
  if FProgressStyle = psDeterminate then
    SetNewValueAnimation;
end;

procedure TBaseProgressControl.DoCalcStrokeColor;
begin
  if not Enabled then
  begin
    FPenColor:= DefGrayColor;
    exit;
  end;
  if FProgressStyle= psPaused then
    FPenColor:= DefPausedColor
  else if FProgressStyle= psError then
    FPenColor:= DefErrorColor
  else
    FPenColor:= GetAccentColorB;
end;

function TBaseProgressControl.NewRenderPen: IPen;
begin
  DoCalcStrokeColor;
  Result:= NewPen(FPenColor, FStrokeWidth);
  Result.LineCap:= cpRound;
end;

procedure TBaseProgressControl.Render;
begin
  DoInitRender(ClientRect);
  if FProgressStyle = psIndeterminate then
    DoIndeterminateRender
  else if FProgressStyle = psDeterminate then
    DoDeterminateRender
  else
    DoWarningErrorRender;

  Surface.Stroke(NewRenderPen());

  inherited Render;
end;

constructor TBaseProgressControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationController:= TAnimationController.Create;
  FMax := 100.0;
  FMin := 0;
  FValue:=0;
  FAnimationValue:=0;
  FProgressStyle:= psDeterminate;
end;

destructor TBaseProgressControl.Destroy;
begin
  FAnimationController.Free;
  inherited Destroy;
end;

end.

