// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentSlider;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, Controls,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentGraphicControl, FluentAnimation;

type

  TTickPlacement = (tpBottomRight, tpTopLeft);
  TShowTipEvent = procedure (Sender: TObject; Value: double; out S: String) of object;

  { TCustomFluentSlider }

  TCustomFluentSlider = class(TFluentGraphicControl)
  private
    FNeedRecalc: boolean;
    FMax: Double;
    FMin: Double;
    FReversed: boolean;
    FShowTip: boolean;
    FValue: Double;

    FLineRenderRect: TRectF;
    FThumbMoveRect: TRectF;
    FThumbRect: TRectF;

    FStrokeWidth: Float;
    FThumbSize: Float;
    FThumbBorderWidth: Float;
    FCircleSize: array [TRenderState] of Float;
    FThumbState: TRenderState;
    FThumbPosition: TPointF;
    FDeltaThumbPos: TPointF;
    FOnShowTip: TShowTipEvent;
    FOnValueChanged: TNotifyEvent;
    FOrientation: TRenderOrientation;
    FShowTicks: boolean;
    FTickFrequency: double;
    FTickPlacement: TTickPlacement;

    FTip: ISplash;
    FAnimationSize: Float;
    FAnimationController: TAnimationController;

    procedure CalcSizes;
    procedure CalcThumbPosition;
    procedure DoDrawLine(pt1, pt2: TPointF);

    function GetThumbPosition: TPointF;
    function GetValueFromMouse(const pt: TPointF): double;
    function GetXPositionFromValue(Value: Float): Float;
    function GetYPositionFromValue(Value: Float): Float;
    procedure OnAnimateValue(AValue: Double);

    procedure SetMax(NewValue: double);
    procedure SetMin(NewValue: double);
    procedure SetReversed(AValue: boolean);
    procedure SetValue(NewValue: double);

    procedure SetOrientation(AValue: TRenderOrientation);
    procedure SetShowTicks(AValue: boolean);
    procedure SetTickFrequency(AValue: double);
    procedure SetTickPlacement(AValue: TTickPlacement);


    procedure DoShowTip;
    procedure DoHideTip;

    procedure DrawBackLine;
    procedure DrawThumb;
    procedure DrawTicks;

    procedure CalcSliderRect;
    procedure StartAnimation(oldState: TRenderState;
      onComplete: TOnCompleteEvent= nil);
    procedure StopAnimation;
    function IsTipVisible: boolean;
  protected
    procedure Render; override;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         {%H-}WithThemeSpace: Boolean); override;

    property Max: Double read FMax write SetMax;
    property Min: Double read FMin write SetMin;
    property Value: Double read FValue write SetValue;
    property Orientation: TRenderOrientation read FOrientation write SetOrientation default roHorizontal;
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
    property OnShowTip: TShowTipEvent read FOnShowTip write FOnShowTip;
    property ShowTicks: boolean read FShowTicks write SetShowTicks default false;
    property ShowTip: boolean read FShowTip write FShowTip default false;
    property TickFrequency: double read FTickFrequency write SetTickFrequency;
    property TickPlacement: TTickPlacement read FTickPlacement write SetTickPlacement default tpBottomRight;
    property Reversed: boolean read FReversed write SetReversed default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TFluentSlider = class(TCustomFluentSlider)
    published
    property Align;
    property Anchors;
    property BorderSpacing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property Max;
    property Min;
    property PopupMenu;
    property Value;
    property Reversed;
    property ShowTicks;
    property ShowTip;
    property TickPlacement;
    property TickFrequency;
    property Orientation;
    property Visible;
    property OnShowTip;
    property OnClick;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
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
    property OnValueChanged;
  end;

implementation

uses
  Utils;

{ TCustomFluentSlider }

procedure TCustomFluentSlider.SetMax(NewValue: double);
begin
  if FMax=NewValue then Exit;
  FMax:= MaxDouble(FMin, NewValue);
  if FValue> FMax then FValue:= FMax;
  FNeedRecalc:= true;
  Invalidate;
end;

procedure TCustomFluentSlider.SetMin(NewValue: double);
begin
  if FMin=NewValue then Exit;
  FMin:= MinDouble(FMax, NewValue);
  if FValue< FMin then FValue:= FMin;
  FNeedRecalc:= true;
  Invalidate;
end;

procedure TCustomFluentSlider.SetReversed(AValue: boolean);
begin
  if FReversed=AValue then Exit;
  FReversed:=AValue;
  FNeedRecalc:= true;
  Invalidate;
end;

procedure TCustomFluentSlider.SetOrientation(AValue: TRenderOrientation);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  if csLoading in ComponentState then exit;
  FNeedRecalc:= true;
  if Width<>Height then
    SetBounds(Left, Top, Height, Width)
  else
    Invalidate;
end;

procedure TCustomFluentSlider.SetShowTicks(AValue: boolean);
begin
  if FShowTicks=AValue then Exit;
  FShowTicks:=AValue;
  Invalidate;
end;

procedure TCustomFluentSlider.SetTickFrequency(AValue: double);
begin
  if FTickFrequency=AValue then Exit;
  FTickFrequency:=AValue;
  Invalidate;
end;

procedure TCustomFluentSlider.SetTickPlacement(AValue: TTickPlacement);
begin
  if FTickPlacement=AValue then Exit;
  FTickPlacement:=AValue;
  Invalidate;
end;

procedure TCustomFluentSlider.SetValue(NewValue: double);
begin
  if FValue=NewValue then Exit;
  FValue:= EnsureRangeDouble(FMin, FMax, NewValue);
  if Assigned(FOnValueChanged) then FOnValueChanged(self);
  FNeedRecalc:= true;
  Invalidate;
end;

procedure TCustomFluentSlider.DoHideTip;
begin
  if FTip<> nil then
    FTip.Visible:= false;
end;

function TCustomFluentSlider.IsTipVisible: boolean;
begin
  if FTip<> nil then Result:= FTip.Visible
  else Result:= false;
end;

procedure TCustomFluentSlider.DoShowTip;
var
  aSurface: ISurface;
  TipFont: IFont;
  R: TRectF;
  tipSize: TPointI;
  ts: TPointF;
  S: string = '';
  pt: TPoint;
begin
  if (not FShowTip) then exit;
  if FOnShowTip = nil then
    S:= IntToStr(round(FValue))
  else
  begin
    FOnShowTip(self, FValue, S);
    if S='' then Exit;
  end;

  if FTip=nil then FTip:= NewSplash();
  aSurface := FTip.Bitmap.Surface;
  TipFont:= GetIFont;
  ts:= aSurface.TextSize(TipFont, S);
  tipSize := TPointI.Create(round(ts.X + ts.Y), round(1.5*ts.y));
  FTip.Bitmap.SetSize(tipSize.X, tipSize.Y);
  aSurface.Clear(clTransparent);
  R:= FTip.Bitmap.ClientRect;
  R.Inflate(-FThumbBorderWidth/2, -FThumbBorderWidth/2);

  aSurface.StrokeRoundRect(
    NewPen(TColorB(DefGrayColor).Fade(0.2), FThumbBorderWidth),
    R, FStrokeWidth);
  R.Inflate(-FThumbBorderWidth/2, -FThumbBorderWidth/2);
  aSurface.RoundRectangle(R, FStrokeWidth - FThumbBorderWidth);
  aSurface.Fill(NewBrush(DefWhiteColor));

  aSurface.TextOut(TipFont, S, R, drCenter);
  if FOrientation = roHorizontal then
    pt:= ClientToScreen(Point(
        round(FThumbRect.X + (FThumbRect.Width - tipSize.X)/2),
        round(FThumbRect.Top - tipSize.Y - 2*FStrokeWidth)))
  else
    pt:= ClientToScreen(Point(
        round(FThumbRect.X - tipSize.X - 2*FStrokeWidth),
        round(FThumbRect.Y + (FThumbRect.Height - tipSize.Y)/2)));

  FTip.Move(pt.X, pt.Y);
  FTip.Opacity := $F0;
  FTip.Visible:= true;
  FTip.Update;
end;

procedure TCustomFluentSlider.DoDrawLine(pt1, pt2: TPointF);
var
  C: TColorB;
begin
  if not Enabled then
    C:= GetDisabledAccentColorB
  else
    C:= GetAccentColorB.Darken(25/255);

  if FMax<= FMin then
  begin
    StrokeLine(Surface, pt1, pt2, NewRoundPen(C, FStrokeWidth));
    exit;
  end;
  if FReversed then
    SwapPointF(pt1, pt2);
  if FValue> FMin then
    StrokeLine(Surface, pt1, FThumbRect.MidPoint, NewRoundPen(C, FStrokeWidth));
  if FValue< FMax then
    StrokeLine(Surface, pt2, FThumbRect.MidPoint, NewRoundPen(DefGrayColor, FStrokeWidth));
end;

procedure TCustomFluentSlider.DrawBackLine;
begin
  if FOrientation = roHorizontal then
    DoDrawLine(FLineRenderRect.MidLeft, FLineRenderRect.MidRight)
  else
    DoDrawLine(FLineRenderRect.MidBottom, FLineRenderRect.MidTop);
end;

procedure TCustomFluentSlider.DrawThumb;
var
  R: TRectF;
  d: Float;
  b: byte;
begin

  R:= FThumbRect;
  R.Inflate(-FThumbBorderWidth/2, -FThumbBorderWidth/2);
  Surface.Ellipse(R);
  Surface.Stroke(NewPen(DefBlackColor.Fade(0.1), FThumbBorderWidth));

  R:= FThumbRect;
  R.Inflate(-FThumbBorderWidth, -FThumbBorderWidth);
  Surface.Ellipse(R);
  Surface.Fill(NewBrush(DefWhiteColor));



  if FAnimationController.AnimationEnabled then
    d:= FAnimationSize
  else
    d:= FCircleSize[FThumbState];

  R:= TrectF.Create(d, d);
  R.Center(FThumbRect.MidPoint);
  Surface.Ellipse(R);

  if Enabled then b:=15 else d:=125;

  Surface.Fill(NewBrush(GetAccentColorB.Lighten(b/255)));
end;

procedure TCustomFluentSlider.DrawTicks;
var
  Pen: IPen;
  val, X, Y, tickSize: Float;
begin
  if not FShowTicks then exit;
  if FTickFrequency<=0 then exit;
  if FMax<= FMin then exit;

  val:= trunc(FMin/FTickFrequency)*FTickFrequency;

  Pen:= NewPen(DefGrayColor);

  tickSize:= round(FStrokeWidth);

  if FTickPlacement = tpTopLeft then
  begin
    X:=FThumbRect.Left+ FThumbBorderWidth;
    Y:=FThumbRect.Top + FThumbBorderWidth;
  end
  else
  begin
    X:= FThumbRect.Right - tickSize-FThumbBorderWidth;
    Y:= FThumbRect.Bottom - tickSize - FThumbBorderWidth;
  end;

  while val<= FMax do
  begin
    if FOrientation = roHorizontal then
    begin
      X:= trunc(GetXPositionFromValue(val)) + 0.5;
      Surface.MoveTo(X, Y + tickSize);
    end
    else
    begin
      Y:= trunc(GetYPositionFromValue(val)) + 0.5;
      Surface.MoveTo(X+ tickSize, Y);
    end;
    Surface.LineTo(X, Y);
    Surface.Stroke(Pen);
    val+= FTickFrequency;
  end;
end;

function TCustomFluentSlider.GetXPositionFromValue(Value: Float): Float;
var
  d: float;
begin
  if FMax<= FMin then
    d:= FThumbMoveRect.Width/2
  else
    d:= FThumbMoveRect.Width*(Value - FMin)/(FMax - FMin);
  if FReversed then
    Result:=  FThumbMoveRect.Right - d
  else
    Result:= FThumbMoveRect.X + d;
end;

function TCustomFluentSlider.GetYPositionFromValue(Value: Float): Float;
var
  d: float;
begin
  if FMax <= FMin then
    d:= FThumbMoveRect.Height/2
  else
    d:= FThumbMoveRect.Height*(Value - FMin)/(FMax - FMin);
  if FReversed then
    Result:= FThumbMoveRect.Y + d
  else
    Result:= FThumbMoveRect.Bottom - d;
end;

function TCustomFluentSlider.GetThumbPosition: TPointF;
begin
  Result:= FThumbMoveRect.MidPoint;
  if FMax <= FMin then exit;
  if FOrientation = roHorizontal then
    Result.X:=GetXPositionFromValue(FValue)
  else
    Result.Y:= GetYPositionFromValue(FValue);
end;

function TCustomFluentSlider.GetValueFromMouse(const pt: TPointF): double;
var
  pos, delta: float;
begin
  if FMax <= FMin then
  begin
    Result:= FMin;
    exit;
  end;
  if FOrientation = roHorizontal then
  begin
    pos:= EnsureRangeFloat(FThumbMoveRect.Left, FThumbMoveRect.Right, pt.X);

    if FReversed then
      delta:= FThumbMoveRect.Right - pos
    else
      delta:= pos - FThumbMoveRect.X;

    Result:= FMin + delta*(FMax-FMin)/FThumbMoveRect.Width;
  end
  else
  begin
    pos:= EnsureRangeFloat(FThumbMoveRect.Top, FThumbMoveRect.Bottom, pt.Y);
    if FReversed then
      delta:= FThumbMoveRect.Top - pos
    else
      delta:= pos - FThumbMoveRect.Bottom;

    Result:= FMin - delta*(FMax-FMin)/FThumbMoveRect.Height;
  end;
end;

procedure TCustomFluentSlider.CalcSizes;
begin
  FStrokeWidth:= Scale96ToScreen(4);
  FThumbSize:= Scale96ToScreen(22);
  FThumbBorderWidth:= Scale96ToScreen(2)/2;
  FCircleSize[rsNormal]:=  FThumbSize/2;
  FCircleSize[rsHover]:=   Scale96ToScreen(28)/2;
  FCircleSize[rsPressed]:= Scale96ToScreen(18)/2;
end;

procedure TCustomFluentSlider.Render;
begin
  if FNeedRecalc then CalcThumbPosition;
  DrawBackLine;
  DrawTicks;
  DrawThumb;
  inherited;
end;

procedure TCustomFluentSlider.CalcSliderRect;
begin
  CalcSizes;
  FLineRenderRect:= ClientRect;
  FThumbMoveRect:= FLineRenderRect;
  FLineRenderRect.Inflate(-FStrokeWidth/2, -FStrokeWidth/2);
  FThumbMoveRect.Inflate(-FThumbSize/2, -FThumbSize/2);
  FThumbRect:= TrectF.Create(FThumbSize, FThumbSize);
  CalcThumbPosition;
end;

procedure TCustomFluentSlider.Resize;
begin
  CalcSliderRect;
  inherited Resize;
end;

procedure TCustomFluentSlider.OnAnimateValue(AValue: Double);
begin
  FAnimationSize:= AValue;
  Invalidate;
end;

procedure TCustomFluentSlider.StartAnimation(oldState: TRenderState; onComplete: TOnCompleteEvent = nil);
var
  v1, v2: Float;
begin
  if FAnimationController.AnimationEnabled then v1:= FAnimationSize else v1:= FCircleSize[oldState];
  v2:= FCircleSize[FThumbState];
  if abs(v1 - v2)<1 then
  begin
    StopAnimation;
    Invalidate;
  end
  else
    FAnimationController.StartAnimation(nil, 150, v1, v2,  @OnAnimateValue, false, onComplete);
end;

procedure TCustomFluentSlider.StopAnimation;
begin
  FAnimationController.StopAnimation;
end;

procedure TCustomFluentSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  inCircle: boolean;
begin
  inherited;
  pt:= TPointF.Create(X, Y);
  inCircle:= FThumbRect.Contains(pt);
  if (FThumbState = rsNormal) and inCircle then
  begin
    FThumbState:= rsHover;
    StartAnimation(rsNormal, @DoShowTip);
  end
  else if (FThumbState = rsHover) and (not inCircle) then
  begin
    FThumbState:= rsNormal;
    StartAnimation(rsHover);
    DoHideTip;
  end
  else
  if (FThumbState = rsPressed) then
  begin
    StopAnimation;
    SetValue(GetValueFromMouse(FDeltaThumbPos + pt));
    CalcThumbPosition;
    DoShowTip;
  end;
end;

procedure TCustomFluentSlider.CalcThumbPosition;
begin
  FThumbPosition:= GetThumbPosition;
  FThumbRect.Center(FThumbPosition);
  FNeedRecalc:= false;
end;

procedure TCustomFluentSlider.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  inherited;
  if Button <> mbLeft then exit;
  pt:= TPointF.Create(X, Y);
  if FThumbState = rsHover then
  begin
    FDeltaThumbPos:= FThumbRect.MidPoint - pt;
    FThumbState:= rsPressed;
    StartAnimation(rsHover, @DoShowTip);
  end
  else
  begin
    StopAnimation;
    FThumbState:= rsPressed;
    FDeltaThumbPos:= TPointF.Create(0, 0);
    SetValue(GetValueFromMouse(pt));
    CalcThumbPosition;
    DoShowTip;
  end;

end;

procedure TCustomFluentSlider.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  pt: TPointF;
begin
  inherited;
  if Button <> mbLeft then exit;
  DoHideTip;
  if FThumbState = rsPressed then
  begin
    pt:= TPointF.Create(X, Y);
    if FThumbRect.Contains(pt) then FThumbState:= rsHover
    else FThumbState:= rsNormal;
    StartAnimation(rsPressed);
  end;

end;

procedure TCustomFluentSlider.MouseLeave;
begin
  DoHideTip;
  if FThumbState = rsHover then
  begin
    FThumbState:= rsNormal;
    StartAnimation(rsHover);
  end;
  inherited MouseLeave;
end;

class function TCustomFluentSlider.GetControlClassDefaultSize: TSize;
begin
  Result.cy:= 24;
  Result.cx:= 180;
end;

procedure TCustomFluentSlider.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:= Scale96ToScreen(GetControlClassDefaultSize.cy);
  if FOrientation = roVertical then
  begin
    PreferredWidth:= PreferredHeight;
    PreferredHeight:= 0;
  end;
end;

constructor TCustomFluentSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReversed:= false;
  FShowTip:= false;
  FOrientation:= roHorizontal;
  FAnimationController:= TAnimationController.Create;

  FTickFrequency:=10;
  FMax := 100.0;
  FMin := 0;
  FValue:=0;
  FAnimationSize:=0;
  FThumbState:= rsNormal;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomFluentSlider.Destroy;
begin
  FAnimationController.Free;
  inherited Destroy;
end;

end.

