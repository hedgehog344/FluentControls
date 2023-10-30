// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentAnimation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type

  //https://learn.microsoft.com/dotnet/desktop/wpf/graphics-multimedia/easing-functions
  //https://easings.net/

  TEasingFunction = function(x: double): double;

  { TEasingFunctions }

  TEasingFunctions = class
  public

    class function EaseInBack(x: double): double; static;      // accelerating from zero velocity.
    class function EaseInOutBack(x: double): double; static;   // acceleration until halfway, then deceleration.
    class function EaseOutBack(x: double): double; static;     // decelerating to zero velocity.
    class function InOutBack: TEasingFunction; static;

    class function EaseInBounce(x: double): double; static;     //accelerating from zero velocity.
    class function EaseInOutBounce(x: double): double; static;  // acceleration until halfway, then deceleration.
    class function EaseOutBounce(x: double): double; static;    // decelerating to zero velocity.
    class function OutBounce: TEasingFunction; static;
    //EaseInCirc	Easing equation for a circular easing in - accelerating from zero velocity.
    //EaseInOutCirc	Easing equation for a circular easing in and out - acceleration until halfway, then deceleration.
    //EaseOutCirc	Easing equation for a circular easing out - decelerating to zero velocity.

    //EaseInCubic	Easing equation for a cubic easing in - accelerating from zero velocity.
    //EaseInOutCubic	Easing equation for a cubic easing in and out - acceleration until halfway, then deceleration.
    //EaseOutCubic	Easing equation for a cubic easing out - decelerating to zero velocity.

    class function EaseInElastic(x: double): double; static;    // accelerating from zero velocity.
    class function EaseInOutElastic(x: double): double; static;    // acceleration until halfway, then deceleration.
    class function EaseOutElastic(x: double): double; static;    // decelerating to zero velocity.

    //EaseInExpo	Easing equation for an exponential easing in - accelerating from zero velocity.
    //EaseInOutExpo	Easing equation for an exponential easing in and out - acceleration until halfway, then deceleration.
    //EaseOutExpo	Easing equation for an exponential easing out - decelerating to zero velocity.

    class function EaseInQuad(x: double): double; static;        // accelerating from zero velocity.
    class function EaseInOutQuad(x: double): double; static;     // acceleration until halfway, then deceleration.
    class function EaseOutQuad(x: double): double; static;       // decelerating to zero velocity.

    //EaseInQuart	Easing equation for a quartic easing in - accelerating from zero velocity.
    //EaseInOutQuart	Easing equation for a quartic easing in and out - acceleration until halfway, then deceleration.
    //EaseOutQuart	Easing equation for a quartic easing out - decelerating to zero velocity.

    //EaseInQuint	Easing equation for a quintic easing in - accelerating from zero velocity.
    //EaseInOutQuint	Easing equation for a quintic easing in and out - acceleration until halfway, then deceleration.
    //EaseOutQuint	Easing equation for a quintic easing out - decelerating to zero velocity.

    class function EaseInSine(x: double): double; static;        // accelerating from zero velocity.
    class function EaseInOutSine(x: double): double; static;     // acceleration until halfway, then deceleration.
    class function EaseOutSine(x: double): double; static;       // decelerating to zero velocity.

    class function Linear(x: double): double; static;            //Simple linear tweening, no easing and no acceleration.
  end;

  TOnAnimateEvent = procedure(AnimationValue: double) of object;
  TOnCompleteEvent = procedure of object;

  { TAnimationController }

  TAnimationController = class
  private
    FTimer: TTimer;
    FStartTickCount: QWord;
    FAnimationDuration: Int64;
    FAnimationTime: Int64;
    FAutoRepeat: boolean;
    FStartValue: double;
    FTargetValue: double;
    FOnAnimate: TOnAnimateEvent;
    FOnComplete: TOnCompleteEvent;
    FEasingFunction: TEasingFunction;
    function GetTimerEnabled: boolean;
    function Interpolate(t: double): double;
    procedure OnStartTimer(Sender: TObject);
    procedure OnAnimationTimer(Sender: TObject);
  protected

  public
    constructor Create;
    destructor Destroy; override;
    procedure StopAnimation;
    procedure StartAnimation(
      aEasing: TEasingFunction;
      aDuration: Int64;
      aStartValue, aTargetValue: double;
      aOnAnimate: TOnAnimateEvent;
      aRepeat: boolean;
      aOnComplete: TOnCompleteEvent=nil);
    property AnimationEnabled: boolean read GetTimerEnabled;
    property StartTickCount: QWord read FStartTickCount;
    property AnimationTime: int64 read FAnimationTime;
  end;

implementation

uses
  Math;

const
  DefTimerInterval = 15;

{ TEasingFunctions }

class function TEasingFunctions.EaseInBack(x: double): double;
const
  c1 = 1.70158;
begin
  Result:= ((c1 + 1)*x - c1)*sqr(x);
end;

class function TEasingFunctions.EaseInOutBack(x: double): double;
const
  c1 = 1.70158;
  c2 = c1 * 1.525;
begin
  if x < 0.5 then
    Result:= (sqr(2* x) * (2*(c2 + 1)*x - c2)) / 2
  else
    Result:= (sqr(2* x - 2) * ((c2 + 1) * (x * 2 - 2) + c2) + 2) / 2;
end;

class function TEasingFunctions.EaseOutBack(x: double): double;
const
  c1 = 1.70158;
  c3 = c1 + 1;
begin
  Result:= 1 + (c3 * (x - 1) + c1) * sqr(x - 1);
end;

class function TEasingFunctions.InOutBack: TEasingFunction;
begin
  Result:= @EaseInOutBack;
end;

class function TEasingFunctions.EaseOutBounce(x: double): double;
const
  n1 = 7.5625;
  d1 = 2.75;
begin
  if x < (1 / d1) then
    Result:= n1 * sqr(x)
  else if x < (2 / d1) then
    Result:=  n1 * sqr(x - 1.5 / d1) + 0.75
  else if x < (2.5 / d1) then
    Result:=  n1 * sqr(x - 2.25 / d1) + 0.9375
  else
    Result:=  n1 * sqr(x - 2.625 / d1) + 0.984375;
end;

class function TEasingFunctions.EaseInBounce(x: double): double;
begin
  Result:= 1 - EaseOutBounce(1 - x);
end;

class function TEasingFunctions.EaseInOutBounce(x: double): double;
begin
  if (x < 0.5) then
    Result:= EaseInBounce(x * 2) / 2
  else
    Result:= 1 - EaseInBounce((1 - x) * 2) / 2;
end;

class function TEasingFunctions.OutBounce: TEasingFunction;
begin
  Result:=@EaseOutBounce;
end;

class function TEasingFunctions.EaseInElastic(x: double): double;
const
  c4 = 2*pi/3;
begin
  if x=0 then exit(0);
  if x=1 then exit(1);
  Result:= - Power(2, 10 * x - 10) * Sin((x * 10 - 10.75) * c4);
end;

class function TEasingFunctions.EaseInOutElastic(x: double): double;
const
  c5 = 2*pi/4.5;
begin
  if x=0 then exit(0);
  if x=1 then exit(1);
  if x < 0.5 then
    Result:=  -(Power(2, 20 * x - 10) * Sin((20 * x - 11.125) * c5)) / 2
  else
  Result:=  (Power(2, -20 * x + 10) * Sin((20 * x - 11.125) * c5)) / 2 + 1;
end;

class function TEasingFunctions.EaseOutElastic(x: double): double;
const
  c4 = (2 * pi) / 3;
begin
  if x=0 then exit(0);
  if x=1 then exit(1);
  Result:= Power(2, -10 * x) * Sin((x * 10 - 0.75) * c4) + 1;
end;

class function TEasingFunctions.EaseInQuad(x: double): double;
begin
  Result:= x * x;
end;

class function TEasingFunctions.EaseInOutQuad(x: double): double;
begin
  if x < 0.5 then
    Result:= 2*sqr(x)
  else
    Result:= 1 - 2*sqr(1 - x);
end;

class function TEasingFunctions.EaseOutQuad(x: double): double;
begin
  Result:= 1 - sqr(1 - x);
end;

class function TEasingFunctions.EaseInSine(x: double): double;
begin
  Result:= 1 - Cos((x * pi) / 2);
end;

class function TEasingFunctions.EaseInOutSine(x: double): double;
begin
  Result:= (1 - Cos(pi * x)) / 2;
end;

class function TEasingFunctions.EaseOutSine(x: double): double;
begin
  Result:= Sin((x * pi) / 2);
end;

class function TEasingFunctions.Linear(x: double): double;
begin
  Result:= x;
end;

{ AnimationController }

procedure TAnimationController.OnStartTimer(Sender: TObject);
begin
  FStartTickCount:= GetTickCount64;
  FAnimationTime:=0;
end;

function TAnimationController.Interpolate(t: double): double;
begin
  if FEasingFunction<>nil then
    t:= FEasingFunction(t);
  Result:= FStartValue +(FTargetValue - FStartValue)*t;
end;

procedure TAnimationController.OnAnimationTimer(Sender: TObject);
var
  aTime: int64;
begin
  FAnimationTime:= GetTickCount64 - FStartTickCount;
  if FAutoRepeat then
    aTime:=  FAnimationTime mod FAnimationDuration
  else
    aTime:=  FAnimationTime;

  if aTime< FAnimationDuration then
    FOnAnimate(Interpolate(aTime/FAnimationDuration))
  else
  begin
    StopAnimation;
    FOnAnimate(FTargetValue);
    if Assigned(FOnComplete) then FOnComplete;
  end;
end;

function TAnimationController.GetTimerEnabled: boolean;
begin
  Result:= FTimer.Enabled;
end;

constructor TAnimationController.Create;
begin
  inherited Create;
  FTimer:= TTimer.Create(nil);
  FTimer.Enabled:= false;
  FTimer.Interval:= DefTimerInterval;
  FTimer.OnStartTimer:= @OnStartTimer;
  FTimer.OnTimer:= @OnAnimationTimer;
end;

destructor TAnimationController.Destroy;
begin
  FTimer.Enabled:= false;
  FTimer.Free;
  inherited Destroy;
end;

procedure TAnimationController.StopAnimation;
begin
  FTimer.Enabled:= false;
end;

procedure TAnimationController.StartAnimation(aEasing: TEasingFunction;
  aDuration: Int64; aStartValue, aTargetValue: double;
  aOnAnimate: TOnAnimateEvent; aRepeat: boolean; aOnComplete: TOnCompleteEvent);
begin
  StopAnimation;
  if not Assigned(aOnAnimate) then exit;
  if aDuration <= 0 then
  begin
    aOnAnimate(aTargetValue);
    exit;
  end;
  FEasingFunction:= aEasing;
  FOnAnimate:= aOnAnimate;
  FOnComplete:= aOnComplete;
  FAnimationDuration:= aDuration;
  FAutoRepeat:= aRepeat;
  FStartValue:= aStartValue;
  FTargetValue:= aTargetValue;
  FTimer.Enabled:= true;
end;


end.

