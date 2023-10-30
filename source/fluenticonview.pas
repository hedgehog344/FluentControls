// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentIconView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentGraphicControl,
  FluentAnimation,
  PathIconDrawer,
  FluentPathIcon;

type

{ TFluentIconView }

  TFluentIconView = class(TFluentGraphicControl)
  private
    FAnimationController: TAnimationController;
    FOnRenderGlyph: TDrawRectEvent;
    FPathIconDrawer: TPathIconDrawer;
    FPathIcon: TPathIcon;
    procedure SetPathIcon(AValue: TPathIcon);
  protected
    procedure PropChange(Prop: PFloat);  override;
    procedure ColorChange(Sender: TObject); virtual;
    procedure PathChange(Sender: TObject); virtual;
    procedure RenderGlyph(R: TRectI); virtual;
    procedure Render; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                {%H-}WithThemeSpace: Boolean); override;
    property PathIconDrawer: TPathIconDrawer read FPathIconDrawer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Animate(aDuration: Int64; aStartValue, aTargetValue: double;
      aOnAnimate: TOnAnimateEvent; aRepeat: boolean); overload;
    procedure Animate(aEasing: TEasingFunction; aDuration: Int64; aStartValue, aTargetValue: double;
      aOnAnimate: TOnAnimateEvent; aRepeat: boolean); overload;
    procedure StopAnimation;
  published
    property PathIcon: TPathIcon read FPathIcon write SetPathIcon;
    property OnRender;
    property OnRenderGlyph: TDrawRectEvent read FOnRenderGlyph write FOnRenderGlyph;
    property Align;
    property AutoSize;
    property Anchors;
    property BorderSpacing;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Constraints;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
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

{ TFluentIconView }

procedure TFluentIconView.RenderGlyph(R: TRectI);
begin
  if Assigned(FOnRenderGlyph) then FOnRenderGlyph(self, Surface, R);
  with FPathIconDrawer do
  begin
    if Enabled then
    begin
      AccentColor:= FPathIcon.AccentColor;
      ForegroundColor :=  FPathIcon.ForegroundColor;
    end
    else
    begin
      AccentColor:= Lighten(FPathIcon.AccentColor, 0.4);
      ForegroundColor :=  Lighten(FPathIcon.ForegroundColor, 0.4);
    end;
    BackColor:= self.ParentCurrentColor;
    Draw(Surface, R, FPathIcon.Angle, FPathIcon.Scale);
  end;
end;

procedure TFluentIconView.Render;
begin
  RenderGlyph(ClientRect);
  inherited Render;
end;

class function TFluentIconView.GetControlClassDefaultSize: TSize;
begin
  Result.cx:= 36;
  Result.cy:= 36;
end;

procedure TFluentIconView.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=  Scale96ToForm(round(GetControlClassDefaultSize.cx));
  PreferredHeight:= Scale96ToForm(round(GetControlClassDefaultSize.cy));
end;

procedure TFluentIconView.ColorChange(Sender: TObject);
begin
  with FPathIconDrawer do
  begin
    AccentColor:= FPathIcon.AccentColor;
    ForegroundColor :=  FPathIcon.ForegroundColor;

  end;
  Invalidate;
end;

procedure TFluentIconView.PathChange(Sender: TObject);
begin
  FPathIconDrawer.ParsePathData(FPathIcon.PathString);
end;

procedure TFluentIconView.SetPathIcon(AValue: TPathIcon);
begin
  FPathIcon.Assign(AValue);
  Invalidate;
end;

procedure TFluentIconView.PropChange(Prop: PFloat);
begin
  //debugln('PropValue: '+ FloatToStr(Prop^));
end;

constructor TFluentIconView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPathIcon:= TPathIcon.Create;
  FPathIconDrawer:= TPathIconDrawer.Create;
  FPathIcon.OnColorChange:= @ColorChange;
  FPathIcon.OnPathChange:= @PathChange;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TFluentIconView.Destroy;
begin
  if Assigned(FAnimationController) then
    FAnimationController.Free;
  FPathIcon.Free;
  inherited Destroy;
end;

procedure TFluentIconView.Animate(aDuration: Int64;
  aStartValue, aTargetValue: double;
  aOnAnimate: TOnAnimateEvent;
  aRepeat: boolean);
begin
  Animate(nil, aDuration, aStartValue, aTargetValue, aOnAnimate, aRepeat);
end;

procedure TFluentIconView.Animate(aEasing: TEasingFunction; aDuration: Int64;
  aStartValue, aTargetValue: double; aOnAnimate: TOnAnimateEvent;
  aRepeat: boolean);
begin
  if FAnimationController=nil then
    FAnimationController:= TAnimationController.Create;
  FAnimationController.StopAnimation;
  FAnimationController.StartAnimation(aEasing, aDuration, aStartValue, aTargetValue, aOnAnimate, aRepeat);
end;

procedure TFluentIconView.StopAnimation;
begin
  if FAnimationController=nil then exit;
  FAnimationController.StopAnimation;
end;

end.

