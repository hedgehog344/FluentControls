// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentIconButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Types,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentGraphicControl,
  FluentIconView;

type

  { TCustomFluentIconButton }

  TCustomFluentIconButton = class(TFluentIconView)
  private
    FShowCaption: boolean;
    FPenColor,
    FAccentColor : TColorB;

    FIconSize: integer;
    procedure RenderText(R: TRectI);
    procedure SetShowCaption(AValue: boolean);
    procedure DoChangeState(NewState: TRenderState);
    procedure DoSetIconColorsFromState;
    function GetBackColor: TColorB;
    procedure RenderBack(R: TRectI);
  protected
    procedure ColorChange(Sender: TObject); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                {%H-}WithThemeSpace: Boolean); override;
    procedure Render; override;
    procedure RenderGlyph(R: TRectI); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure MouseLeave; override;
    procedure MouseEnter; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default false;
  end;

  { TFluentIconButton }

  TFluentIconButton = class(TCustomFluentIconButton)

  published
    property Action;
    property Caption;
    property ShowCaption;
    property BiDiMode;
    property Font;
    property ParentFont;
  end;

implementation

{ TCustomFluentIconButton }

procedure TCustomFluentIconButton.SetShowCaption(AValue: boolean);
begin
  if FShowCaption=AValue then Exit;
  FShowCaption:=AValue;
  InvalidatePreferredSize;
end;

procedure TCustomFluentIconButton.DoSetIconColorsFromState;
begin
  FPenColor:= PathIcon.ForegroundColor;
  FAccentColor:= PathIcon.AccentColor;

  case FRenderState of
    rsNormal, rsHover:
      begin
        FPenColor:= FPenColor.Lighten(26/255);
        FAccentColor:= FAccentColor.Lighten(26/255);
      end;
    rsPressed, rsDisabled:
      begin
        FPenColor:= FPenColor.Lighten(90/255);
        FAccentColor:= FAccentColor.Lighten(90/255);
      end;
  end;
  PathIconDrawer.ForegroundColor:= FPenColor;
  PathIconDrawer.AccentColor:= FAccentColor;
  PathIconDrawer.BackColor:= GetBackColor;
end;

procedure TCustomFluentIconButton.DoChangeState(NewState: TRenderState);
begin
  if FRenderState= NewState then exit;
  FRenderState:= NewState;
  DoSetIconColorsFromState;
  Invalidate;
end;

procedure TCustomFluentIconButton.RenderBack(R: TRectI);
var
  C: TColorB;
  b: byte;
begin
  if (FRenderState = rsNormal) or (FRenderState = rsDisabled) then exit;
  if FRenderState = rsHover then b:=9 else b:=6; // pressed
  C:= Darken(GetBackColor, b/255);
  Surface.FillRoundRect(NewBrush(C), R, Radius);
end;

procedure TCustomFluentIconButton.ColorChange(Sender: TObject);
begin
  DoSetIconColorsFromState;
  inherited ColorChange(Sender);
end;

procedure TCustomFluentIconButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  if FShowCaption then PreferredWidth+= TextSize(Caption).X+4;
end;

procedure TCustomFluentIconButton.RenderText(R: TRectI);
var
  S: string;
begin
  S:= Caption;
  if S='' then exit;
  //if IsRightToLeft then
  //begin
  //  R.Width:= R.Width - FBoxSize*1.4;
  //  Surface.TextOut(GetIFont, S, R, drRight)
  //end
  //else
  //begin
    R.Left:= R.X+ FIconSize+24;
    Surface.TextOut(GetIFont, S, R, drLeft);
//  end;
end;

procedure TCustomFluentIconButton.RenderGlyph(R: TRectI);
var
  dw, dh: integer;
  VR: TRectI;
begin
  VR:=R;
  dw:= (2*R.Width) div 8;
  dh:= (2*R.Height) div 8;
  if FShowCaption then
  begin
    if dw> dh then
      FIconSize:=dh*2
    else
      FIconSize:=dw*2;
    RenderText(R);
    VR.X:=12;
    VR.Width:=FIconSize;
  end
  else
    VR.Inflate(-dw, -dh);
  inherited RenderGlyph(VR);
end;

function TCustomFluentIconButton.GetBackColor: TColorB;
begin
  if ParentColor then
    Result:= ParentCurrentColor
  else
    Result:= Color;
end;

procedure TCustomFluentIconButton.Render;
var
  R: TRectI;
begin
  R:= ClientRect;


  RenderBack(R);

  inherited Render;
end;

class function TCustomFluentIconButton.GetControlClassDefaultSize: TSize;
begin
  Result.cx:= 40;
  Result.cy:= 36;
end;

procedure TCustomFluentIconButton.MouseLeave;
begin
  DoChangeState(rsNormal);
  inherited MouseLeave;
end;

procedure TCustomFluentIconButton.MouseEnter;
begin
  DoChangeState(rsHover);
  inherited MouseEnter;
end;

procedure TCustomFluentIconButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FRenderState = rsHover) and (Button = mbLeft) then
    DoChangeState(rsPressed);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomFluentIconButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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

constructor TCustomFluentIconButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowCaption:= false;
  FRenderState:= rsNormal;
  DoSetIconColorsFromState;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.

