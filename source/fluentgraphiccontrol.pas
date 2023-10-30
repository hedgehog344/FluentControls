// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentGraphicControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;

type

  TRenderState = (rsNormal, rsHover, rsPressed, rsDisabled);
  TRenderOrientation = (roHorizontal, roVertical);

  { TFluentGraphicControl }

  TFluentGraphicControl = class(TGraphicControl, IFloatPropertyNotify)
  private
    FFontHeight: double;
    FOnRender: TDrawEvent;
    FSurface: ISurface;
    function GetRadius: Float;
  protected
    FRenderState: TRenderState;
    FShowText: boolean;
    procedure Paint; override; final;
    procedure Render; virtual;
    procedure EnabledChanged; override;
    procedure TextChanged; override;
    procedure PropChange({%H-}Prop: PFloat);  virtual;
    function GetFillColor(isAccent: boolean): TColorB;
    function GetFillColorAccent(aState: TRenderState): TColorB;
    function GetIFont: IFont;
    function CalculateTextSize(const S: string; F: IFont): TPointI;
    function GetTextDirection: TDirection;
    property Surface: ISurface read FSurface;
    property OnRender: TDrawEvent read FOnRender write FOnRender;
    property Radius: Float read GetRadius;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
//    property FontHeight: double read FFontHeight write SetFontHeight;
  end;

implementation

uses
  Utils;

const
  DefLightenAccent: array [TRenderState] of byte = (0, 10, 20, 22);

  DefDarkenParent:  array [TRenderState] of byte = (0, 3, 2, 2);
  DefLightenParent: array [TRenderState] of byte = (179, 125, 75, 75);

{ TFluentGraphicControl }

function TFluentGraphicControl.GetFillColor(isAccent: boolean): TColorB;
begin
  if isAccent then
    Result:= GetFillColorAccent(FRenderState)
  else
    Result:= Lighten(Darken(ParentCurrentColor, DefDarkenParent[FRenderState]/150), DefLightenParent[FRenderState]/255);
end;

function TFluentGraphicControl.GetFillColorAccent(aState: TRenderState): TColorB;
begin
  Result:= GetAccentColorB.Lighten(DefLightenAccent[aState]/100);
  if FRenderState = rsDisabled then
    Result:= Result.Desaturate(1).Lighten(0.6);
end;

function TFluentGraphicControl.GetRadius: Float;
begin
  Result:= Scale96ToScreen(4);
end;

procedure TFluentGraphicControl.Paint;
begin
  FSurface := NewSurface(Canvas);
  if FSurface <> nil then
  begin
    if Assigned(FOnRender) then
      FOnRender(Self, FSurface);
    Render;
    FSurface.Flush;
    FSurface := nil;
  end;
end;

procedure TFluentGraphicControl.Render;
begin
  //
end;

procedure TFluentGraphicControl.TextChanged;
begin
  inherited TextChanged;
  if not FShowText then exit;
  if AutoSize then
  begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
  Invalidate;
end;

procedure TFluentGraphicControl.EnabledChanged;
begin
  if Enabled then FRenderState:= rsNormal else FRenderState:= rsDisabled;
  inherited EnabledChanged;
end;

procedure TFluentGraphicControl.PropChange(Prop: PFloat);
begin
  //
end;

function TFluentGraphicControl.CalculateTextSize(const S: string; F: IFont): TPointI;
  function CeilFloat(val: Float): integer; inline;
  begin
    Result:=Trunc(val)+ord(Frac(val)>0);
  end;
var
  bmp: IBitmap;
  ts: TPointF;
begin
  bmp := NewBitmap;
  ts:= bmp.Surface.TextSize(F, S);
  Result.X:=CeilFloat(ts.X);
  Result.Y:=CeilFloat(ts.Y);
end;

function TFluentGraphicControl.GetTextDirection: TDirection;
begin
  if IsRightToLeft then
    Result:= drRight
  else
    Result:= drLeft;
end;

function TFluentGraphicControl.GetIFont: IFont;
var
  fs: integer;
begin
  Result := NewFont(Font);
  if FFontHeight>0 then
  begin
    Result.Size:= FFontHeight;
    exit;
  end;
  if (Font.Size = 0)  then
  begin
    fs:= Scale96ToScreen(19);
    Result.Size:= fs;
    //debugln('fs= '+FloatToStr(fs/100));
  end
  else if Font.Height<0 then
  begin
    fs:= trunc(-Font.Height*96/72);
    Result.Size:=  fs;
    //debugln('fh= '+IntToStr(Font.Height)+' fs= '+IntToStr(fs));
  end;
end;

constructor TFluentGraphicControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
    ControlStyle:= ControlStyle - [csDoubleClicks];
  FShowText:= false;
end;

procedure TFluentGraphicControl.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin
  if Font.Height=0 then exit;
  inherited;
end;

end.

