// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentTextBlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Types, LCLType,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentGraphicControl;

type

  { TCustomFluentTextBlock }

  TCustomFluentTextBlock = class(TFluentGraphicControl)
  private

  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                {%H-}WithThemeSpace: Boolean); override;
    procedure Render; override;
    procedure RenderText(R: TrectF; F: IFont; D: TDirection);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TFluentTextBlock }

  TFluentTextBlock = class(TCustomFluentTextBlock)
  private

  published
    property Align;
    property Anchors;
    property AutoSize default true;
    property BiDiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Enabled;
    property Font;
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
    property Visible;
  end;

implementation

{ TCustomFluentTextBlock }

class function TCustomFluentTextBlock.GetControlClassDefaultSize: TSize;
begin
  Result.cx:= 170;
  Result.cy:= 19;
end;

procedure TCustomFluentTextBlock.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  with CalculateTextSize(Caption, GetIFont) do
  begin
    PreferredWidth:= X;
    PreferredHeight:= Y;
  end;
end;

procedure TCustomFluentTextBlock.Render;
begin
  inherited Render;
  RenderText(ClientRect, GetIFont, GetTextDirection);
end;

procedure TCustomFluentTextBlock.RenderText(R: TrectF; F: IFont; D: TDirection);
begin
  if Caption='' then exit;
  Surface.TextOut(F, Caption, R, D);
end;

constructor TCustomFluentTextBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  AutoSize := true;
  FShowText:= true;
end;

end.

