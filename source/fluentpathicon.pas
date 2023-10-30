// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentPathIcon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,

  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentIconList;

const
  DefIconPenColor = clBlack;
  DefIconBackColor = clWhite;
  DefIconAccentColor = clBlack;

type

  TCodePoint = 0..$FFFF;

  { TPathIcon }

  TPathIcon = class(TPersistent)
  private
    FAngle: integer;
    FForegroundColor, FAccentColor: TColorB;
    FOnPathChange: TNotifyEvent;
    FScale: double;
    FUpdateCount: integer;
    FChanged: boolean;
    FPathData: string;
    FCodePoint: TCodePoint;
    FOnChange: TNotifyEvent;
    procedure Changed;
    procedure CopyFrom(const Src: TPathIcon);
    procedure DoSetPathData(const AValue: string);
    function GetAccentColor: TColor;
    function GetPathString: string;
    function GetForegroundColor: TColor;
    procedure SetAngle(AValue: integer);
    procedure SetForegroundColor(AValue: TColor);
    procedure SetAccentColor(AValue: TColor);
    procedure SetCodePoint(AValue: TCodePoint);
    procedure SetPathString(AValue: string);
    procedure SetScale(AValue: double);
  public
    constructor Create; virtual;
    procedure EndUpdate;
    procedure BeginUpdate;
    procedure Assign(Source: TPersistent); override;
    property OnColorChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPathChange: TNotifyEvent read FOnPathChange write FOnPathChange;
  published
    property PathString: string read GetPathString write SetPathString;
    property CodePoint: TCodePoint read FCodePoint write SetCodePoint default 0;
    property ForegroundColor: TColor read GetForegroundColor write SetForegroundColor default DefIconPenColor;
    property AccentColor: TColor read GetAccentColor write SetAccentColor default DefIconAccentColor;
    property Scale: double read FScale write SetScale;
    property Angle: integer read FAngle write SetAngle default 0;
  end;


implementation

{ TPathIcon }

procedure TPathIcon.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TPathIcon.EndUpdate;
begin
  if FUpdateCount=0 then exit;
  dec(FUpdateCount);
  if (FUpdateCount=0) and FChanged then Changed;
end;

procedure TPathIcon.Changed;
begin
  if FUpdateCount > 0 then
  begin
    FChanged := True;
    exit;
  end;
  FChanged := false;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPathIcon.DoSetPathData(const AValue: string);
begin
  if FPathData = AValue then exit;
  FPathData:= AValue;
  if Assigned(FOnPathChange) then FOnPathChange(Self);
  Changed;
end;

// path
procedure TPathIcon.SetPathString(AValue: string);
begin
  FCodePoint:= 0;
  DoSetPathData(AValue);
end;

procedure TPathIcon.SetScale(AValue: double);
begin
  if FScale=AValue then Exit;
  FScale:=AValue;
  Changed;
end;

function TPathIcon.GetPathString: string;
begin
  Result:= FPathData;
end;

procedure TPathIcon.SetCodePoint(AValue: TCodePoint);
begin
  if FCodePoint=AValue then Exit;
  FCodePoint:=AValue;
  DoSetPathData(GetIconPathByIndex(AValue));
end;

// foreground
procedure TPathIcon.SetForegroundColor(AValue: TColor);
begin
  if FForegroundColor.Color = AValue then exit;
  FForegroundColor:= AValue;
  Changed;
end;

function TPathIcon.GetForegroundColor: TColor;
begin
  Result:= FForegroundColor.Color;
end;

procedure TPathIcon.SetAngle(AValue: integer);
begin
  if FAngle= AValue then Exit;
  FAngle:=AValue mod 360;
  Changed;
end;

// accent
function TPathIcon.GetAccentColor: TColor;
begin
  Result:= FAccentColor.Color;
end;

procedure TPathIcon.SetAccentColor(AValue: TColor);
begin
  if FAccentColor.Color = AValue then exit;
  FAccentColor:= AValue;
  Changed;
end;

constructor TPathIcon.Create;
begin
  inherited Create;
  FCodePoint:= 0;
  FUpdateCount:= 0;
  FScale:= 1;
  FAngle:= 0;
  FChanged:= false;
end;

procedure TPathIcon.CopyFrom(const Src: TPathIcon);
begin
  FPathData:=Src.PathString;
  if Assigned(FOnPathChange) then FOnPathChange(Self);
  FCodePoint:=Src.CodePoint;
  FForegroundColor:=Src.ForegroundColor;
  FAccentColor:= Src.AccentColor;
  Changed;
end;

procedure TPathIcon.Assign(Source: TPersistent);
begin
  if Source is TPathIcon then
    CopyFrom(TPathIcon(Source))
  else
    inherited Assign(Source);
end;

end.

