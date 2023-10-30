// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit PathIconDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, RegExpr,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentPathIcon;


const
  DEF_ICON_SIZE = 64;
  DEF_PEN_WIDTH = 4;
  DEF_FIND_COMMAND = '[FPUZSVHMLETRAQC]';
type

  TDrawFunc = procedure(Surface: ISurface; C: TColor);
  TDrawFuncArray = array of TDrawFunc;

  TDrawCommand = record
    DrawFunc: TMethodEvent;
    Params: FloatArray;
  end;

  TDrawCommandList = specialize TArrayList<TDrawCommand>;

  { TPathIconDrawer }

  TPathIconDrawer = class
  private
    FSurface: ISurface;
    FParams: FloatArray;
    FDrawCommandList: TDrawCommandList;

    FPenColor,
    FBackColor,
    FAccentColor: TColorB;
    FLastPoint: TPointF;
    FPenWidth: Float;
    procedure AddCommandToList(const aPathData: String);
    procedure CheckParamCount(const Params: FloatArray; n: integer);
    procedure DoDrawAll;
    function GetCurrentColor: TColorB;
    function GetCurrentPen: IPen;
    function GetCurrentPenWidth: Float;
    function GetCurrentPreserve: boolean;
    procedure SetDrawParams(const Params: FloatArray; AInputStr: string);

    procedure DoFill;
    procedure DoStroke;
    procedure DoClip;
    procedure DoUnclip;
    procedure DoClosePath;
    procedure DoMove;
    procedure DoLine;
    procedure DoVertLine;
    procedure DoHorzLine;
    procedure DoEllipse;
    procedure DoRectangle;
    procedure DoRoundRectangle;
    procedure DoArc;
    procedure DoCubicBezier;
    procedure DoQuadraticBezier;
    procedure SetPenColor(AValue: TColorB);

  protected

  public
    constructor Create;
    procedure ParsePathData(PathData: string);
    procedure Draw(Surface: ISurface; DrawRect: TRectI; aAngle: integer; aScale: double);
    property ForegroundColor: TColorB read FPenColor write SetPenColor;
    property AccentColor: TColorB read FAccentColor write FAccentColor;
    property BackColor: TColorB read FBackColor write FBackColor;
  end;

implementation

uses
  Utils;

const
  //DEF_FIND_COMMAND = '[FPUZSVHMLETRAQC]';
  DEF_FIND_FLOAT = '\s*-*\d+(?:\.\d+)?';

{ TPathIconDrawer }

procedure TPathIconDrawer.AddCommandToList(const aPathData: String);
var
  adc: TDrawCommand;
  n: integer = 0;
begin
  if aPathData.IsEmpty then exit;

  adc.Params.Clear;
  adc.DrawFunc:= nil;

  SetDrawParams(adc.Params, aPathData.Copy(2));

  case aPathData[1] of
    'F': adc.DrawFunc:= @DoFill;
    'P': adc.DrawFunc:= @DoClip;
    'U': adc.DrawFunc:= @DoUnclip;
    'Z': adc.DrawFunc:= @DoClosePath;
    'S': adc.DrawFunc:= @DoStroke;
    'V': begin
        adc.DrawFunc:= @DoVertLine;
        n:=1;
      end;
    'H': begin
        adc.DrawFunc:= @DoHorzLine;
        n:=1;
      end;
    'M': begin
        adc.DrawFunc:= @DoMove;
        n:=2;
      end;
    'L': begin
        adc.DrawFunc:= @DoLine;
        n:=2;
      end;
    'E': begin
        adc.DrawFunc:= @DoEllipse;
        n:=4;
      end;
    'T': begin
        adc.DrawFunc:= @DoRectangle;
        n:=4;
      end;
    'R': begin
        adc.DrawFunc:= @DoRoundRectangle;
        n:=5;
      end;
    'A': begin
        adc.DrawFunc:= @DoArc;
        n:=6;
      end;
    'Q': begin
        adc.DrawFunc:= @DoQuadraticBezier;
        n:=4;
      end;
    'C': begin
        adc.DrawFunc:= @DoCubicBezier;
        n:=6;
      end;
  end;

  if Assigned(adc.DrawFunc) then
  begin
    CheckParamCount(adc.Params, n);
    FDrawCommandList.Push(adc);
  end;
end;

procedure TPathIconDrawer.CheckParamCount(const Params: FloatArray; n: integer);
begin
  if n<1 then exit;
  if Params.Length>=n then exit;
  while Params.Length<n do Params.Push(0);
end;

function TPathIconDrawer.GetCurrentColor: TColorB;
begin
  Result:= FPenColor;
  if FParams.Length<1 then exit;
  case round(FParams[0]) of
    1: Result:= FAccentColor;
    2: Result:= FBackColor;
  end;
end;

function TPathIconDrawer.GetCurrentPen: IPen;
begin
  Result:= NewPen(GetCurrentColor, GetCurrentPenWidth);
  Result.LineCap:=cpRound;
  Result.LineJoin:=jnRound;
end;

function TPathIconDrawer.GetCurrentPenWidth: Float;
begin
  if FParams.Length<3 then
    Result:= FPenWidth
  else
    Result:= FParams[2];
end;

function TPathIconDrawer.GetCurrentPreserve: boolean;
begin
  Result:= (FParams.Length>1) and (FParams[1]>0);
end;

procedure TPathIconDrawer.SetDrawParams(const Params: FloatArray;
  AInputStr: string);
var
  s: string;
  re: TRegExpr;
  v: Float;
  fs: TFormatSettings;
begin
  Params.Clear;
  if AInputStr.IsEmpty then exit;

  fs:= FormatSettings;
  fs.DecimalSeparator:='.';
  re:= TRegExpr.Create(DEF_FIND_FLOAT);
  if re.Exec(AInputStr) then
    repeat
      s:= AInputStr.Copy(re.MatchPos[0], re.MatchLen[0]);
      v:= SysUtils.StrToFloatDef(s, 0, fs);
      Params.Push(v);
    until not re.ExecNext;
  re.Free;
end;

procedure TPathIconDrawer.ParsePathData(PathData: string);
var
  re: TRegExpr;
  pPos, aPos: integer;
begin
  FDrawCommandList.Clear;
  re:= TRegExpr.Create(DEF_FIND_COMMAND);
  pPos:= -1;
  if re.Exec(PathData) then
  repeat
    aPos:= re.MatchPos[0];
    if pPos>=0 then
      AddCommandToList(PathData.Copy(pPos, aPos- pPos));
    pPos:= aPos;
  until not re.ExecNext;
  AddCommandToList(PathData.Copy(pPos, 1000));
  re.Free;
end;

procedure TPathIconDrawer.DoFill;
begin
  FSurface.Fill(NewBrush(GetCurrentColor), GetCurrentPreserve);
end;

procedure TPathIconDrawer.DoStroke;
begin
  FSurface.Stroke(GetCurrentPen, GetCurrentPreserve);
end;

procedure TPathIconDrawer.DoClip;
begin
  FSurface.Path.Clip;
end;

procedure TPathIconDrawer.DoUnclip;
begin
  FSurface.Path.Unclip;
end;

procedure TPathIconDrawer.DoClosePath;
begin
  FSurface.Path.Close;
end;

procedure TPathIconDrawer.DoMove;
begin
  FSurface.MoveTo(FParams[0], FParams[1]);
  FLastPoint:= TPointF.Create(FParams[0], FParams[1]);
end;

procedure TPathIconDrawer.DoLine;
begin
  FSurface.LineTo(FParams[0], FParams[1]);
  FLastPoint:= TPointF.Create(FParams[0], FParams[1]);
end;

procedure TPathIconDrawer.DoVertLine;
begin
  FSurface.LineTo(FLastPoint.X, FParams[0]);
  FLastPoint.Y:= FParams[0];
end;

procedure TPathIconDrawer.DoHorzLine;
begin
  FSurface.LineTo(FParams[0], FLastPoint.Y);
  FLastPoint.X:= FParams[0];
end;

procedure TPathIconDrawer.DoEllipse;
begin
  FSurface.Ellipse(
    TRectF.Create(FParams[0], FParams[1], FParams[2], FParams[3]));//Rect
end;

procedure TPathIconDrawer.DoRectangle;
begin
  FSurface.Rectangle(
    TRectF.Create(FParams[0], FParams[1], FParams[2], FParams[3]));//Rect
end;

procedure TPathIconDrawer.DoRoundRectangle;
begin
  FSurface.RoundRectangle(
    TRectF.Create(FParams[0], FParams[1], FParams[2], FParams[3]), //Rect
    FParams[4]);                                                   //Radius
end;

procedure TPathIconDrawer.DoArc;
begin
  FSurface.ArcTo(
    TRectF.Create(FParams[0], FParams[1], FParams[2], FParams[3]), //Rect
    DegToRad(FParams[4]),                                          //BeginAngle
    DegToRad(FParams[5]));                                         //EndAngle
end;

procedure TPathIconDrawer.DoCubicBezier;
begin
  FSurface.CurveTo(
    FParams[0], FParams[1],                                        //X, Y
    TPointF.Create(FParams[2], FParams[3]),                        //C1
    TPointF.Create(FParams[4], FParams[5]));                       //C2
  FLastPoint:= TPointF.Create(FParams[0], FParams[1]);
end;

procedure TPathIconDrawer.DoQuadraticBezier;
begin
  FSurface.CurveTo(
    FParams[0], FParams[1],                                        //X, Y
    TPointF.Create(FParams[2], FParams[3]),                        //C1
    TPointF.Create(FParams[2], FParams[3]));                       //C1
  FLastPoint:= TPointF.Create(FParams[0], FParams[1]);
end;

procedure TPathIconDrawer.SetPenColor(AValue: TColorB);
begin
  if FPenColor=AValue then Exit;
  FPenColor:=AValue;
end;

constructor TPathIconDrawer.Create;
begin
  inherited;
  FPenColor:=DefIconPenColor;
  FBackColor:= DefIconBackColor;
  FAccentColor:= DefIconAccentColor;
  FPenWidth:= DEF_PEN_WIDTH;
end;

procedure TPathIconDrawer.DoDrawAll;
var
  C: TDrawCommand;
begin
  FLastPoint:= TPointF.Create(0,0);
  for C in FDrawCommandList do
  begin
    FParams:= C.Params;
    C.DrawFunc;
  end;
end;

procedure TPathIconDrawer.Draw(Surface: ISurface; DrawRect: TRectI;
  aAngle: integer; aScale: double);
var
  R: TRectF;
  a : integer;
  w, h: float;
  dx: float = 0;
  dy: float = 0;
begin
  if aScale<0.01 then exit;
  FSurface:= Surface;
  if FSurface= nil then exit;

  R:= QuadratRectF(DrawRect);
  FSurface.Matrix.Identity;
  w:= aScale*R.Width;
  h:= aScale*R.Height;
  FSurface.Matrix.Scale(w/DEF_ICON_SIZE, h/DEF_ICON_SIZE);
  a:= aAngle mod 360;
  if a<>0 then
  begin
    dx:=  w/2;
    dy:= h/2;
    FSurface.Matrix.Translate( -dx, -dy);
    FSurface.Matrix.Rotate(pi*a/180);
  end;
  dx-=(aScale - 1)*R.Width/2;
  dy-=(aScale- 1)*R.Height/2;
  FSurface.Matrix.Translate(dx+R.X, dy+R.Y);
  DoDrawAll;
  FSurface.Matrix.Identity;
  FSurface:= nil;
end;

end.

