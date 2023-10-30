// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit Utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls,  Graphics,

  Codebot.System, Codebot.Graphics, Codebot.Graphics.Types;

const
  DefMarginSize = 6;

procedure FindLightenDarken(C1, C2: TColorB; aParam: integer; aStrings: TStrings);
procedure FindDarkenLighten(C1, C2: TColorB; aParam: integer; aStrings: TStrings);
function GetAccentColorB: TColorB;
function GetDisabledAccentColorB: TColorB;
//function DPIScale: double;
function MaxInteger(a, b: Integer): Integer; inline;
function MinInteger(a, b: Integer): Integer; inline;
function MaxFloat(a, b: Float): Float; inline;
function MinFloat(a, b: Float): Float; inline;
function MaxDouble(a, b: double): double; inline;
function MinDouble(a, b: double): double; inline;
function EnsureRangeFloat(aMin, aMax, aValue: float): float;
function EnsureRangeDouble(aMin, aMax, aValue: double): double;
function QuadratRectI(R: TRectI): TRectI;
function QuadratRectF(R: TRectF): TRectF;
procedure SwapFloat(var a, b: Float);
procedure SwapPointF(var a, b: TPointF);
function NewRoundPen(C: TColorB; W: Float): IPen;
procedure StrokeLine(Surface: iSurface; pt1, pt2: TPointF; Pen: IPen);


const
  //DefBorderColor: TColorB =  (Blue: 0; Green: 0; Red: 0; Alpha: $9A); // TColorB($868686);    // gray
  DefGrayColor: TColorB   =  (Blue: 0; Green: 0; Red: 0; Alpha: $72); // TColorB($868686);    // gray
  DefWhiteColor: TColorB  =  (Blue: $FF; Green: $FF; Red: $FF; Alpha: $FF);
  DefBlackColor: TColorB  =  (Blue: 0; Green: 0; Red: 0; Alpha: $FF);
  //DefWindowColor = clWindow;

implementation
// Common functions

procedure FindLightenDarken(C1, C2: TColorB; aParam: integer; aStrings: TStrings);
var
  i, k: byte;
  C: TColorB;
  a: integer;
begin
  for i:=0 to 255 do
  begin
    for k:= 0 to 255 do
    begin
      C:= Lighten(Darken(C1, k/255), i/255);
      a:= abs(C.Red - C2.Red) + abs(C.Green - C2.Green) + abs(C.Blue - C2.Blue);
      if a< aParam then
        aStrings.Add('i='+IntToStr(i)+' k='+IntToStr(k)+' a='+IntToStr(a));
    end;
  end;
end;

procedure FindDarkenLighten(C1, C2: TColorB; aParam: integer; aStrings: TStrings);
var
  i, k: byte;
  C: TColorB;
  a: integer;
begin
  for i:=0 to 255 do
  begin
    for k:= 0 to 255 do
    begin
      C:= Darken(Lighten(C1, i/255), k/255);
      a:= abs(C.Red - C2.Red) + abs(C.Green - C2.Green) + abs(C.Blue - C2.Blue);
      if a< aParam then
        aStrings.Add('i='+IntToStr(i)+' k='+IntToStr(k)+' a='+IntToStr(a));
    end;
  end;
end;

function GetAccentColorB: TColorB;
begin
  Result:= Darken(clHighlight, 37/255);
end;

function GetDisabledAccentColorB: TColorB;
begin
  Result:= GetAccentColorB.Lighten(100/255);
end;

//function DPIScale: double;
//begin
//  Result:= ScreenInfo.PixelsPerInchX/96;
//end;

function MaxInteger(a, b: Integer): Integer;
begin
  if a> b then Result:= a
  else Result:= b;
end;

function MinInteger(a, b: Integer): Integer;
begin
  if a< b then Result:= a
  else Result:= b;
end;

function MaxFloat(a, b: Float): Float;
begin
  if a> b then Result:= a
  else Result:=b;
end;

function MinFloat(a, b: Float): Float;
begin
  if a< b then Result:= a
  else Result:=b;
end;

function MaxDouble(a, b: double): double;
begin
  if a> b then Result:= a
  else Result:=b;
end;

function MinDouble(a, b: double): double;
begin
  if a< b then Result:= a
  else Result:=b;
end;

function EnsureRangeFloat(aMin, aMax, aValue: float): float;
begin
  if aValue< aMin then Result:= aMin
  else if aValue> aMax then Result:= aMax
  else Result:= aValue;
end;

function EnsureRangeDouble(aMin, aMax, aValue: double): double;
begin
  if aValue< aMin then Result:= aMin
  else if aValue> aMax then Result:= aMax
  else Result:= aValue;
end;

function QuadratRectI(R: TRectI): TRectI;
var
  QuadSize: integer;
begin
  QuadSize:= MinInteger(R.Width, R.Height);
  Result:= TRectI.Create(QuadSize, QuadSize);
  Result.Center(R.MidPoint);
end;

function QuadratRectF(R: TRectF): TRectF;
var
  QuadSize: float;
begin
  QuadSize:= MinFloat(R.Width, R.Height);
  Result:= TRectF.Create(QuadSize, QuadSize);
  Result.Center(R.MidPoint);
end;

procedure SwapFloat(var a, b: Float);
var
  x: Float;
begin
  x:= a;
  a:= b;
  b:= x;
end;

procedure SwapPointF(var a, b: TPointF);
var
  pt: TPointF;
begin
  pt:= a;
  a:= b;
  b:= pt;
end;

function NewRoundPen(C: TColorB; W: Float): IPen;
begin
  Result:= NewPen(C, W);
  Result.LineCap:= cpRound;
end;

procedure StrokeLine(Surface: iSurface; pt1, pt2: TPointF; Pen: IPen);
begin
  Surface.MoveTo(pt1.X, pt1.Y);
  Surface.LineTo(pt2.X, pt2.Y);
  Surface.Stroke(Pen);
end;

end.

