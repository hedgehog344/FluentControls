// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit PathIconEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
  tdCtrls, Dialogs, RegExpr,

  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentPathIcon,
  FluentIconView;

type

  { TPathIconEditorForm }

  TPathIconEditorForm = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    ButtonApply: TButton;
    ColorButtonAccentColor: TColorButton;
    ColorButtonColor: TColorButton;
    EditCodePoint: TEdit;
    FluentIconView: TFluentIconView;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MemoPath: TMemo;
    MemoHelp: TMemo;
    procedure ButtonApplyClick(Sender: TObject);
    procedure ColorButtonAccentColorColorChanged(Sender: TObject);
    procedure ColorButtonColorColorChanged(Sender: TObject);
    procedure EditCodePointEditingDone(Sender: TObject);
    procedure FluentIconViewRenderGlyph(Sender: TObject; Surface: ISurface;
      Rect: TRectI);
    procedure MemoPathChange(Sender: TObject);
  private
    procedure AddToMemo(AInputStr: string; SL: TStrings);
    procedure DrawCodePoint;

  public
    procedure SetPathIcon(Src: TPathIcon);
  end;

implementation

{$R *.lfm}

{ TPathIconEditorForm }

procedure TPathIconEditorForm.ButtonApplyClick(Sender: TObject);
var
  S: string;
begin
  S:= StringReplace(MemoPath.Text, LineEnding, '', [rfReplaceAll]);
  FluentIconView.PathIcon.PathString:= S;
  ButtonApply.Enabled:= false;
end;

procedure TPathIconEditorForm.ColorButtonAccentColorColorChanged(Sender: TObject
  );
begin
  FluentIconView.PathIcon.AccentColor:= ColorButtonAccentColor.ButtonColor;
  FluentIconView.Invalidate;
end;

procedure TPathIconEditorForm.ColorButtonColorColorChanged(Sender: TObject);
begin
  FluentIconView.PathIcon.ForegroundColor:= ColorButtonColor.ButtonColor;
  FluentIconView.Invalidate;
end;

procedure TPathIconEditorForm.DrawCodePoint;
var
  cp: integer;
begin
  cp:= FluentIconView.PathIcon.CodePoint;
  if cp>0 then EditCodePoint.Text:= '$'+IntToHex(FluentIconView.PathIcon.CodePoint, 4)
  else EditCodePoint.Text:= '';
end;


procedure TPathIconEditorForm.AddToMemo(AInputStr: string; SL: TStrings);
const
  CharStr = 'FPUZSVHMLETRAQC';//'FPUCSMLETRAQC';
var
  k, count: integer;
  p1, p2: integer;
begin
  count:= AInputStr.Length;
  p1:=-1;
  for k:=1 to count do
  begin
    if CharStr.IndexOf(AInputStr[k])>=0 then
    begin
      p2:= k;
      if p1>=0 then SL.Add(AInputStr.Substring(p1-1, p2-p1));
      p1:=p2;
    end;
  end;
  if p1>0 then SL.Add(AInputStr.Substring(p1-1, MaxInt));
end;

procedure TPathIconEditorForm.EditCodePointEditingDone(Sender: TObject);
var
  cp: integer;
begin
  cp:= StrToIntDef(EditCodePoint.Text, 0);
  if cp>0 then
  begin
    FluentIconView.PathIcon.CodePoint:= cp;
    AddToMemo(FluentIconView.PathIcon.PathString, MemoPath.Lines);
    ButtonApply.Enabled:= false;
  end;
  DrawCodePoint;
end;

procedure TPathIconEditorForm.FluentIconViewRenderGlyph(Sender: TObject;
  Surface: ISurface; Rect: TRectI);
begin
  Surface.FillRect(Brushes.Transparent, Rect);
end;

procedure TPathIconEditorForm.MemoPathChange(Sender: TObject);
begin
  ButtonApply.Enabled:= true;
end;

procedure TPathIconEditorForm.SetPathIcon(Src: TPathIcon);
begin
  if Src=nil then exit;
  FluentIconView.PathIcon.Assign(Src);
  AddToMemo(FluentIconView.PathIcon.PathString, MemoPath.Lines);
  ButtonApply.Enabled:= false;
  DrawCodePoint;
  ColorButtonColor.ButtonColor:= FluentIconView.PathIcon.ForegroundColor;
  ColorButtonAccentColor.ButtonColor:= FluentIconView.PathIcon.AccentColor;
end;


end.

