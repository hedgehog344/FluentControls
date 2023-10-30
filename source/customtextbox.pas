// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit CustomTextBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, StdCtrls, Controls, Graphics, LCLType, Types,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types;
type

  { TCustomTextBox }

  TCustomTextBox = class(TCustomControl)
  private
    FHeader: TTranslateString;
    FOnChange: TNotifyEvent;
    FOnEditEditingDone: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FRadius: Float;
    FIsFocused: boolean;
    FIsLoaded: boolean;
    FEdit: TEdit;
    FEditRect: TRectI;
    FHeaderLabel: TLabel;

    procedure EnterEdit(Sender: TObject);
    procedure ExitEdit(Sender: TObject);
    procedure ChangeEdit(Sender: TObject);
    procedure EditingDoneEdit(Sender: TObject);
    procedure DoSetEditFont;
    procedure DrawBottomLine(const Surface: ISurface; R: TRectF);
    function GetAlignment: TAlignment;
    function GetCanUndo: Boolean;
    function GetCaretPos: TPoint;

    function GetMaxLength: Integer;
    function GetModified: Boolean;
    function GetPlaceholderText: TTranslateString;
    function GetReadOnly: Boolean;
    function GetSelText: String;
    function GetTabStop: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCaretPos(AValue: TPoint);
    procedure SetHeader(AValue: TTranslateString);

    procedure SetMaxLength(AValue: Integer);
    procedure SetModified(AValue: Boolean);
    procedure SetPlaceholderText(AValue: TTranslateString);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetSelText(AValue: String);
    procedure SetTabStop(AValue: Boolean);

  protected
    procedure Paint; override;
    procedure Render(const Surface: ISurface; R: TRectI);
    procedure DoChangeEdit; virtual;
    procedure DoArrangeCtrls; virtual;
    function GetRightSpace: integer; virtual;
    function GetLeftSpace: integer; virtual;
    procedure FontChanged(Sender: TObject); override;
    procedure DoOnResize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                {%H-}WithThemeSpace: Boolean); override;
    procedure TextChanged; override;
    function GetPreferredHeight: integer;
    class function GetControlClassDefaultSize: TSize; override;
    procedure EnabledChanging; override;
    procedure RealSetText(const AValue: TCaption); override;
    function RealGetText: TCaption; override;
    procedure Loaded; override;
    property Edit: TEdit read FEdit;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EraseBackground({%H-}DC: HDC); override;
 //   procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
 //   procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
    procedure SetFocus; override;
    function Focused: Boolean; override;
    procedure Clear;
    procedure ClearSelection; virtual;
    procedure CopyToClipboard; virtual;
    procedure CutToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure SelectAll;
    procedure Undo; virtual;

    property Header: TTranslateString read FHeader write SetHeader;
    property Modified: Boolean read GetModified write SetModified;

    property CanUndo: Boolean read GetCanUndo;
    property CaretPos: TPoint read GetCaretPos write SetCaretPos;
    property SelText: String read GetSelText write SetSelText;

    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property AutoSize default true;
    property BorderSpacing;


    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property PlaceholderText: TTranslateString read GetPlaceholderText write SetPlaceholderText;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEditingDone: TNotifyEvent read FOnEditEditingDone write FOnEditEditingDone;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property TabStop: Boolean read GetTabStop write SetTabStop default True;
  end;

implementation

uses
  Utils;

{ TCustomTextBox }

procedure TCustomTextBox.DrawBottomLine(const Surface: ISurface; R: TRectF);
var
  C: TColorB;
begin
  if not Enabled then exit;
  if FIsFocused then
  begin
    C:= GetAccentColorB;
    R.Top:= R.Bottom - Scale96ToForm(20)/10;
    Surface.Rectangle(R);
    Surface.Path.Clip;
    R.Top:= R.Bottom - 2*FRadius;
    Surface.FillRoundRect(NewBrush(C), R, FRadius);
    Surface.Path.Unclip;
  end
  else
  begin
    R.Inflate(-FRadius, 0);
    C:= Darken(ParentCurrentColor, 115/255);
    R.Top:= R.Bottom - Scale96ToForm(1);
    Surface.FillRect(NewBrush(C), R);
  end;
end;

function TCustomTextBox.GetAlignment: TAlignment;
begin
  Result:= FEdit.Alignment;
end;

function TCustomTextBox.GetCanUndo: Boolean;
begin
  Result:= FEdit.CanUndo;
end;

function TCustomTextBox.GetCaretPos: TPoint;
begin
  Result:= FEdit.CaretPos;
end;


function TCustomTextBox.GetMaxLength: Integer;
begin
  Result:= FEdit.MaxLength;
end;

function TCustomTextBox.GetModified: Boolean;
begin
  Result:= FEdit.Modified;
end;

function TCustomTextBox.GetPlaceholderText: TTranslateString;
begin
  Result:= FEdit.TextHint;
end;

function TCustomTextBox.GetReadOnly: Boolean;
begin
  Result:= FEdit.ReadOnly;
end;

function TCustomTextBox.GetSelText: String;
begin
  Result:= FEdit.SelText;
end;

function TCustomTextBox.GetTabStop: Boolean;
begin
  Result := FEdit.TabStop;
end;

procedure TCustomTextBox.SetAlignment(AValue: TAlignment);
begin
  FEdit.Alignment:= AValue;
end;

procedure TCustomTextBox.SetCaretPos(AValue: TPoint);
begin
  FEdit.CaretPos:= AValue;
end;

procedure TCustomTextBox.SetHeader(AValue: TTranslateString);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;
  if FHeader = '' then
  begin
    if FHeaderLabel<>nil then
      FHeaderLabel.Visible:= false;
  end
  else
  begin
    if FHeaderLabel=nil then
    begin
      FHeaderLabel:= TLabel.Create(self);
      FHeaderLabel.AutoSize:=false;
      FHeaderLabel.Layout:= tlCenter;
      FHeaderLabel.Font.Assign(FEdit.Font);
      FHeaderLabel.Parent:= self;
    end
    else FHeaderLabel.Visible:= true;
    FHeaderLabel.Caption:= FHeader;
  end;
  Height:= self.GetPreferredHeight;
  InvalidatePreferredSize;
  AdjustSize;
  Invalidate;
end;

procedure TCustomTextBox.SetMaxLength(AValue: Integer);
begin
  FEdit.MaxLength:= AValue;
end;

procedure TCustomTextBox.SetModified(AValue: Boolean);
begin
  FEdit.Modified:= AValue;
end;

procedure TCustomTextBox.SetPlaceholderText(AValue: TTranslateString);
begin
  FEdit.TextHint:= AValue;
end;

procedure TCustomTextBox.SetReadOnly(AValue: Boolean);
begin
  FEdit.ReadOnly:= AValue;
end;

procedure TCustomTextBox.SetSelText(AValue: String);
begin
  FEdit.SelText:= AValue;
end;

procedure TCustomTextBox.SetTabStop(AValue: Boolean);
begin
  FEdit.TabStop := AValue;
end;

procedure TCustomTextBox.Render(const Surface: ISurface; R: TRectI);
var
  BackColor: TColorB;
begin
  BackColor:=  ParentCurrentColor;
  Surface.Clear(BackColor);
  Surface.FillRoundRect(NewBrush(FEdit.Color), R, FRadius);
  Surface.StrokeRoundRect(NewPen(Darken(BackColor, 15/255)), R, FRadius);
  if Enabled then
    DrawBottomLine(Surface, R);

end;

procedure TCustomTextBox.Paint;
var
  Surface: ISurface;
begin
  Surface:= NewSurface(Canvas);
  if Surface<>nil then
  begin
    Render(Surface, FEditRect);
    Surface.Flush;
  end;
//  inherited Paint;
end;

procedure TCustomTextBox.EnterEdit(Sender: TObject);
begin
  FIsFocused:= true;
  if Assigned(FOnEnter) then FOnEnter(self);
  Invalidate;
end;

procedure TCustomTextBox.ExitEdit(Sender: TObject);
begin
  FIsFocused:= false;
  if Assigned(FOnExit) then FOnExit(self);
  Invalidate;
end;

procedure TCustomTextBox.DoChangeEdit;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TCustomTextBox.ChangeEdit(Sender: TObject);
begin
  DoChangeEdit;
end;

procedure TCustomTextBox.EditingDoneEdit(Sender: TObject);
begin
  if Assigned(FOnEditEditingDone) then FOnEditEditingDone(self);
end;

procedure TCustomTextBox.DoSetEditFont;
var
  fs, fh: integer;
begin
  fs:= Font.Size;
  if fs=0 then fs:=11;
  fh:= trunc(96*Scale96ToFont(fs)) div 72;
  FEdit.Font.BeginUpdate;
    FEdit.Font.Assign(Font);
    FEdit.Font.Height:= -fh;
  FEdit.Font.EndUpdate;
end;

procedure TCustomTextBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  if (not FIsLoaded) or HandleAllocated then
  begin
    if FHeaderLabel<>nil then
      FHeaderLabel.Font.Assign(FEdit.Font);
    DoSetEditFont;
  end;
end;

procedure TCustomTextBox.DoArrangeCtrls;
var
  LS, RS, H: integer;

begin
  FEditRect:= ClientRect;
  if (FHeader<>'') and (FHeaderLabel<>nil) then
  begin
    FEditRect.Top:= FEditRect.Height div 2;
    FHeaderLabel.SetBounds(0, 0, FEditRect.Width, FEditRect.Height);
  end;
  H:= FEdit.Height;
  LS:= GetLeftSpace;
  RS:= GetRightSpace;
  FEdit.SetBounds(LS, FEditRect.Top + (FEditRect.Height - H) div 2, FEditRect.Width- LS- RS, H);
end;

function TCustomTextBox.GetRightSpace: integer;
begin
  Result:=Scale96ToForm(8);
end;

function TCustomTextBox.GetLeftSpace: integer;
begin
  Result:=Scale96ToForm(8);
end;

procedure TCustomTextBox.DoOnResize;
begin
  DoArrangeCtrls;
  inherited DoOnResize;
end;

procedure TCustomTextBox.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:= GetPreferredHeight;
end;

procedure TCustomTextBox.TextChanged;
begin
  inherited TextChanged;
end;

function TCustomTextBox.GetPreferredHeight: integer;
var
  h: integer;
begin
  h:= FEdit.Height;
  Result:=  h + self.Scale96ToScreen(12);
  if FHeader<>'' then Result:= 2*Result;
end;

class function TCustomTextBox.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 170;
  Result.CY := 32;
end;

procedure TCustomTextBox.EnabledChanging;
begin
  inherited;
  if Enabled then
    FEdit.Color:=Lighten(Darken(ParentCurrentColor, 2/150), 75/255).Color
  else
    FEdit.Color:= clWindow;
end;

procedure TCustomTextBox.RealSetText(const AValue: TCaption);
begin
  FEdit.Text := AValue;
end;

function TCustomTextBox.RealGetText: TCaption;
begin
  Result:= FEdit.Text;
end;

procedure TCustomTextBox.Loaded;
begin
  FIsLoaded:= true;
  inherited Loaded;

end;

constructor TCustomTextBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsFocused:= false;
  ParentColor:= true;

  ControlStyle := ControlStyle + [csNoFocus];

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  FRadius:= Scale96ToScreen(44)/10;

  FEdit:= TEdit.Create(self);
  FEdit.Anchors:= [];
  FEdit.Color:= clWindow;
  FEdit.BorderStyle:= bsNone;
  FEdit.ParentFont:= false;
  FEdit.OnEnter := @EnterEdit;
  FEdit.OnExit := @ExitEdit;
  FEdit.OnChange:=@ChangeEdit;
  FEdit.OnEditingDone:= @EditingDoneEdit;

  DoSetEditFont;
  FEdit.Parent:= self;
  AutoSize:=true;
end;

procedure TCustomTextBox.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;

procedure TCustomTextBox.SetFocus;
begin
  FEdit.SetFocus;
end;

function TCustomTextBox.Focused: Boolean;
begin
  Result := FEdit.Focused;
end;

procedure TCustomTextBox.Clear;
begin
  FEdit.Clear;
end;

procedure TCustomTextBox.ClearSelection;
begin
  FEdit.ClearSelection;
end;

procedure TCustomTextBox.CopyToClipboard;
begin
  FEdit.CopyToClipboard;
end;

procedure TCustomTextBox.CutToClipboard;
begin
  FEdit.CutToClipboard;
end;

procedure TCustomTextBox.PasteFromClipboard;
begin
  FEdit.PasteFromClipboard;
end;

procedure TCustomTextBox.SelectAll;
begin
  FEdit.SelectAll;
end;

procedure TCustomTextBox.Undo;
begin
  FEdit.Undo;
end;

end.

