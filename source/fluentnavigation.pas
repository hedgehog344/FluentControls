// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentNavigation;

//https://learn.microsoft.com/windows/apps/design/controls/navigationview

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, Controls, ExtCtrls, Graphics, LCLType,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentGraphicControl,
  FluentPathIcon,
  FluentIconList,
  PathIconDrawer;

type

  { TNavigationViewItem }

  TNavigationViewItem = class(TCollectionItem)
  private
    FCaption: TTranslateString;
    FEnabled: boolean;
    FIcon: TPathIcon;
    FIconDrawer: TPathIconDrawer;
    FOnClick: TNotifyEvent;
    FPage: TPage;
    FTag: PtrInt;
    procedure IconChange(Sender: TObject);
    procedure SetCaption(AValue: TTranslateString);
    procedure SetEnabled(AValue: boolean);
    procedure SetIcon(AValue: TPathIcon);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Click;
  published
    property Caption: TTranslateString read FCaption write SetCaption;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Icon: TPathIcon read FIcon write SetIcon;
    property Tag: PtrInt read FTag write FTag default 0;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Page: TPage read FPage write FPage;
  end;

  { TNavigationViewItemEnumerator }

  TNavigationViewItemEnumerator = class(TCollectionEnumerator)
  public
    function GetCurrent: TNavigationViewItem;
    property Current: TNavigationViewItem read GetCurrent;
  end;

  TCustomFluentNavigationView = class;

  { TNavigationViewItems }

  TNavigationViewItems = class(TOwnedCollection)
  private
    FOwner: TCustomFluentNavigationView;
    function GetMenuItem(AIndex: Integer): TNavigationViewItem;
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TCustomFluentNavigationView);
    function Add: TNavigationViewItem;
    function GetEnumerator: TNavigationViewItemEnumerator; inline;
    property MenuItem[AIndex: Integer]: TNavigationViewItem read GetMenuItem; default;
  end;

  TSelectMenuItemEvent = procedure (Sender: TObject; Item: TNavigationViewItem) of object;

  TPageStack = specialize TArrayList<integer>;

  { TCustomFluentNavigationView }

  TCustomFluentNavigationView = class(TFluentGraphicControl)
  private
    FInternalFont: TFont;
    FOnChangeNavigationStatus: TNotifyEvent;
    FPageStack: TPageStack;
    FShowMenuButton: boolean;
    FStrongScrollBar: boolean;
    FScrollBarSize: integer;
    FBandRect: TRectF;
    FScrollBarVisible: boolean;
    FScrollBarCapture: boolean;
    FScrollBarCaptureY: integer;
    FCaptureTopHeight: integer;

    FItemHeight: integer;

    FHoverMenuItem: integer;
    FPressedMenuItem: integer;
    FSelectMenuItem: integer;
    FMenuItemsRect: TRectI;

    FHoverNavButton: boolean;
    FPressedNavButton: boolean;
    FNavButtonRect: TRectI;

    FScrollTopHeight: integer;
    FScrollHeight: integer;
    FClientHeight: integer;
    FClientWidth: integer;

 //   FFontSize: Float;



    FCompactMode: boolean;



    FNavigationViewItems: TNavigationViewItems;
    FOnSelectMenuItem: TSelectMenuItemEvent;

    FNavButtonDrawer: TPathIconDrawer;
    FNavButtonState: TDrawState;

    procedure ChangeInternalFont;
    procedure DoChangeInternalFont(Sender: TObject);
    procedure DoSelectItem(Value: Integer);
    procedure DrawBlueMarker(R: TRectI);
    function GetBackColorFromState(aState: TDrawState): TColorB;
    function GetCompactModeWidth: integer;
 //   function GetFontColorFromState(aState: TDrawState): TColorB;
 //   function GetFontFromState(aState: TDrawState): IFont;
    function GetNormalWidth: integer;
    function GetPageStackCount: integer;
    function GetPrefferedWidth: integer;
    procedure PushToStack(k: Integer);
    procedure RenderMenuItem(R: TRectI; k: integer);
    procedure RenderMenuButton(R: TRectI);
    procedure RenderMenuItemState(R: TRectI; const it: TPathIconDrawer;
      aCaption: string; aState: TDrawState);
    procedure RenderScrollBars(R: TRectI);
    procedure ScrollTo(Y: integer);
    procedure SetHoverItem(Value: Integer);
    function GetEndRenderItem: integer;
    function GetFirstRenderItem: integer;
    function GetItemAtY(Y: integer): integer;
    procedure RenderMenuItems(R: TRectI);
    procedure SetCompactMode(AValue: boolean);
    procedure SetHoverNavButton(Value: boolean);
    procedure SetNavigationViewItems(AValue: TNavigationViewItems);
    procedure SetSelectMenuItem(AValue: integer);
    procedure SetShowMenuButton(AValue: boolean);


  protected
    procedure Loaded; override;
    procedure Render; override;
 //   class function GetControlClassDefaultSize: TSize; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
                {%H-}WithThemeSpace: Boolean); override;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;

    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetPageIndex(const aPage: TPage): integer;
    function GoToPage(const aPage: TPage): boolean;
    function GoBack: boolean;
    property OnSelectMenuItem: TSelectMenuItemEvent read FOnSelectMenuItem
                                                      write FOnSelectMenuItem;

    property CompactMode: boolean read FCompactMode write SetCompactMode default false;
    property MenuItems: TNavigationViewItems read FNavigationViewItems write SetNavigationViewItems;
    property SelectMenuItem: integer read FSelectMenuItem write SetSelectMenuItem;
    property OnChangeNavigationStatus: TNotifyEvent read FOnChangeNavigationStatus
      write FOnChangeNavigationStatus;
    property PageStackCount: integer read GetPageStackCount;
    property ShowMenuButton: boolean read FShowMenuButton write SetShowMenuButton default true;
  end;

  TFluentNavigationView = class(TCustomFluentNavigationView)
  published
    property Align default alLeft;
    property Color;
    property Font;
    property ParentFont;
    property OnSelectMenuItem;
    property OnChangeNavigationStatus;
    property CompactMode;
    property MenuItems;
    property SelectMenuItem;
    property ShowMenuButton;
  end;


implementation

uses math;

const
  DefScrollBarSize = 13;
  DefItemHeight = 40;
  DefNormalWidth = 320;

{ TNavigationViewItems }

function TNavigationViewItems.GetMenuItem(AIndex: Integer): TNavigationViewItem;
begin
  Result:= Items[AIndex] as TNavigationViewItem;
end;

procedure TNavigationViewItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  FOwner.Invalidate;
end;

function TNavigationViewItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

constructor TNavigationViewItems.Create(AOwner: TCustomFluentNavigationView);
begin
  inherited Create(self, TNavigationViewItem);
  FOwner := AOwner;
end;

function TNavigationViewItems.Add: TNavigationViewItem;
begin
  Result:= TNavigationViewItem(inherited Add);
end;

function TNavigationViewItems.GetEnumerator: TNavigationViewItemEnumerator;
begin
  Result := TNavigationViewItemEnumerator.Create(Self);
end;

{ TNavigationViewItemEnumerator }

function TNavigationViewItemEnumerator.GetCurrent: TNavigationViewItem;
begin
  Result := TNavigationViewItem(inherited GetCurrent);
end;

{ TNavigationViewItem }

procedure TNavigationViewItem.SetCaption(AValue: TTranslateString);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  Changed(false);
end;

procedure TNavigationViewItem.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  Changed(false);
end;

procedure TNavigationViewItem.SetIcon(AValue: TPathIcon);
begin
  FIcon.Assign(AValue);
  Changed(false);
end;

function TNavigationViewItem.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    Result := inherited GetDisplayName;
end;

procedure TNavigationViewItem.IconChange(Sender: TObject);
begin
  FIconDrawer.ParsePathData(FIcon.PathString);
  Changed(true);
end;

constructor TNavigationViewItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTag:=0;
  FIcon:= TPathIcon.Create;
  FIconDrawer:= TPathIconDrawer.Create;
  FIcon.OnPathChange:= @IconChange;

  FCaption:= 'NavigationViewItem'+IntToStr(ID+1);
  FEnabled:= true;
end;

destructor TNavigationViewItem.Destroy;
begin
  FIconDrawer.Free;
  FIcon.Free;
  inherited Destroy;
end;

procedure TNavigationViewItem.Assign(Source: TPersistent);
begin
  if Source is TNavigationViewItem then
    with TNavigationViewItem(Source) do
    begin
      self.FCaption := Caption;
      self.FEnabled := Enabled;
      self.FIcon.Assign(Icon);
    end;
  inherited Assign(Source);
end;

procedure TNavigationViewItem.Click;
begin
  if Assigned(FPage) then FPage.Show
  else if Assigned(FOnClick) then FOnClick(self);
end;

{ TCustomFluentNavigationView }

procedure TCustomFluentNavigationView.SetCompactMode(AValue: boolean);
begin
  if FCompactMode=AValue then Exit;
  FCompactMode:=AValue;
  Width:= GetPrefferedWidth;
  //debugln('============Compact modewidth = '+ IntToStr(Width));
end;

procedure TCustomFluentNavigationView.SetNavigationViewItems(AValue: TNavigationViewItems);
begin
  FNavigationViewItems.Assign(AValue);
end;

function TCustomFluentNavigationView.GetFirstRenderItem: integer;
var
  g: integer;
  k: integer=-1;
  r: integer=0;
begin
  Result:= 0;
  g:= FItemHeight div 20;
  DivMod(FScrollTopHeight, FItemHeight, k, r);
  if r>= (FItemHeight-g) then Result:= k+1
  else Result:=k;
end;

function TCustomFluentNavigationView.GetEndRenderItem: integer;
var
  g: integer;
  k: integer = -1;
  r: integer = 0;
begin
  g:= FItemHeight div 20;
  DivMod(FScrollTopHeight+ClientHeight, FItemHeight, k, r);
  if r<=g then Result:= k-1 else Result:= k;
  Result:= min(Result, FNavigationViewItems.Count-1);
end;

function TCustomFluentNavigationView.GetItemAtY(Y: integer): integer;
var
  g: integer;
  k: integer=-1;
  r: integer=0;
begin
  g:= FItemHeight div 20;
  DivMod(Y + FScrollTopHeight - FMenuItemsRect.Top, FItemHeight, k, r);
  if (k>= FNavigationViewItems.Count) or (r<= g) or (r>=(FItemHeight - g)) then
  begin
    //Menu items
    Result:=  -1;
    exit;
  end;
  Result:= k;
end;

//function TCustomFluentNavigationView.GetFontFromState(aState: TDrawState): IFont;
//begin
//  Result:= GetIFont;
//  if aState * [dsPressed, dsDisabled] <> [] then
//    Result.Color:= Result.Color.Lighten(0.4);
//end;

//function TCustomFluentNavigationView.GetFontColorFromState(aState: TDrawState): TColorB;
//begin
//  Result:= TColorB.Create(0,0,0);
//  if aState * [dsPressed, dsDisabled] <> [] then
//    Result:= Result.Lighten(0.4);
//end;

function TCustomFluentNavigationView.GetBackColorFromState(aState: TDrawState): TColorB;
begin
  if (dsSelected in aState) xor (dsHot in aState) xor (dsPressed in aState) then
    Result:= TColorB.Create(234,234,234)
  else
    Result:= TColorB.Create(237,237,237);
end;

procedure TCustomFluentNavigationView.DrawBlueMarker(R: TRectI);
var
  P: IPen;
  w, g: float;
begin
  g:= FItemHeight/20;
  w:= FItemHeight/13;
  Surface.MoveTo(R.X+0.75*g, R.Y+6*g);
  Surface.LineTo(R.X+0.75*g, R.Y+12*g);
  P:= NewPen(GetFillColor(true), w);
  P.LineCap:= cpRound;
  Surface.Stroke(P);
end;

procedure TCustomFluentNavigationView.RenderMenuItemState(R: TRectI;
  const it: TPathIconDrawer; aCaption: string; aState: TDrawState);
var
  g10: integer;
  ir, cr: TRectI;
  F: IFont;
  C: TColorB;
begin

  g10:= FItemHeight div 10;
  cr:= TRectI.Create(R.X + g10, R.Y + g10 div 2, R.Width - 2*g10, R.Height - g10);
  F:= NewFont(FInternalFont);
  if aState * [dsPressed, dsDisabled] <> [] then
  begin
    C:= F.Color.Lighten(0.4);
    F.Color:= C;
  end
  else
  begin
    C:= F.Color;
  end;

  if aState * [dsPressed, dsHot, dsSelected] <> []  then
    Surface.FillRoundRect(NewBrush(GetBackColorFromState(aState)), cr, 4);

  // Draw blue marker
  if dsSelected in aState then DrawBlueMarker(cr);

  // Draw icon
  ir:= TRectI.Create(cr.X + 3*g10, cr.Y + (5*g10) div 2, g10*4, g10*4);
  it.ForegroundColor:= C;
  it.Draw(Surface, ir, 0, 1);

  // Draw caption
  if FCompactMode or aCaption.IsEmpty then exit;
  cr.Left:= cr.X+ FItemHeight + g10;
  Surface.TextOut(F, aCaption, cr, drLeft);
end;

procedure TCustomFluentNavigationView.RenderMenuItem(R: TRectI; k: integer);
var
  aState: TDrawState;
  it: TNavigationViewItem;
begin
  it:= MenuItems[k];
  if not it.Enabled then aState:= [dsDisabled]
  else
  begin
    if k=FSelectMenuItem then aState:= [dsSelected] else aState:= [];
    if k=FHoverMenuItem then  aState+= [dsHot];
    if (k=FPressedMenuItem) then aState+= [dsPressed];
  end;
  RenderMenuItemState(R, it.FIconDrawer, it.Caption, aState);
end;

procedure TCustomFluentNavigationView.RenderMenuButton(R: TRectI);
begin
  if FScrollBarVisible then
  begin

  end;
  FNavButtonRect:= R;
  FNavButtonState:=[];
  if FHoverNavButton then
  begin
    FNavButtonState+= [dsHot];

  end
  else if FPressedNavButton then
  begin
    FNavButtonState+= [dsPressed];

  end;

  RenderMenuItemState(R, FNavButtonDrawer, '', FNavButtonState);
end;

procedure TCustomFluentNavigationView.RenderScrollBars(R: TRectI);
var
  BarHeight, BandHeight, BandWidth, BandPos: Float;
  alpha: byte;
begin
  FScrollHeight:= MenuItems.Count*FItemHeight;
  FScrollBarVisible:= FScrollHeight > FClientHeight;
  if not FScrollBarVisible then
  begin
    FBandRect:= TRectF.Create;
    exit;

  end;
  BarHeight:= R.Height;
  BandHeight:=  BarHeight*BarHeight/FScrollHeight;
  BandPos:= BarHeight*FScrollTopHeight/FScrollHeight;
  BandWidth:= FScrollBarSize;

  if FStrongScrollBar then alpha:=50
  else alpha:=25;

  FBandRect:= TRectF.Create(R.Right - BandWidth, R.Top+ BandPos, BandWidth, BandHeight);
  Surface.FillRoundRect(NewBrush(TColorB.Create(0,0,0, alpha)), FBandRect, FScrollBarSize/6);

end;

procedure TCustomFluentNavigationView.RenderMenuItems(R: TRectI);
var
  k: integer = 0;
  Rem: integer = 0;
  RF: TRectI;
begin
  Surface.Rectangle(FMenuItemsRect);
  Surface.Path.Clip;

  DivMod(FScrollTopHeight, FItemHeight, k, Rem);
  RF:= TRectI.Create(R.X, R.Y - Rem, R.Width, FItemHeight);

  while k< MenuItems.Count do
  begin
    RenderMenuItem(RF, k);
    RF.Y+= FItemHeight;
    if RF.Y >= R.Bottom then break;
    k+=1;
  end;

  Surface.Path.Unclip;
  RenderScrollBars(R);
end;

procedure TCustomFluentNavigationView.Render;
//var
//  F: IFont;
begin
//  F:= NewFont(FInternalFont);
  FItemHeight:= Scale96ToForm(round(DefItemHeight));
  FScrollBarSize:= Scale96ToForm(round(DefScrollBarSize));
  FMenuItemsRect:= ClientRect;
  FMenuItemsRect.Top:= FItemHeight;
  FClientHeight:= FMenuItemsRect.Height;
  FClientWidth:= FMenuItemsRect.Width;
  if (FClientHeight>1) and (FClientWidth>1) then
    RenderMenuItems(FMenuItemsRect);
  RenderMenuButton(TRectI.Create(GetCompactModeWidth, FItemHeight));
  inherited Render;
end;

function TCustomFluentNavigationView.GetCompactModeWidth: integer;
begin
  //if csLoading in ComponentState then
  //begin
  //  Result:= ScaleDesignToForm(48);
  //
  //end
  //else
  Result:=  Scale96ToForm(round(48));
   //debugln('Compact width = '+ IntToStr(Result));
end;

function TCustomFluentNavigationView.GetNormalWidth: integer;
begin
  Result:=  Scale96ToForm(round(320));
 //  debugln('Normal width = '+ IntToStr(Result));
end;

function TCustomFluentNavigationView.GetPageStackCount: integer;
begin
  Result:= FPageStack.Length;
end;

function TCustomFluentNavigationView.GetPrefferedWidth: integer;
begin
  if FCompactMode then
    Result:=  GetCompactModeWidth
  else
    Result:=  GetNormalWidth;
  //debugln('Preferred width = '+ IntToStr(Result));
end;

procedure TCustomFluentNavigationView.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=  GetPrefferedWidth;
  PreferredHeight:= 0;
end;

procedure TCustomFluentNavigationView.Resize;
begin
  if FScrollTopHeight > (FScrollHeight - FClientHeight) then
    FScrollTopHeight:= max(0, FScrollHeight - FClientHeight);
  inherited Resize;
end;

procedure TCustomFluentNavigationView.SetHoverItem(Value: Integer);
begin
  if Value = FHoverMenuItem then exit;
  FHoverMenuItem:= Value;
  FHoverNavButton:= false;
  Invalidate;
end;

procedure TCustomFluentNavigationView.SetHoverNavButton(Value: boolean);
begin
  if Value = FHoverNavButton then exit;
  FHoverNavButton:= Value;
  if FHoverNavButton then FHoverMenuItem:= -1;
  Invalidate;
end;

procedure TCustomFluentNavigationView.PushToStack(k: Integer);
begin
  if k>=0 then
  begin
    if FPageStack.Length>512 then FPageStack.Delete(0);
    FPageStack.Push(k);
    if Assigned(FOnChangeNavigationStatus) then FOnChangeNavigationStatus(self);
  end;
end;

procedure TCustomFluentNavigationView.DoSelectItem(Value: Integer);
var
  it: TNavigationViewItem = nil;
begin
  if (Value<0) or (Value >= MenuItems.Count) then
  begin
    FSelectMenuItem:= -1;
    exit;
  end;
  it:= MenuItems[Value];
  if it.Enabled and (FSelectMenuItem <> Value) then
  begin
    if FSelectMenuItem>= 0 then PushToStack(FSelectMenuItem);
    FSelectMenuItem:= Value;
    if (csLoading in ComponentState) then exit;

    if Assigned(FOnSelectMenuItem) then FOnSelectMenuItem(self, it);
    it.Click;
  end;
end;

procedure TCustomFluentNavigationView.SetSelectMenuItem(AValue: integer);
begin
  if FSelectMenuItem=AValue then Exit;

  DoSelectItem(AValue);
  Invalidate;
end;

procedure TCustomFluentNavigationView.SetShowMenuButton(AValue: boolean);
begin
  if FShowMenuButton=AValue then Exit;
  FShowMenuButton:=AValue;
  Invalidate;
end;

procedure TCustomFluentNavigationView.ChangeInternalFont;
var
  fs, fh: integer;
  fss: single;
begin
  FInternalFont.Height:= Font.Height;
  fs:= Font.Size;
  if fs>=0 then
  begin
    if (fs=0) or (fs=11) then fss:=10.6 else fss:= fs;
    fh:= round(96*fss/72*Font.PixelsPerInch/FInternalFont.PixelsPerInch);
    FInternalFont.Height:= -fh;
  end;
  Invalidate;
end;

procedure TCustomFluentNavigationView.DoChangeInternalFont(Sender: TObject);
begin
  ChangeInternalFont;
end;


procedure TCustomFluentNavigationView.Loaded;
begin
  inherited Loaded;
  ChangeInternalFont;
end;

procedure TCustomFluentNavigationView.ScrollTo(Y: integer);
var
  Value: integer;
begin
  Value:= FCaptureTopHeight + round((Y - FScrollBarCaptureY)*FScrollHeight/FClientHeight);
  if Value<0 then
    Value:=0
  else
  if Value> (FScrollHeight - FClientHeight) then
    Value:= (FScrollHeight - FClientHeight);
  if Value = FScrollTopHeight then exit;
  FScrollTopHeight:= Value;
  Invalidate;
end;

procedure TCustomFluentNavigationView.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FScrollBarVisible then
  begin
    if FScrollBarCapture then
    begin
      ScrollTo(Y);
      exit;
    end;
    if FStrongScrollBar <> FMenuItemsRect.Contains(X,Y) then
    begin
      FStrongScrollBar:= not FStrongScrollBar;
      Invalidate;
    end;
  end;

  if (FPressedMenuItem<0) and (not FPressedNavButton) then
  begin
    if FNavButtonRect.Contains(X, Y) then
    begin
      SetHoverNavButton(true);
    end
    else
    begin
      SetHoverNavButton(false);
      if FScrollBarVisible and FBandRect.Contains(X, Y) then
        SetHoverItem(-1)
      else
        SetHoverItem(GetItemAtY(Y));
    end;
  end;
end;

procedure TCustomFluentNavigationView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if FScrollBarVisible then
  begin
    if FBandRect.Contains(TPointF.Create(X, Y)) then
    begin
      FScrollBarCapture:= true;
      FScrollBarCaptureY:= Y;
      FCaptureTopHeight:= FScrollTopHeight;
      exit;
    end;
  end;
  if (Button = mbLeft) and FHoverNavButton then
  begin
    FHoverNavButton:= false;
    FPressedNavButton:= true;
    Invalidate;
    exit;
  end;
  if (Button = mbLeft) and (FHoverMenuItem>=0) then
  begin

    FPressedMenuItem:= FHoverMenuItem;
    Invalidate;
  end;

end;

procedure TCustomFluentNavigationView.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  k: integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FScrollBarCapture then
  begin
    FScrollBarCapture:= false;
    exit;
  end;
  if FPressedNavButton then
  begin
    FPressedNavButton:= false;
    if FNavButtonRect.Contains(X, Y) then
    begin
      FHoverNavButton:= true;
      CompactMode:= not FCompactMode;
    end
    else
    begin
      FHoverNavButton:=false;
      Repaint;
    end;
    exit;
  end;
  k:= GetItemAtY(Y);
  if (FPressedMenuItem = k) and (k>=0) then DoSelectItem(k);
  FPressedMenuItem:= -1;
  FHoverMenuItem:= k;
  Repaint;
  //Invalidate;
end;

procedure TCustomFluentNavigationView.MouseLeave;
begin
  inherited MouseLeave;
  if FStrongScrollBar and FScrollBarVisible then
  begin
    FStrongScrollBar:= false;
    Invalidate;
  end;
  if (FHoverMenuItem>=0) or FHoverNavButton then
  begin
    FHoverMenuItem:= -1;
    FHoverNavButton:= false;
    Repaint;
  end;
end;

function TCustomFluentNavigationView.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  if FScrollHeight<=  (FClientHeight + FScrollTopHeight) then exit;
  FScrollTopHeight:=  min(FScrollHeight - FClientHeight,  FScrollTopHeight + FItemHeight);
  Invalidate;
  Result:=inherited DoMouseWheelDown(Shift, MousePos);
end;

function TCustomFluentNavigationView.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  if FScrollTopHeight <= 0 then exit;
  FScrollTopHeight:= max(0, FScrollTopHeight - FItemHeight);
  Invalidate;
  Result:=inherited DoMouseWheelUp(Shift, MousePos);
end;

constructor TCustomFluentNavigationView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalFont:= TFont.Create;
  Font.OnChange:= @DoChangeInternalFont;
  FShowMenuButton:= true;
  FCompactMode:= false;
  FNavigationViewItems:= TNavigationViewItems.Create(self);
  FNavButtonDrawer:= TPathIconDrawer.Create;
  FNavButtonDrawer.ParsePathData(IconGlobalNavButtonPath);
  FNavButtonState:= [];
  Align:= alLeft;
  Width:= DefNormalWidth;  //320 96 dpi
  ControlStyle := ControlStyle - [csSetCaption];
  FItemHeight:= DefItemHeight;  //40 96 dpi
  FHoverMenuItem:=-1;
  FPressedMenuItem:= -1;
  FSelectMenuItem:= -1;
  FScrollTopHeight:= 0;
  FScrollHeight:=0;
  FStrongScrollBar:= false;
end;

destructor TCustomFluentNavigationView.Destroy;
begin
  Font.OnChange:= nil;
  FInternalFont.Free;
  FNavigationViewItems.Free;
  FNavButtonDrawer.Free;
  inherited Destroy;
end;

function TCustomFluentNavigationView.GetPageIndex(const aPage: TPage): integer;
var
  i, n: integer;
begin
  Result:= -1;
  n:= FNavigationViewItems.Count;
  for i:=0 to n-1 do
    if FNavigationViewItems[i].Page = aPage then exit(i);
end;

function TCustomFluentNavigationView.GoToPage(const aPage: TPage): boolean;
var
  k: integer;
begin
  k:= GetPageIndex(aPage);
  if k>=0 then
    SetSelectMenuItem(k);
  Result:= k>=0;
end;

function TCustomFluentNavigationView.GoBack: boolean;
var
  k: integer;
  it: TNavigationViewItem = nil;
begin
  Result:= false;
  if FPageStack.IsEmpty then exit;
  try
    k:= FPageStack.Pop;
    Result:= true;
  except

  end;
  if Result then
  begin
    it:= MenuItems[k];
    FSelectMenuItem:= k;
    if Assigned(FOnSelectMenuItem) then FOnSelectMenuItem(self, it);
    Invalidate;
    it.Click;
    if Assigned(FOnChangeNavigationStatus) then FOnChangeNavigationStatus(self);
  end;
end;

end.

