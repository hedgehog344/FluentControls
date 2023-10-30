// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit ContentDialog;

//https://learn.microsoft.com/windows/apps/design/controls/dialogs-and-flyouts/dialogs

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, LCLType, LCLStrConsts, Graphics,
  SysUtils, Forms, ExtCtrls, StdCtrls,

  Codebot.System,
  Codebot.Graphics,
  Codebot.Graphics.Types,

  FluentCheckControls,
  FluentTextBox,
  FluentPasswordBox,
  FluentProgress,
  FluentButtons,
  FluentPathIcon,
  FluentIconView;

type

  TCustomContentDialog = class;

  { TBasicContentItem }

  TBasicContentItem = class(TComponent)
  private
    FDialog: TCustomContentDialog;
    FControl: TControl;

    FBorderSpacing: TControlBorderSpacing;
    FConstraints: TSizeConstraints;

    procedure SetBorderSpacing(AValue: TControlBorderSpacing);
    procedure SetConstraints(AValue: TSizeConstraints);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateControl(AContentPanel: TWinControl); virtual; abstract;
  published
    property BorderSpacing: TControlBorderSpacing read FBorderSpacing write SetBorderSpacing;
    property Constraints: TSizeConstraints read FConstraints write SetConstraints;
  end;


  TContentItemClass = class of TBasicContentItem;


  { TCheckContentItem }

  TCheckContentItem = class(TBasicContentItem)
  private
    FCaption: TCaption;
    FChecked: boolean;
    FEnablePrimaryButton: boolean;
    FOnChange: TNotifyEvent;
    function GetChecked: boolean;
    procedure InternalChange(Sender: TObject);
    procedure SetCaption(AValue: TCaption);
    procedure SetChecked(AValue: boolean);
  public
    procedure CreateControl(AContentPanel: TWinControl); override;
    property Checked: boolean read GetChecked write SetChecked;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property EnablePrimaryButton: boolean read FEnablePrimaryButton write FEnablePrimaryButton;
  end;

  { TTextBoxContentItem }

  TTextBoxContentItem = class(TBasicContentItem)
  private
    FPlaceholderText: TTranslateString;
    FText: TCaption;
    FTextBox: TFluentTextBox;
    FOnChange: TNotifyEvent;
    procedure InternalChange(Sender: TObject);
    procedure SetText(AValue: TCaption);
  public
    procedure CreateControl(AContentPanel: TWinControl); override;
  published
    property Text: TCaption read FText write SetText;
    property PlaceholderText: TTranslateString read FPlaceholderText
             write FPlaceholderText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TPasswordBoxContentItem }

  TPasswordBoxContentItem = class(TBasicContentItem)
  private
    FPassword: TCaption;
    FPasswordBox: TFluentPasswordBox;
    FOnChange: TNotifyEvent;
    FPlaceholderText: TTranslateString;
    procedure InternalChange(Sender: TObject);
    procedure SetPassword(AValue: TCaption);
  public
    procedure CreateControl(AContentPanel: TWinControl); override;
  published
    property Password: TCaption read FPassword write SetPassword;
    property PlaceholderText: TTranslateString read FPlaceholderText
             write FPlaceholderText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TProgressBarContentItem }

  TProgressBarContentItem = class(TBasicContentItem)
  private
    FValue: double;
    FMax: double;
    FMin: double;
    FProgressBar: TFluentProgressBar;
    FProgressStyle: TProgressStyle;
    function GetValue: double;
    procedure SetMax(AValue: double);
    procedure SetMin(AValue: double);
    procedure SetProgressStyle(AValue: TProgressStyle);
    procedure SetValue(AValue: double);
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateControl(AContentPanel: TWinControl); override;
  published
    property Value: double read GetValue write SetValue;
    property Min: double read FMin write SetMin;
    property Max: double read FMax write SetMax;
    property ProgressStyle: TProgressStyle read FProgressStyle
             write SetProgressStyle default psDeterminate;
  end;

  { TDialogContent }

  TDialogContent = class(TPersistent)
  private
    FList: TFPList;
    function GetItems(AIndex: integer): TBasicContentItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    procedure Add(const Item: TBasicContentItem);
    procedure Delete(Index: integer);
    procedure Clear;
    property Items[AIndex: integer]: TBasicContentItem read GetItems; default;
  end;

  { TDialogForm }

  TDialogForm = class(TForm)
  //protected
  //  procedure CreateParams(var Params: TCreateParams); override;
  end;

  TIconPosition = (ipNone, ipContent, ipTitle, ipAll);

  { TCustomContentDialog }

  TCustomContentDialog = class(TComponent)
  private
    // Buttons captions
    FPrimaryButtonText: TTranslateString;
    FSecondaryButtonText: TTranslateString;
    FCloseButtonText: TTranslateString;

    // Buttons
    FPrimaryButton, FSecondaryButton, FCloseButton: TFluentButton;

    // Icon
    FPathIcon: TPathIcon;

    FCloseFlag: boolean;

    FContent: TDialogContent;
    FIconPosition: TIconPosition;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;



    FButtonCount: integer;
    FDialogResult: TDialogResult;
    FDefaultButton: TDialogResult;
    FPrimaryButtonClick: TNotifyEvent;
    FPrimaryButtonEnabled: boolean;

    FSecondaryButtonClick: TNotifyEvent;

    FSubtitle: TTranslateString;
    FTitle: TTranslateString;
    FDialogForm: TForm;
    FParentForm: TForm;
    FBackForm: TForm;
    FBottomPanel: TPanel;
    FContentPanel: TPanel;

    FBottomPanelColor: TColorB;
    FBottomPanelLineColor: TColorB;
    FBorderColor: TColorB;
    FDefBorderColor: TColorB;

    FIconSize: integer;
    FLeftRightIndent: integer;
    FLeftIndentIcon: integer;
    FBottomIndent: integer;

    procedure BottomPanelPaint(Sender: TObject);
    function DefaultButtonAssigned: boolean;
    procedure DialogFormPaint(Sender: TObject);
    procedure DialogFormResize(Sender: TObject);
    procedure DoCenterFormDialog(w, h: integer);

    procedure DoCreateDialogTitle;
    procedure DoCreateSubtitle;
    procedure DoCreateDialogForm;
    procedure DoCreateBackForm;
    procedure DoCreateBottomPanel;
    procedure DoCreateContentPanel;
    procedure DoCreateIconView;

    procedure DoCreateNullButton;
    procedure DoClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure DoShowFormDialog;
    procedure DoShowModal;
    procedure DialogFormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ClickButton(Sender: TObject);
    procedure ParentFormResize(Sender: TObject);
    procedure SetDefaultButton(AValue: TDialogResult);
    procedure SetPathIcon(AValue: TPathIcon);
    procedure SetPrimaryButtonEnabled(AValue: boolean);
    function DoCreateButton(aCaption: String; aButtonType: TDialogResult
      ): TFluentButton;
  protected
    procedure DoPrimaryButtonClick;
    procedure DoSecondaryButtonClick;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddCheckBoxControl: TCheckContentItem;
    function AddTextBoxControl: TTextBoxContentItem;
    function AddPasswordBoxControl: TPasswordBoxContentItem;

    function Show: TDialogResult;
    procedure ShowAsync;

    property Content: TDialogContent read FContent;
    property Title: TTranslateString read FTitle write FTitle;
    property Subtitle: TTranslateString read FSubtitle write FSubtitle;
    property DialogResult: TDialogResult read FDialogResult;
    property DefaultButton: TDialogResult read FDefaultButton write SetDefaultButton default drNone;
    property PrimaryButtonText: TTranslateString read FPrimaryButtonText write FPrimaryButtonText;
    property SecondaryButtonText: TTranslateString read FSecondaryButtonText write FSecondaryButtonText;
    property CloseButtonText: TTranslateString read FCloseButtonText write FCloseButtonText;
    property PrimaryButtonEnabled: boolean read FPrimaryButtonEnabled write SetPrimaryButtonEnabled default true;
    property PrimaryButtonClick: TNotifyEvent read FPrimaryButtonClick write FPrimaryButtonClick;
    property SecondaryButtonClick: TNotifyEvent read FSecondaryButtonClick write FSecondaryButtonClick;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;

    property Icon: TPathIcon read FPathIcon write SetPathIcon;
    property IconPosition: TIconPosition read FIconPosition write FIconPosition default ipNone;
 //   property VerificationText
 //   AssociateToPrimaryButton
 //   Checked
 //   Binding

  end;

  { TContentDialog }

  TContentDialog = class(TCustomContentDialog)
  published
    property DefaultButton;
    property Subtitle;
    property Title;

    property PrimaryButtonText;
    property SecondaryButtonText;
    property CloseButtonText;
    property PrimaryButtonEnabled;
    property OnShow;
    property OnClose;
    property Icon;
    property IconPosition;
  end;

  procedure ShowSimpleMsg(const aForm: TCustomForm; aTitle, aMsg: string);

implementation

uses
  LCLIntf;

const
  Form_Round_Radius = 8;
  DefAlphaBlenValue = 77;
  DefIconSize = 48;

procedure ShowSimpleMsg(const aForm: TCustomForm; aTitle, aMsg: string);
begin
  with TContentDialog.Create(aForm) do
  try
    Title:= aTitle;
    Subtitle:= aMsg;
    DefaultButton:= drPrimary;
    Show;
  finally
    Free;
  end;
end;


{ TProgressBarContentItem }

function TProgressBarContentItem.GetValue: double;
begin
  if FProgressBar<> nil then Result:= FProgressBar.Value
  else Result:= FValue;
end;

procedure TProgressBarContentItem.SetMax(AValue: double);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;
end;

procedure TProgressBarContentItem.SetMin(AValue: double);
begin
  if FMin=AValue then Exit;
  FMin:=AValue;
end;

procedure TProgressBarContentItem.SetProgressStyle(AValue: TProgressStyle);
begin
  if FProgressBar<>nil then FProgressBar.ProgressStyle:= AValue;
  FProgressStyle:=AValue;
end;

procedure TProgressBarContentItem.SetValue(AValue: double);
begin
  if FProgressBar<>nil then FProgressBar.Value:= AValue;
  FValue:= AValue;
end;

constructor TProgressBarContentItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FProgressStyle:= psDeterminate;
  FMax:= 100;
  FMin:= 0;
  FValue:= 0;

end;

procedure TProgressBarContentItem.CreateControl(AContentPanel: TWinControl);
begin
  FProgressBar:= TFluentProgressBar.Create(AContentPanel.Parent);
  FControl:= FProgressBar;
  FProgressBar.Max:= FMax;
  FProgressBar.Min:= FMin;
  FProgressBar.Value:= FValue;
  FProgressBar.ProgressStyle:= FProgressStyle;
  with FProgressBar do
  begin
    Align:=alTop;
    BorderSpacing.Bottom:=6;
    Parent:= AContentPanel;
  end;
end;

{ TPasswordBoxContentItem }

procedure TPasswordBoxContentItem.InternalChange(Sender: TObject);
begin
  FPassword:= TFluentPasswordBox(FControl).Password;
  if Assigned(FOnChange) then FOnChange(Sender);
end;

procedure TPasswordBoxContentItem.SetPassword(AValue: TCaption);
begin
  if FPassword=AValue then Exit;
  FPassword:=AValue;
end;

procedure TPasswordBoxContentItem.CreateControl(AContentPanel: TWinControl);
begin
  FPasswordBox:= TFluentPasswordBox.Create(AContentPanel.Parent);
  FControl:= FPasswordBox;
  FPasswordBox.Password:= FPassword;
  FPasswordBox.PlaceholderText:= FPlaceholderText;
  FPasswordBox.OnChange:= @InternalChange;
  FPasswordBox.Align:=alTop;

  FPasswordBox.Constraints.MaxWidth:=220;
  FPasswordBox.BorderSpacing.Bottom:=12;
  FPasswordBox.TabStop:= false;
  FPasswordBox.Parent:= AContentPanel;
end;

{ TTextBoxContentItem }

procedure TTextBoxContentItem.InternalChange(Sender: TObject);
begin
  FText:= TFluentTextBox(FControl).Text;
  if Assigned(FOnChange) then FOnChange(Sender);
end;

procedure TTextBoxContentItem.SetText(AValue: TCaption);
begin
  if FText=AValue then Exit;
  FText:=AValue;
end;

procedure TTextBoxContentItem.CreateControl(AContentPanel: TWinControl);
begin
  FTextBox:= TFluentTextBox.Create(AContentPanel.Parent);
  FControl:= FTextBox;
  FTextBox.Text:= FText;
  FTextBox.PlaceholderText:= FPlaceholderText;
  FTextBox.OnChange:= @InternalChange;
  FTextBox.Align:=alTop;

  FTextBox.Constraints.MaxWidth:=220;
  FTextBox.BorderSpacing.Bottom:=12;
  FTextBox.TabStop:= false;
  FTextBox.Parent:= AContentPanel;
end;

{ TBasicContentDialogItem }

procedure TBasicContentItem.SetBorderSpacing(AValue: TControlBorderSpacing);
begin
  if FBorderSpacing=AValue then Exit;
  FBorderSpacing:=AValue;
end;

procedure TBasicContentItem.SetConstraints(AValue: TSizeConstraints);
begin
  if FConstraints=AValue then Exit;
  FConstraints:=AValue;
end;

procedure TBasicContentItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FControl) and (Operation = opRemove) then FControl:= nil;
  inherited Notification(AComponent, Operation);
end;

constructor TBasicContentItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //if AOwner is TCustomContentDialog then
  FDialog:= TCustomContentDialog(AOwner);
end;

destructor TBasicContentItem.Destroy;
begin
  inherited Destroy;
end;

{ TCheckContentDialogItem }

procedure TCheckContentItem.SetChecked(AValue: boolean);
begin
  FChecked:= AValue;
  if FControl<>nil then
    TFluentCheckBox(FControl).Checked:= AValue;
end;

procedure TCheckContentItem.InternalChange(Sender: TObject);
begin
  if FEnablePrimaryButton then
  begin
    FDialog.PrimaryButtonEnabled:= TFluentCheckBox(FControl).Checked;
  end;
  if Assigned(FOnChange) then FOnChange(FControl);
end;

function TCheckContentItem.GetChecked: boolean;
begin
  if FControl<>nil then Result:= TFluentCheckBox(FControl).Checked
  else Result:= FChecked;
end;

procedure TCheckContentItem.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TCheckContentItem.CreateControl(AContentPanel: TWinControl);
begin
  FControl:= TFluentCheckBox.Create(AContentPanel.Parent);
  with TFluentCheckBox(FControl) do
  begin
    Caption:= FCaption;
    Checked:= FChecked;
    OnChange:= @InternalChange;
    Align:=alTop;
    BorderSpacing.Bottom:=6;
    Parent:= AContentPanel;
  end;

end;

{ TDialogContent }

function TDialogContent.GetItems(AIndex: integer): TBasicContentItem;
begin
  Result:= TBasicContentItem(FList[AIndex]);
end;

constructor TDialogContent.Create;
begin
  FList:= TFPList.Create;
end;

destructor TDialogContent.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TDialogContent.Count: integer;
begin
  Result:= FList.Count;
end;

procedure TDialogContent.Add(const Item: TBasicContentItem);
begin
  FList.Add(Item);
end;

procedure TDialogContent.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

procedure TDialogContent.Clear;
begin
  FList.Clear;
end;


{ TCustomContentDialog }

procedure TCustomContentDialog.DoCreateDialogTitle;
begin
  with TLabel.Create(FDialogForm) do
  begin
    Font.Color:=$1A1A1A;
    Align:= alTop;
    Top := 0;
    BorderSpacing.Top:= 4*FBottomIndent;
    BorderSpacing.Bottom:= 2*FBottomIndent;
    BorderSpacing.Right:= FLeftRightIndent;
    if (FIconPosition = ipTitle) or (FIconPosition = ipAll) then
    begin
      BorderSpacing.Left:= FLeftIndentIcon;
      if (FIconPosition = ipTitle) then
        Constraints.MinHeight:= FIconSize;
    end
    else BorderSpacing.Left:= FLeftRightIndent;
    {$IFDEF WINDOWS}
    if Screen.Fonts.IndexOf('Segoe UI Semibold')>=0 then
      Font.Name := 'Segoe UI Semibold';
    {$ENDIF}
    Font.Size:= 15;
    if FTitle = '' then Caption:= ' ' else Caption:= WrapText(FTitle, 50);
    Parent := FDialogForm;
  end;
end;

procedure TCustomContentDialog.DoCreateSubtitle;
begin
  if FSubtitle = '' then exit;
  with TLabel.Create(FDialogForm) do
  begin
    Font.Color:=$1A1A1A;
    Align:= alTop;
    Top := 1;
    AutoSize:= true;
    BorderSpacing.Top:= FBottomIndent;
    BorderSpacing.Bottom:= FBottomIndent;
    if FIconPosition = ipAll then BorderSpacing.Left:= FLeftIndentIcon
    else BorderSpacing.Left:= FLeftRightIndent;
    BorderSpacing.Right:= FLeftRightIndent;
    Caption:= WrapText(FSubtitle, 75);
    Font.Height:= -round(Screen.PixelsPerInch*10.5/72);
    Parent := FDialogForm;
  end;
end;

procedure TCustomContentDialog.DoClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FOnClose = nil then exit;
  FOnClose(Sender);
end;

procedure TCustomContentDialog.DoCreateDialogForm;
begin
  FBottomPanelColor:= TColor($F3F3F3);
  FBottomPanelLineColor:= Darken(FBottomPanelColor, 15/255);
  FDefBorderColor:= TColor($959595);
  FBorderColor:= FBottomPanelLineColor;

  FDialogForm:= TDialogForm.CreateNew(self);
  FDialogForm.PixelsPerInch:=Screen.PixelsPerInch;
  FDialogForm.Font.Size:=0;

  FIconSize:= FDialogForm.Scale96ToForm(DefIconSize);
  FLeftRightIndent:= FDialogForm.Scale96ToForm(24);
  FBottomIndent:=  FLeftRightIndent div 4;
  FLeftIndentIcon:= FIconSize + 3*(FLeftRightIndent div 2);
  //FDialogForm.HandleNeeded;
  FDialogForm.DisableAlign;
  DoCreateDialogTitle;
  DoCreateSubtitle;
  DoCreateBottomPanel;
  DoCreateContentPanel;
  DoCreateIconView;

  with FDialogForm do
  begin
    BorderStyle := bsNone;
    Color:= TColor($FDFDFD); //clWindow;
    ShowInTaskBar:=stNever;
    KeyPreview := True;

    Constraints.MaxWidth:= Constraints.MinWidth * 2;
    OnKeyDown:= @DialogFormKeyDown;
    OnPaint:= @DialogFormPaint;
    AutoSize:=true;
    OnShow:= FOnShow;
    OnClose:= @DoClose;
    {$IFDEF WINDOWS}
    if Screen.Fonts.IndexOf('Segoe UI')>=0 then
      Font.Name := 'Segoe UI';
    {$ENDIF}
    Parent:=FParentForm;
    Constraints.MinWidth:= Scale96ToForm(300);
  end;
  FDialogForm.EnableAlign;
end;

procedure TCustomContentDialog.DoCreateBackForm;
begin
  if FParentForm=nil then exit;
  FBackForm:=TForm.CreateNew(self);
  with FBackForm do
  begin
    Anchors:=[akTop, akLeft, akRight, akBottom];
    ShowInTaskBar:=stNever;
    BorderStyle := bsNone;
    AlphaBlend:=True;
    AlphaBlendValue:=2;
    Color:=clBlack;
    SetBounds(0,0, FParentForm.Width, FParentForm.Height);
    Parent:=FParentForm;
  end;
end;



procedure TCustomContentDialog.DoCreateBottomPanel;
var
  primaryTxt: string;
  btnCnt: integer;

procedure CalcButtonCount;
begin
  btnCnt:=0;
  if FPrimaryButtonText<>'' then btnCnt+=1;
  if FSecondaryButtonText<>'' then btnCnt+=1;
  if FCloseButtonText<>'' then btnCnt+=1;
end;


begin
  FPrimaryButton:= nil;
  FSecondaryButton:= nil;
  FCloseButton:=nil;

  FBottomPanel:= TPanel.Create(FDialogForm);
  FBottomPanel.Parent:= FDialogForm;
  CalcButtonCount();

  if btnCnt = 0 then
  begin
    primaryTxt:= rsMbOK;
    if primaryTxt.BeginsWith('&') then Delete(primaryTxt, 1,1);
    btnCnt:= 1;
  end
  else primaryTxt:= FPrimaryButtonText;


  FButtonCount:= btnCnt;
  if btnCnt<2 then
  begin
    btnCnt:= 2;
    DoCreateNullButton;
  end;

  if primaryTxt<>'' then
  begin
    FPrimaryButton:= DoCreateButton(primaryTxt, drPrimary);
    FPrimaryButton.Enabled:= FPrimaryButtonEnabled;
  end;
  if FSecondaryButtonText<>'' then
    FSecondaryButton:=DoCreateButton(FSecondaryButtonText, drSecondary);
  if FCloseButtonText<>'' then
    FCloseButton:=DoCreateButton(FCloseButtonText, drClose);


  with FBottomPanel do
  begin
    Align:= alBottom;
    AutoSize:= true;
    Color:= FBottomPanelColor.Color;
    BevelOuter:= bvNone;
    with ChildSizing do
    begin
      ControlsPerLine:= btnCnt;
      EnlargeHorizontal:= crsHomogenousChildResize;
      ShrinkHorizontal:= crsScaleChilds;
      HorizontalSpacing:=Scale96ToForm(8);
      TopBottomSpacing:=Scale96ToForm(24);
      LeftRightSpacing:=TopBottomSpacing;
      Layout:= cclLeftToRightThenTopToBottom;
    end;
    OnPaint:= @BottomPanelPaint;

  end;
end;

procedure TCustomContentDialog.DoCreateContentPanel;
var
  i: integer;
  it: TBasicContentItem;
begin
  FContentPanel:= TPanel.Create(FDialogForm);
  with FContentPanel do
  begin
    Align:= alClient;
    AutoSize:= true;
    BevelOuter:= bvNone;
    if (FIconPosition = ipContent) or (FIconPosition = ipAll) then
    begin
      BorderSpacing.Left:= FLeftIndentIcon;
      if (FIconPosition = ipContent) then
        Constraints.MinHeight:= FIconSize;
    end
    else
    begin
      BorderSpacing.Left:= FLeftRightIndent;
      Constraints.MinHeight:= FBottomIndent;
    end;
    BorderSpacing.Right:= FLeftRightIndent;
    BorderSpacing.Bottom:= FBottomIndent;
    Parent:= FDialogForm;
  end;

  for i:=0 to FContent.Count-1 do
  begin
    it:= FContent[i];
    it.CreateControl(FContentPanel);
    it.FControl.Top:= i;
  end;
end;

procedure TCustomContentDialog.DoCreateIconView;
begin
  if FIconPosition = ipNone then exit;
  with TFluentIconView.Create(FDialogForm) do
  begin
    PathIcon:=FPathIcon;
    SetBounds(0, 0, FIconSize, FIconSize);
    BorderSpacing.Left := FLeftRightIndent;
    AnchorSide[akTop].Side := asrTop;
    if FIconPosition = ipContent then
    begin
      AnchorSide[akTop].Control := FContentPanel;
    end
    else
    begin
      AnchorSide[akTop].Control := FDialogForm;
      BorderSpacing.Top := FLeftRightIndent;
    end;
    Parent:= FDialogForm;
  end;
end;

function TCustomContentDialog.DoCreateButton(aCaption: String;
  aButtonType: TDialogResult): TFluentButton;
begin
  Result := TFluentButton.Create(FDialogForm);
  with Result do
  begin
    Caption:= aCaption;
    OnClick:= @ClickButton;
    DialogResult:= aButtonType;
    if aButtonType = FDefaultButton then
      ButtonStyle:= fbsAccent;
    AutoSize:= true;
    Parent := FBottomPanel;
    Constraints.MinWidth:= Scale96ToForm(80);


  end;
end;

procedure TCustomContentDialog.DoCreateNullButton;
begin
  with TPanel.Create(FDialogForm) do
  begin
    Caption:= '';
    BevelOuter:= bvNone;
    AutoSize:= true;
    Parent := FBottomPanel;
    Constraints.MinWidth:= Scale96ToForm(80);
  end;
end;

procedure TCustomContentDialog.BottomPanelPaint(Sender: TObject);
var
  S: ISurface;
  R: TRectF;
  w: Float;
  rf: TRectF;
begin
  S := NewSurface(FBottomPanel.Canvas);
  if S <> nil then
  begin
    R:= FBottomPanel.ClientRect;
    S.MoveTo(0, 0.5);
    S.LineTo(R.Width, 0.5);
    S.Stroke(NewPen(FBottomPanelLineColor));
    w:= ScaleX(2*Form_Round_Radius, 96);
    rf:= TRectF.Create(w,w);
    S.MoveTo(0.5, 0);
    rf.X:=0.5;
    rf.Y:= R.Bottom - w - 1.5;
    S.ArcTo(rf, -pi/2, -pi);
    rf.X:=R.Right - w - 1.5;
    S.ArcTo(rf, pi, pi/2);
    S.LineTo(R.Width-1.5, 0);
    S.Stroke(NewPen(FBorderColor));
    S.Flush;
  end;
  inherited;
end;

procedure TCustomContentDialog.DialogFormPaint(Sender: TObject);
var
  S: ISurface;
  R: TRectF;
  w: Float;
  rf: TRectF;
begin
  S := NewSurface(FDialogForm.Canvas);
  if S <> nil then
  begin
    // draw border
    R:= FDialogForm.ClientRect;
    w:= ScaleX(2*Form_Round_Radius, 96);
    rf:= TRectF.Create(w,w);
    S.MoveTo(0.5, FBottomPanel.Top);
    rf.X:=0.5;
    rf.Y:= 0.5;
    S.ArcTo(rf, -pi/2, 0);
    rf.X:=R.Right - w - 1.5;
    S.ArcTo(rf, 0, pi/2);
    S.LineTo(R.Width-1.5, FBottomPanel.Top);
    S.Stroke(NewPen(FBorderColor));
    S.Flush;
  end;
  inherited;
end;

procedure DoRoundCorners(AControl: TWinControl; w, h: integer);
var
  rgn: HRGN;
  d: integer;
begin
  d:= 2*ScaleX(Form_Round_Radius, 96);
  rgn := CreateRoundRectRgn(0, 0, w, h, d, d);
  SetWindowRgn(AControl.Handle, rgn, true);
  DeleteObject(rgn);
end;

procedure TCustomContentDialog.DialogFormResize(Sender: TObject);
var
  w, h: integer;
begin
  w:= FDialogForm.Width;
  h:= FDialogForm.Height;
  if FParentForm<> nil then DoCenterFormDialog(w, h);
  DoRoundCorners(FDialogForm, w, h);
end;

procedure TCustomContentDialog.DoCenterFormDialog(w, h: integer);
begin
  FDialogForm.SetBounds((FParentForm.Width - w) div 2, (FParentForm.Height - h) div 2, w, h);
end;

procedure TCustomContentDialog.ParentFormResize(Sender: TObject);
begin
  if Assigned(FDialogForm) and FDialogForm.Showing then
    DoCenterFormDialog(FDialogForm.Width, FDialogForm.Height);
end;

procedure TCustomContentDialog.DoShowFormDialog;
var
  w, h: integer;
  a: byte;
begin

  if FDialogForm<> nil then exit;
  DoCreateBackForm;
  DoCreateDialogForm;

  try
    FCloseFlag:= false;
    FBackForm.Show;
    if FParentForm<> nil then
      FBackForm.SetBounds(0,0, FParentForm.Width, FParentForm.Height);
    Application.ProcessMessages;
    FDialogForm.HandleNeeded;
    w:= FDialogForm.Width;
    h:= FDialogForm.Height;
    FDialogForm.GetPreferredSize(w, h);
    DoRoundCorners(FDialogForm, w, h);
    DoCenterFormDialog(w, h);

    FDialogForm.Show;
    FDialogForm.SetFocus;

    if FParentForm<> nil then
          FParentForm.AddHandlerOnResize(@ParentFormResize);
    FDialogForm.OnResize:= @DialogFormResize;

    a:=2;
    while a < DefAlphaBlenValue do
    begin
      a:=a+10;
      FBorderColor:=FBottomPanelLineColor.Blend(FDefBorderColor, a/DefAlphaBlenValue);
      FBackForm.AlphaBlendValue:= a;
      FDialogForm.Invalidate;
      Application.ProcessMessages;
    end;

    while true do
    begin
      try
        Application.ProcessMessages;
      except
        if Application.CaptureExceptions then Application.HandleException(Self)
        else raise;
      end;
      Application.Idle(false);
      if FCloseFlag or Application.Terminated then break;
    end;

    if FParentForm<> nil then
      FParentForm.RemoveHandlerOnResize(@ParentFormResize);
    FBackForm.Close;
  finally
    FreeAndNil(FDialogForm);
    FreeAndNil(FBackForm);
  end;
end;

procedure TCustomContentDialog.DoShowModal;
begin
  if FDialogForm<> nil then exit;
  DoCreateDialogForm;
  FDialogForm.Position:= poScreenCenter;
  try
    FDialogForm.ShowModal;
  finally
    FDialogForm.Free;
  end;
end;

function TCustomContentDialog.DefaultButtonAssigned: boolean;
begin
  Result:= false;
  case FDefaultButton of
    drPrimary:   Result:= Assigned(FPrimaryButton) and FPrimaryButton.Enabled;
    drSecondary: Result:= Assigned(FSecondaryButton);
    drClose:     Result:= Assigned(FCloseButton);
  end;
end;

procedure TCustomContentDialog.DialogFormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_TAB: Key:= 0;
    VK_ESCAPE: FCloseFlag:= true;
    VK_RETURN: if DefaultButtonAssigned then
    begin
      FDialogResult:= FDefaultButton;
      FCloseFlag:= true;
    end;
  end;
  if FCloseFlag then FDialogForm.Close;
end;

procedure TCustomContentDialog.ClickButton(Sender: TObject);
begin
  FDialogResult:= TFluentButton(Sender).DialogResult;
  case FDialogResult of
    drPrimary:  DoPrimaryButtonClick;
    drSecondary: DoSecondaryButtonClick;
  end;
  FCloseFlag:= FDialogResult<>drNone;
  if FCloseFlag then FDialogForm.Close;
end;

procedure TCustomContentDialog.SetDefaultButton(AValue: TDialogResult);
begin
  if FDefaultButton=AValue then Exit;
  FDefaultButton:=AValue;
end;

procedure TCustomContentDialog.SetPathIcon(AValue: TPathIcon);
begin
  FPathIcon.Assign(AValue);
end;

procedure TCustomContentDialog.SetPrimaryButtonEnabled(AValue: boolean);
begin
  if FPrimaryButtonEnabled=AValue then Exit;
  FPrimaryButtonEnabled:=AValue;
  if Assigned(FDialogForm) then
    FPrimaryButton.Enabled:= AValue;
end;

procedure TCustomContentDialog.DoPrimaryButtonClick;
begin
  if Assigned(FPrimaryButtonClick) then
    FPrimaryButtonClick(self);
end;

procedure TCustomContentDialog.DoSecondaryButtonClick;
begin
  if Assigned(FSecondaryButtonClick) then
    FSecondaryButtonClick(self);
end;

constructor TCustomContentDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContent:= TDialogContent.Create;
  FPathIcon:= TPathIcon.Create;

  if Assigned(AOwner) and (AOwner is TForm) then
    FParentForm:=  TForm(AOwner);

  FPrimaryButtonEnabled:= true;
end;

destructor TCustomContentDialog.Destroy;
begin
  FPathIcon.Free;
  FContent.Free;
  inherited Destroy;
end;

function TCustomContentDialog.AddCheckBoxControl: TCheckContentItem;
begin
  Result:= TCheckContentItem.Create(self);
  FContent.Add(Result);
end;

function TCustomContentDialog.Show: TDialogResult;
begin
  FDialogResult:= drNone;
  if Assigned(FParentForm) then
  begin
    DoShowFormDialog;
  end
  else
  begin
    DoShowModal;
  end;
  Result:= FDialogResult;
end;

procedure TCustomContentDialog.ShowAsync;
begin
  FDialogResult:= drNone;
  //
end;


function TCustomContentDialog.AddTextBoxControl: TTextBoxContentItem;
begin
  Result:= TTextBoxContentItem.Create(self);
  FContent.Add(Result);
end;

function TCustomContentDialog.AddPasswordBoxControl: TPasswordBoxContentItem;
begin
  Result:= TPasswordBoxContentItem.Create(self);
  FContent.Add(Result);
end;

end.

