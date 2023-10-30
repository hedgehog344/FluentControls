unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Graphics,
  ExtCtrls, StdCtrls, LCLIntf,

  FluentAnimation,
  FluentNavigation,
  FluentIconView,
  FluentSlider,
  FluentButtons,
  FluentTextBox,
  FluentIconButton,
  ContentDialog,
  FluentPasswordBox,
  FluentCheckControls,
  FluentProgress,
  FluentTextBlock;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnStandard: TFluentButton;
    cbAccentStyle1: TFluentCheckBox;
    cbButtonDisabled1: TFluentCheckBox;
    cbReversed: TFluentCheckBox;
    cbShowTicks: TFluentCheckBox;
    cbShowTip: TFluentCheckBox;
    ContentDialog1: TContentDialog;
    Error1: TFluentRadioButton;
    fbStartAnimation2: TFluentButton;
    fbStartAnimation3: TFluentButton;
    fbStartAnimation4: TFluentButton;
    FlowPanelIcons: TFlowPanel;
    btnShowDialog1: TFluentButton;
    btnShowDialog2: TFluentButton;
    btnShowDialog3: TFluentButton;
    fbStartAnimation1: TFluentButton;
    FluentCheckBox: TFluentCheckBox;
    FluentCheckBox1: TFluentCheckBox;
    FluentCheckBox2: TFluentCheckBox;
    cbEnableAccentColor: TFluentCheckBox;
    FluentHyperlinkButton2: TFluentHyperlinkButton;
    FluentHyperlinkButton3: TFluentHyperlinkButton;
    FluentHyperlinkCodebot: TFluentHyperlinkButton;
    BackBtn: TFluentIconButton;
    FluentControlsHyperlink: TFluentHyperlinkButton;
    FluentIconButton1: TFluentIconButton;
    FluentIconButton2: TFluentIconButton;
    FluentIconButton3: TFluentIconButton;
    FluentIconButton4: TFluentIconButton;
    FluentIconButton5: TFluentIconButton;
    FluentIconView1: TFluentIconView;
    FluentNavigationView1: TFluentNavigationView;
    FluentNavigationView2: TFluentNavigationView;
    FluentPasswordBox1: TFluentPasswordBox;
    FluentRadioButton1: TFluentRadioButton;
    FluentRadioButton2: TFluentRadioButton;
    txtHomeSubtitle: TFluentTextBlock;
    txtHomeTitle: TFluentTextBlock;
    txtIconsSubtitle: TFluentTextBlock;
    lblProgressRingValue: TFluentTextBlock;
    Label10: TLabel;
    Label17: TLabel;
    LabelHeadNav: TLabel;
    lblButton1: TLabel;
    lblButton2: TLabel;
    lblRadioButtonResult: TLabel;
    lblToggleButton1: TLabel;
    lblToggleButton2: TLabel;
    lblToggleResult1: TLabel;
    lblToggleResult2: TLabel;
    PageButtons: TPage;
    panButton1: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    panRadioButton: TPanel;
    panToggleButton1: TPanel;
    LabelDialog5: TLabel;
    lblCheckBox: TLabel;
    lblResult3: TLabel;
    lblResult4: TLabel;
    lblSlider: TLabel;
    Panel10: TPanel;
    panCheckBox: TPanel;
    panSlider: TPanel;
    PanelIconsBottom: TPanel;
    panToggleButton2: TPanel;
    sbBasic: TScrollBox;
    ProgressBar1: TFluentProgressBar;
    FluentSlider2: TFluentSlider;
    sbNavigation: TScrollBox;
    slBarValue: TFluentSlider;
    frbDeterminate1: TFluentRadioButton;
    frbIndeterminate: TFluentRadioButton;
    frbIndeterminate1: TFluentRadioButton;
    frbPaused1: TFluentRadioButton;
    lblProgressBar: TLabel;
    lblProgressBarValue: TLabel;
    panProgressBar: TPanel;
    ProgressRing1: TFluentProgressRing;
    frbDeterminate: TFluentRadioButton;
    frbPaused: TFluentRadioButton;
    Error: TFluentRadioButton;
    FluentTextBox1: TFluentTextBox;
    lblContentDialog: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    LabelDialog1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelDialog2: TLabel;
    LabelDialog3: TLabel;
    LabelDialog4: TLabel;
    lblProgressRing: TLabel;
    lblResult1: TLabel;
    lblResult2: TLabel;
    Notebook1: TNotebook;
    PageDialog: TPage;
    PageHome: TPage;
    PageIcons: TPage;
    PageBasic: TPage;
    PageNavigation: TPage;
    PageSettings: TPage;
    PageText: TPage;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    panProgressRing: TPanel;
    ScrollBox1: TScrollBox;
    PageStatus: TPage;
    Slider1: TFluentSlider;
    ToggleButton1: TFluentToggleButton;
    txtIconsTitle: TFluentTextBlock;
    procedure BackBtnClick(Sender: TObject);
    procedure btnShowDialog3Click(Sender: TObject);
    procedure cbAccentStyleChange(Sender: TObject);
    procedure cbButtonDisabledChange(Sender: TObject);
    procedure cbEnableAccentColorChange(Sender: TObject);
    procedure cbReversedChange(Sender: TObject);
    procedure cbShowTicksChange(Sender: TObject);
    procedure cbShowTipChange(Sender: TObject);
    procedure btnShowDialog1Click(Sender: TObject);
    procedure btnShowDialog2Click(Sender: TObject);
    procedure fbStartAnimation1Click(Sender: TObject);
    procedure fbStartAnimation2Click(Sender: TObject);
    procedure fbStartAnimation3Click(Sender: TObject);
    procedure fbStartAnimation4Click(Sender: TObject);
    procedure FluentCheckBox1Change(Sender: TObject);
    procedure FluentCheckBox2Change(Sender: TObject);
    procedure FluentCheckBoxChange(Sender: TObject);
    procedure FluentControlsHyperlinkClick(Sender: TObject);
    procedure FluentHyperlinkButton1Click(Sender: TObject);
    procedure FluentHyperlinkButton2Click(Sender: TObject);
    procedure FluentHyperlinkCodebotClick(Sender: TObject);
    procedure FluentIconView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FluentIconView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FluentNavigationView1HomeClick(Sender: TObject);
    procedure FluentNavigationView1SelectMenuItem(Sender: TObject;
      Item: TNavigationViewItem);
    procedure FluentNavigationView2ChangeNavigationStatus(Sender: TObject);
    procedure FluentNavigationView2SelectMenuItem(Sender: TObject;
      Item: TNavigationViewItem);
    procedure FluentRadioButton1Change(Sender: TObject);
    procedure FluentRadioButton2Change(Sender: TObject);
    procedure FluentSlider2ValueChanged(Sender: TObject);
    procedure ToggleButtonChange(Sender: TObject);
    procedure PageIconsBeforeShow({%H-}ASender: TObject; {%H-}ANewPage: TPage;
      {%H-}ANewIndex: Integer);
    procedure ProgressBarChangeState(Sender: TObject);
    procedure ProgressRingChangeState(Sender: TObject);
    procedure slBarValueValueChanged(Sender: TObject);
  private
    procedure CreatePageIcons;
    procedure OnAnimateAngle(AnimationValue: double);
    procedure OnAnimateAngleScale(AnimationValue: double);
    procedure OnAnimateScale(AnimationValue: double);

  public

  end;

var
  Form1: TForm1;

implementation

uses
  System.UITypes,
  FluentIconList;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnShowDialog1Click(Sender: TObject);
begin
  case ContentDialog1.Show of
    drNone:       lblResult1.Caption:= '';
    drPrimary:    lblResult1.Caption:= 'User saved their work';
    drSecondary:  lblResult1.Caption:= 'User did not save their work';
    drClose:      lblResult1.Caption:= 'User cancelled the dialog';
  end;
end;

procedure TForm1.cbShowTicksChange(Sender: TObject);
begin
  Slider1.ShowTicks:= cbShowTicks.Checked;
end;

procedure TForm1.cbReversedChange(Sender: TObject);
begin
  Slider1.Reversed:= cbReversed.Checked;
end;

procedure TForm1.cbAccentStyleChange(Sender: TObject);
begin
  if cbAccentStyle1.Checked then btnStandard.ButtonStyle:= fbsAccent
  else btnStandard.ButtonStyle:= fbsStandard;
end;

procedure TForm1.btnShowDialog3Click(Sender: TObject);
begin
  ShowSimpleMsg(self, 'Sorry', 'It''s not implemented yet!');
end;

procedure TForm1.BackBtnClick(Sender: TObject);
begin
  FluentNavigationView2.GoBack;
end;

procedure TForm1.cbButtonDisabledChange(Sender: TObject);
begin
  btnStandard.Enabled:= not cbButtonDisabled1.Checked;
end;

procedure TForm1.cbEnableAccentColorChange(Sender: TObject);
var
  i, n: integer;
  c: TControl;
begin
  n:= FlowPanelIcons.ControlList.Count;
  for i:= 0 to n-1 do
  begin
    c:=FlowPanelIcons.ControlList[i].Control;
    if c is TFluentIconButton then  with TFluentIconButton(c) do
    begin
      if cbEnableAccentColor.Checked then
        PathIcon.AccentColor:= clHighlight
      else
        PathIcon.AccentColor:= PathIcon.ForegroundColor;
    end;
  end;
end;

procedure TForm1.cbShowTipChange(Sender: TObject);
begin
  Slider1.ShowTip:= cbShowTip.Checked;
end;

procedure TForm1.btnShowDialog2Click(Sender: TObject);
var
  cd: TContentDialog;
  tb: TTextBoxContentItem;
  pb: TPasswordBoxContentItem;
  cb: TCheckContentItem;
  s: string;
begin
  cd:= TContentDialog.Create(self);
  cd.Title:= 'Sign Up';
  cd.PrimaryButtonText:='Sign Up';
  cd.CloseButtonText:='Cancel';
  cd.Icon.CodePoint:=$E77B;
  cd.IconPosition:=ipContent;
  cd.DefaultButton:=drPrimary;
  cd.PrimaryButtonEnabled:= false;

  tb:= TTextBoxContentItem.Create(cd);
  tb.PlaceholderText:= 'Enter your name';
  cd.Content.Add(tb);

  pb:= TPasswordBoxContentItem.Create(cd);
  pb.PlaceholderText:= 'Enter your password';
  cd.Content.Add(pb);

  cb:= TCheckContentItem.Create(cd);
  cb.Caption:='Accept Terms and Conditions';
  cb.EnablePrimaryButton:= true;
  cd.Content.Add(cb);

  try
    case cd.Show of
      drNone:       s:= '';
      drPrimary:    s:= 'Email: "' + tb.Text+ '"; Password: "'+ pb.Password+'"';
      drClose:      s:= 'User cancelled the dialog';
    end;
    lblResult2.Caption:= s;
  finally
    cd.Free;
  end;
end;

procedure TForm1.OnAnimateScale(AnimationValue: double);
begin
  FluentIconView1.PathIcon.Scale := AnimationValue;
end;

procedure TForm1.OnAnimateAngle(AnimationValue: double);
begin
  FluentIconView1.PathIcon.Angle := round(AnimationValue);
end;

procedure TForm1.OnAnimateAngleScale(AnimationValue: double);
begin
  FluentIconView1.PathIcon.BeginUpdate;
  FluentIconView1.PathIcon.Angle := round(AnimationValue*360);
  FluentIconView1.PathIcon.Scale := AnimationValue;
  FluentIconView1.PathIcon.EndUpdate;
end;

procedure TForm1.fbStartAnimation1Click(Sender: TObject);
begin
  FluentIconView1.Animate(@TEasingFunctions.EaseOutBounce, 2000, 1, 0.2, @OnAnimateScale, false);
end;

procedure TForm1.fbStartAnimation2Click(Sender: TObject);
begin
  FluentIconView1.Animate(@TEasingFunctions.EaseInOutBack, 2000, 1, 0.2, @OnAnimateScale, false);
end;

procedure TForm1.fbStartAnimation3Click(Sender: TObject);
begin
  FluentIconView1.PathIcon.Scale:=1.0;
  FluentIconView1.Animate(@TEasingFunctions.easeOutBack, 2000, 0, 1080, @OnAnimateAngle, false);
end;

procedure TForm1.fbStartAnimation4Click(Sender: TObject);
begin
  FluentIconView1.Animate(@TEasingFunctions.easeOutBack, 2000, 0.1, 1, @OnAnimateAngleScale, false);
end;

procedure TForm1.FluentNavigationView1HomeClick(Sender: TObject);
begin
  PageHome.Show;
end;

procedure TForm1.FluentCheckBox1Change(Sender: TObject);
begin
  FluentPasswordBox1.HidePassword:= not FluentCheckBox1.Checked;
end;

procedure TForm1.FluentCheckBox2Change(Sender: TObject);
begin
  if FluentCheckBox2.Checked then
    ProgressRing1.BackgroundColor:=TColorRec.LightGray
  else
    ProgressRing1.BackgroundColor:= clNone;
end;

procedure TForm1.FluentCheckBoxChange(Sender: TObject);
begin
  if FluentCheckBox.Checked then lblResult4.Caption:='You checked the box'
  else lblResult4.Caption:='You unchecked the box'
end;

procedure TForm1.FluentControlsHyperlinkClick(Sender: TObject);
begin
  OpenURL('https://github.com/hedgehog344/FluentControls');
end;

procedure TForm1.FluentHyperlinkButton1Click(Sender: TObject);
begin
  FluentNavigationView1.GoToPage(PageStatus);
end;

procedure TForm1.FluentHyperlinkButton2Click(Sender: TObject);
begin
  OpenURL('https://www.google.com/');
end;

procedure TForm1.FluentHyperlinkCodebotClick(Sender: TObject);
begin
  OpenURL('https://github.com/sysrpl/Codebot.Cross');
end;

procedure TForm1.FluentIconView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FluentIconView1.Tag = 0 then
  begin
    FluentIconView1.Animate(
    @TEasingFunctions.easeOutBack,
    200,
    1,
    0.8,
    @OnAnimateScale, false);
  end
  else
  begin
    FluentIconView1.Animate(
    @TEasingFunctions.EaseOutQuad,
    100,
    0,
    -30,
    @OnAnimateAngle, false);
  end;
end;

procedure TForm1.FluentIconView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FluentIconView1.Tag = 0 then
  begin
    FluentIconView1.Tag := 1;
    FluentIconView1.Animate(
    @TEasingFunctions.easeOutBack,
    300,
    FluentIconView1.PathIcon.Scale,
    1,
    @OnAnimateScale, false);
  end
  else
  begin
    FluentIconView1.Tag := 0;
    FluentIconView1.Animate(
    @TEasingFunctions.EaseOutQuad,
    900,
    FluentIconView1.PathIcon.Angle,
    360,
    @OnAnimateAngle, false);
  end;
end;

procedure TForm1.FluentNavigationView1SelectMenuItem(Sender: TObject;
  Item: TNavigationViewItem);
begin
  Caption:=Item.Caption;
end;

procedure TForm1.FluentNavigationView2ChangeNavigationStatus(Sender: TObject);
begin
  BackBtn.Enabled:= FluentNavigationView2.PageStackCount>0;
end;

procedure TForm1.FluentNavigationView2SelectMenuItem(Sender: TObject;
  Item: TNavigationViewItem);
begin
  LabelHeadNav.Caption:= Item.Caption;
end;

procedure TForm1.FluentRadioButton1Change(Sender: TObject);
begin
  if FluentRadioButton1.Checked then lblRadioButtonResult.Caption:= 'RadioButton1 checked';
end;

procedure TForm1.FluentRadioButton2Change(Sender: TObject);
begin
  if FluentRadioButton2.Checked then lblRadioButtonResult.Caption:= 'RadioButton2 checked';
end;

procedure TForm1.FluentSlider2ValueChanged(Sender: TObject);
begin
  ProgressRing1.Value:= FluentSlider2.Value;
  lblProgressRingValue.Caption:= 'Value: '+IntToStr(round(FluentSlider2.Value));
end;

procedure TForm1.CreatePageIcons;
var
  i,n : integer;
begin
  n:=0;
  i:= $e700;
  while i<$ffff do
  begin
    if GetIconPathByIndex(i)<>'' then
    begin
      with TFluentIconButton.Create(self) do
      begin
        PathIcon.CodePoint:=i;
        Hint:= GetIconNameByIndex(i)+' ($'+ IntToHex(i, 4)+')';
        ShowHint:= true;
        Width:=96;
        Height:=96;
        Parent:= FlowPanelIcons;
      end;
      n+=1;
    end;
    i+=1;
  end;
end;

procedure TForm1.ToggleButtonChange(Sender: TObject);
begin
  if ToggleButton1.Checked then lblToggleResult1.Caption:='On'
  else lblToggleResult1.Caption:='Off';
end;

procedure TForm1.PageIconsBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin
  if FlowPanelIcons.ControlList.Count<1 then CreatePageIcons;
end;

procedure TForm1.ProgressBarChangeState(Sender: TObject);
var
  rb: TFluentRadioButton;
begin
  if Sender is TFluentRadioButton then
  begin
    rb:= TFluentRadioButton(Sender);
    if rb.Checked then
    begin
      ProgressBar1.ProgressStyle:=  TProgressStyle(rb.Tag);
      lblProgressBarValue.Visible:= ProgressBar1.ProgressStyle = psDeterminate;
      slBarValue.Visible:= lblProgressBarValue.Visible;
    end;
  end;
end;

procedure TForm1.ProgressRingChangeState(Sender: TObject);
var
  rb: TFluentRadioButton;
begin
  if Sender is TFluentRadioButton then
  begin
    rb:= TFluentRadioButton(Sender);
    if rb.Checked then
    begin
      ProgressRing1.ProgressStyle:=  TProgressStyle(rb.Tag);
      lblProgressRingValue.Visible:= ProgressRing1.ProgressStyle = psDeterminate;
      FluentSlider2.Visible:= lblProgressRingValue.Visible;
    end;
  end;
end;

procedure TForm1.slBarValueValueChanged(Sender: TObject);
begin
  ProgressBar1.Value:= slBarValue.Value;
  lblProgressBarValue.Caption:= 'Value: '+IntToStr(round(slBarValue.Value));
end;

end.

