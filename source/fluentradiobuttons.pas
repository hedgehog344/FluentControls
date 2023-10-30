
unit FluentRadioButtons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  Codebot.System,
  FluentCheckControls;

type

  { TFluentRadioButtons }

  TFluentRadioButtons = class(TCustomControl)
  private
    FButtonList: specialize TArrayList<TFluentRadioButton>;
    FColumns: integer;
    FItemIndex: integer;
    FItems: TStrings;
    FOnSelectionChanged: TNotifyEvent;
    procedure CreateRadioButtons;
    procedure ItemsChanged(Sender: TObject);
    procedure SetColumns(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);

    procedure ButtonChanged(Sender: TObject);
  protected
//    procedure InitializeWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoSize;
    property ItemIndex: integer read FItemIndex write SetItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
    property Columns: integer read FColumns write SetColumns default 1;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

implementation

uses
  Utils;

{ TFluentRadioButtons }

procedure TFluentRadioButtons.CreateRadioButtons;
var
  i: integer;
  ARadioButton: TFluentRadioButton;
begin
  while FButtonList.Length > FItems.Count do
    FButtonList.Pop.Free;


  while FButtonList.Length < FItems.Count do
  begin
    ARadioButton := TFluentRadioButton.Create(Self);
    with ARadioButton do
    begin
      Name := 'RadioButton'+IntToStr(FButtonList.Length);
   //   OnClick := @Self.Clicked;
      OnChange := @ButtonChanged;
     // OnEnter := @Self.ItemEnter;
     // OnExit := @Self.ItemExit;
     // OnKeyDown := @Self.ItemKeyDown;
     // OnKeyUp := @Self.ItemKeyUp;
    //  OnKeyPress := @Self.ItemKeyPress;
     // OnUTF8KeyPress := @Self.ItemUTF8KeyPress;
     // OnResize := @Self.ItemResize;
   //   ParentFont := True;
      BorderSpacing.CellAlignHorizontal := ccaLeftTop;
      BorderSpacing.CellAlignVertical := ccaCenter;
      ControlStyle := ControlStyle + [csNoDesignSelectable];
    end;
    FButtonList.Push(ARadioButton);
  end;
  if (FItemIndex>=FButtonList.Length) and not (csLoading in ComponentState) then FItemIndex:=FButtonList.Length-1;
  if FButtonList.Length> 0 then
  begin
    for i:= 0 to FButtonList.Length-1 do
    begin
      ARadioButton:= FButtonList[i];
      ARadioButton.Caption:= FItems[i];
      ARadioButton.Parent:= self;
    end;
    for i:= 0 to FButtonList.Length-1 do
    begin
      ARadioButton:= FButtonList[i];
      ARadioButton.Checked:= (i= FItemIndex);
    end;
  end;

end;

procedure TFluentRadioButtons.SetColumns(AValue: integer);
begin
  if (FColumns=AValue) or (AValue<1) then Exit;
  FColumns:=AValue;
  ChildSizing.ControlsPerLine:= FColumns;
end;

procedure TFluentRadioButtons.SetItemIndex(AValue: integer);
begin
  if FItemIndex = AValue then exit;
  if AValue>=FButtonList.Length then exit;
  FItemIndex:= AValue;
  FButtonList[FItemIndex].Checked:= true;
end;

procedure TFluentRadioButtons.SetItems(AValue: TStrings);
begin
  if (AValue <> FItems) then
  begin
    FItems.Assign(AValue);
    CreateRadioButtons;
  end;
 // self.AdjustSize;
  self.InvalidatePreferredChildSizes;
  Invalidate;
end;

procedure TFluentRadioButtons.ItemsChanged(Sender: TObject);
begin
  self.CreateRadioButtons;
end;

procedure TFluentRadioButtons.ButtonChanged(Sender: TObject);
var
  Btn: TFluentRadioButton;
  index: integer;
begin
  if Sender is TFluentRadioButton then
  begin
    Btn:= TFluentRadioButton(Sender);
    if Btn.Checked then
    begin
      index:= FButtonList.IndexOf(Btn);
      FItemIndex:= index;
      if Assigned(FOnSelectionChanged) then
        FOnSelectionChanged(self);
    end;
  end;
end;

//procedure TFluentRadioButtons.InitializeWnd;
//begin
//
//end;

constructor TFluentRadioButtons.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  ControlStyle := ControlStyle + [csNoFocus] - [csOpaque, csCaptureMouse, csSetCaption];
  FItems := TStringList.Create;
  TStringList(FItems).OnChange:= @ItemsChanged;
  FItemIndex  := -1;
  FColumns  := 1;
  ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
  ChildSizing.ControlsPerLine:=FColumns;
  //shrink
  ChildSizing.ShrinkHorizontal:=crsScaleChilds;
  ChildSizing.ShrinkVertical:=crsScaleChilds;
  //enlarge
  ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;
  ChildSizing.EnlargeVertical:= crsAnchorAligning; //crsHomogenousChildResize;
  //spacing
  ChildSizing.LeftRightSpacing:=DefMarginSize;
  ChildSizing.TopBottomSpacing:=0;
  ChildSizing.HorizontalSpacing:= DefMarginSize;
  ChildSizing.VerticalSpacing:= DefMarginSize;
end;

destructor TFluentRadioButtons.Destroy;
begin
  FButtonList.Clear;
  FItems.Free;
  inherited Destroy;
end;

end.

