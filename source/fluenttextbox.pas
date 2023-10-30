// ------------------------------
// Fluent Controls Package
// https://github.com/hedgehog344
// ------------------------------

unit FluentTextBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, StdCtrls,

  CustomTextBox;

type

  { TCustomFluentTextBox }

  TCustomFluentTextBox = class(TCustomTextBox)
  private
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(AValue: TEditCharCase);
  public
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
  end;

  TFluentTextBox = class(TCustomFluentTextBox)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property CharCase;
    property Constraints;
    property Enabled;
    property Font;
    property Header;
    property MaxLength;
    property ReadOnly;

    property ParentBiDiMode;
    property ParentFont;
    property PlaceholderText;
    property Text;
    property Visible;

    property OnChange;
    property Color;
    property Cursor;
    property Hint;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property ParentColor;
    property ParentShowHint;

    property ShowHint;

    property TabOrder;
    property TabStop;
  end;

implementation

{ TCustomFluentTextBox }

function TCustomFluentTextBox.GetCharCase: TEditCharCase;
begin
  Result:= Edit.CharCase;
end;

procedure TCustomFluentTextBox.SetCharCase(AValue: TEditCharCase);
begin
  Edit.CharCase:= AValue;
end;

end.
