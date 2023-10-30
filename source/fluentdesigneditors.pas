unit FluentDesignEditors;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, PropEdits, ComponentEditors, ProjectIntf,
  FluentPathIcon;

type
  { TColorPropertyEditor
    PropertyEditor editor for the TColor type.
    Displays the color as a clXXX value if one exists, otherwise displays the value as hex.
    Also allows the clXXX value to be picked from a list.

    Редактор PropertyEditor для типа TColor.
    Отображает цвет как значение clXXX, если оно существует, в противном случае отображает значение в шестнадцатеричном формате.
    Также позволяет выбирать значение clXXX из списка.}

 {   TColorPropertyEditor = class(TIntegerPropertyEditor)
    public
      procedure Edit; override;
      function GetAttributes: TPropertyAttributes; override;
      function OrdValueToVisualValue(OrdValue: longint): string; override;
      procedure GetValues(Proc: TGetStrProc); override;
      procedure SetValue(const NewValue: ansistring); override;
      procedure ListMeasureWidth(const {%H-}CurValue: ansistring; {%H-}Index: integer;
        ACanvas: TCanvas; var AWidth: Integer);  override;
      procedure ListDrawValue(const CurValue: ansistring; Index: integer;
        ACanvas: TCanvas; const ARect:TRect; AState: TPropEditDrawState); override;
      procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
        AState: TPropEditDrawState); override;
    end;
}



  { TCodePointPropertyEditor }

  TCodePointPropertyEditor = class(TIntegerPropertyEditor)
  public
    function OrdValueToVisualValue(OrdValue: longint): string; override;
  end;

  { TPathIconPropertyEditor }

  TPathIconPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

uses
  PathIconEditor, Dialogs, System.UITypes;

//function GetIconNameByIndex(Index: integer): string;
//begin
//  Result:= '';
//  if Index<$e700 then exit;
//  if Index<$e800 then
//  begin
//    IntToIdent(Index, Result, IconNamesE7);
//    exit;
//  end;
//  if Index<$e900 then
//  begin
//    IntToIdent(Index, Result, IconNamesE8);
//    exit;
//  end;
//end;

{ TPathIconPropertyEditor }

procedure TPathIconPropertyEditor.Edit;
var
  PathIconEditorForm: TPathIconEditorForm;
  PathIcon: TPathIcon;
begin
  PathIcon:= TPathIcon(GetObjectValue(TPathIcon));
  PathIconEditorForm:= TPathIconEditorForm.Create(nil);
  PathIconEditorForm.SetPathIcon(PathIcon);
  try
    if PathIconEditorForm.ShowModal = mrOk then
    begin
      PathIcon.Assign(PathIconEditorForm.FluentIconView.PathIcon);
      SetPtrValue(PathIcon);
      Modified;
    end
  finally
    PathIconEditorForm.Free;
  end;
end;

function TPathIconPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

{ TCodePointPropertyEditor }

function CodePointToIdent(CodePoint: TCodePoint; out Ident: string): boolean;
begin
  Ident:='';
  Result:= false;
  if CodePoint<$E700 then exit;

end;

function CodePointToString(CodePoint: TCodePoint): string;
begin
  Result := '';
  if not CodePointToIdent(CodePoint, Result) then
    Result:='$'+HexStr(CodePoint, 4);
end;


function TCodePointPropertyEditor.OrdValueToVisualValue(OrdValue: longint
  ): string;
begin
  if OrdValue = 0 then
    Result := ''
  else
    Result := CodePointToString(TCodePoint(OrdValue));
end;

end.

