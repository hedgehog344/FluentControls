unit ContentDialogEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes,LCLType, PropEdits, ComponentEditors, Forms,
  ContentDialog, ContentDialogEditorForm;

type

  { TContentDialogEditor }

  TContentDialogEditor = class(TComponentEditor)
  protected
    procedure DoOpenContentsEditor;
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{ TContentDialogEditor }

{
procedure TActionListComponentEditor.Edit;
var
  AActionList: TActionList;
  AEditor: TActionListEditor;
begin
  AActionList := GetComponent as TActionList;
  if AActionList = nil
  then raise Exception.Create('TActionListComponentEditor.Edit AActionList=nil');
  AEditor := FindActionEditor(AActionList);
  if not Assigned(AEditor) then begin
    AEditor := TActionListEditor.Create(Application);
    AEditor.lstActionName.ItemIndex := -1;
    AEditor.FCompDesigner := Self.FCompDesigner;
    AEditor.FCompEditor := Self;
    AEditor.SetActionList(AActionList);
  end;
  SetPopupModeParentForPropertyEditor(AEditor);
  AEditor.ShowOnTop;
end;
}

procedure TContentDialogEditor.DoOpenContentsEditor;
var
  ADialog: TContentDialog;
 // FormContentEditor: TFormContentEditor;
begin
  ADialog:= GetComponent as TContentDialog;
  if ADialog = nil then exit;
 //   raise Exception.Create('TContentDialogEditor.Edit AContentDialog=nil');


  if FormContentEditor = nil then
  begin
    FormContentEditor := TFormContentEditor.Create(Application);
    FormContentEditor.SetData(ADialog, self.Designer);
  end;


  //EditWindow.SetData(TSpkToolbar(Component),Self.GetDesigner);

  FormContentEditor.Show;
end;



procedure TContentDialogEditor.Edit;
begin
  DoOpenContentsEditor;
end;

procedure TContentDialogEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0 : DoOpenContentsEditor;
  end;
end;

function TContentDialogEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : result:='Contents editor...';
  end;
end;

function TContentDialogEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

finalization
  if FormContentEditor<>nil then FormContentEditor.Free;

end.

