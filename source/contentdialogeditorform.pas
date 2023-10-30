unit ContentDialogEditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComponentEditors, Dialogs, ExtCtrls, StdCtrls, IDEWindowIntf,
  Buttons,
  ContentDialog;

type

  { TFormContentEditor }

  TFormContentEditor = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    FDialog: TContentDialog;
    FDesigner: TComponentEditorDesigner;
  public
    procedure SetData(ADialog: TContentDialog; ADesigner: TComponentEditorDesigner);
  end;

var
  FormContentEditor: TFormContentEditor;

implementation

{$R *.lfm}

{ TFormContentEditor }

{
unit ActionsEditor;

procedure TActionListEditor.ActNewExecute(Sender: TObject);
var
  NewAction: TContainedAction;
begin
  if FActionList=nil then exit;
  NewAction := TAction.Create(FActionList.Owner);
  NewAction.Name := FCompDesigner.CreateUniqueComponentName(NewAction.ClassName);

  if lstCategory.ItemIndex > 1 // ignore first two items (virtual categories)
  then NewAction.Category := lstCategory.Items[lstCategory.ItemIndex]
  else NewAction.Category := '';

  NewAction.ActionList := FActionList;

  // Selection updates correctly when we first clear the selection in Designer
  //  and in Object Inspector, then add a new item. Otherwise there is
  //  a loop of back-and-forth selection updates and the new item does not show.
  FCompDesigner.ClearSelection;
  FCompDesigner.PropertyEditorHook.PersistentAdded(NewAction,True);
  FCompDesigner.Modified;
end;
}
procedure TFormContentEditor.SpeedButton1Click(Sender: TObject);
var
  it: TCheckContentItem;
begin
  if (FDialog = nil) or (FDesigner = nil) then
    exit;
  if FDesigner.PropertyEditorHook = nil then
    exit;

  it:= FDialog.AddCheckBoxControl;
  it.Name := FDesigner.CreateUniqueComponentName(it.ClassName);
  ShowMessage(it.Name);

  FDesigner.ClearSelection;
  FDesigner.PropertyEditorHook.PersistentAdded(it, True);
  FDesigner.Modified;
end;

procedure TFormContentEditor.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TFormContentEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  CloseAction := caFree;
end;

procedure TFormContentEditor.SpeedButton2Click(Sender: TObject);
begin
  //
end;

procedure TFormContentEditor.SetData(ADialog: TContentDialog;
  ADesigner: TComponentEditorDesigner);
begin
  FDialog:= ADialog;
  FDesigner:= ADesigner;
end;

end.

