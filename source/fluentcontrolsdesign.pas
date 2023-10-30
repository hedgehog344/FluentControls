{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit FluentControlsDesign;

{$warn 5023 off : no warning about unused units}
interface

uses
  ContentDialogEditor, ContentDialogEditorForm, FluentDesignEditors, 
  PathIconEditor, Registration, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Registration', @Registration.Register);
end;

initialization
  RegisterPackage('FluentControlsDesign', @Register);
end.
