unit Registration;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, PropEdits, ComponentEditors, FluentDesignEditors,
  FluentButtons, FluentTextBlock,
  FluentProgress, FluentSlider, FluentTextBox, FluentPasswordBox,
  FluentCheckControls,
  ContentDialog, FluentPathIcon, FluentIconView, FluentIconButton,
  FluentNavigation;


procedure Register;

implementation

procedure Register;
begin
  {$I fluent_icons.lrs}

  RegisterComponents('Fluent Controls',[
    TFluentButton,
    TFluentTextBlock,
    TFluentTextBox,
    TFluentToggleButton,
    TFluentHyperlinkButton,
    TFluentCheckBox,
    TFluentRadioButton,
    TFluentPasswordBox,
    TFluentProgressRing,
    TFluentProgressBar,
    TFluentSlider,
    TContentDialog,
    TFluentIconView,
    TFluentIconButton,
    TFluentNavigationView

  ]);
  RegisterPropertyEditor(TypeInfo(TCodePoint), TPathIcon, 'CodePoint', TCodePointPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TPathIcon), nil, '', TPathIconPropertyEditor);

end;

end.

