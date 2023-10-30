# FluentControls
Fluent Controls Package for Lazarus

## Installing

1. Download Codebot.Cross library from: https://github.com/sysrpl/Codebot.Cross
2. Open **Codebot.lpk** (...source/codebot folder).
3. Open **Codebot.Graphics.Windows.SurfaceD2D** unit.
4. Replace line 2259: "Layout := Create**Gdi**TextLayout(FontObj.Format, Text, Rect.Width, Rect.Height);" with "Layout := CreateTextLayout(FontObj.Format, Text, Rect.Width, Rect.Height);"
5. Compile **Codebot** package.
6. Open and compile **FluentControls.lpk** (...source folder).
7. Open and compile **FluentControlsDesign.lpk** (...source folder).
8. Install **FluentControlsDesign** package.

## Components
1. TFluentButton
2. TFluentTextBlock
3. TFluentTextBox
4. TFluentToggleButton
5. TFluentHyperlinkButton
6. TFluentCheckBox
7. TFluentRadioButton
8. TFluentPasswordBox
9. TFluentProgressRing
10. TFluentProgressBar
11. TFluentSlider
12. TContentDialog
13. TFluentIconView
14. TFluentIconButton
15. TFluentNavigationView


