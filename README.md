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
