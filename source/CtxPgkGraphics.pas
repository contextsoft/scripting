(******************************************************************************)
(*
(*  Context Scripting Suite
(*
(*  Registration of basic constants and classes from Graphics unit
(*
(*                TCtxPkgGraphics = class (TCtxPackage)
(*
(*  Copyright (c) 2010 Michael Baytalsky
(*
(******************************************************************************)
unit CtxPgkGraphics;

interface

uses CtxScript;

type
  {:: The only purpose of this component is to make sure CtxClassesLib unit is }
  {:: included into uses clause. }
  TCtxPkgGraphics = class (TCtxPackage);

implementation

uses Graphics;

initialization
  with TCtxCustomIntrospector.Create(TCtxSystemScope) do
  begin
    AddConst('clScrollBar', clScrollBar);
    AddConst('clBackground', clBackground);
    AddConst('clActiveCaption', clActiveCaption);
    AddConst('clInactiveCaption', clInactiveCaption);
    AddConst('clMenu', clMenu);
    AddConst('clWindow', clWindow);
    AddConst('clWindowFrame', clWindowFrame);
    AddConst('clMenuText', clMenuText);
    AddConst('clWindowText', clWindowText);
    AddConst('clCaptionText', clCaptionText);
    AddConst('clActiveBorder', clActiveBorder);
    AddConst('clInactiveBorder', clInactiveBorder);
    AddConst('clAppWorkSpace', clAppWorkSpace);
    AddConst('clHighlight', clHighlight);
    AddConst('clHighlightText', clHighlightText);
    AddConst('clBtnFace', clBtnFace);
    AddConst('clBtnShadow', clBtnShadow);
    AddConst('clGrayText', clGrayText);
    AddConst('clBtnText', clBtnText);
    AddConst('clInactiveCaptionText', clInactiveCaptionText);
    AddConst('clBtnHighlight', clBtnHighlight);
    AddConst('cl3DDkShadow', cl3DDkShadow);
    AddConst('cl3DLight', cl3DLight);
    AddConst('clInfoText', clInfoText);
    AddConst('clInfoBk', clInfoBk);
    AddConst('clHotLight', clHotLight);
    AddConst('clGradientActiveCaption', clGradientActiveCaption);
    AddConst('clGradientInactiveCaption', clGradientInactiveCaption);
    AddConst('clMenuHighlight', clMenuHighlight);
    AddConst('clMenuBar', clMenuBar);
    AddConst('clBlack', clBlack);
    AddConst('clMaroon', clMaroon);
    AddConst('clGreen', clGreen);
    AddConst('clOlive', clOlive);
    AddConst('clNavy', clNavy);
    AddConst('clPurple', clPurple);
    AddConst('clTeal', clTeal);
    AddConst('clGray', clGray);
    AddConst('clSilver', clSilver);
    AddConst('clRed', clRed);
    AddConst('clLime', clLime);
    AddConst('clYellow', clYellow);
    AddConst('clBlue', clBlue);
    AddConst('clFuchsia', clFuchsia);
    AddConst('clAqua', clAqua);
    AddConst('clLtGray', clLtGray);
    AddConst('clDkGray', clDkGray);
    AddConst('clWhite', clWhite);
    AddConst('clMoneyGreen', clMoneyGreen);
    AddConst('clSkyBlue', clSkyBlue);
    AddConst('clCream', clCream);
    AddConst('clMedGray', clMedGray);
    AddConst('clNone', clNone);
    AddConst('clDefault', clDefault);
  end;
end.
