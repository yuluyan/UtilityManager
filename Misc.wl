(* ::Package:: *)

Package["UtilityManager`"]


PackageExport["RGBToHex"]

RGBToHex::usage = "Convert RGBColor to Hex String.";

RGBToHex[color_RGBColor] := 
  StringJoin["#", IntegerString[Round[Level[color, 1]*255], 16, 2]];



PackageExport["StepEvaluate"]
(*https://mathematica.stackexchange.com/questions/334/how-do-i-evaluate-only-one-step-of-an-expression/1447#1447*)
StepEvaluate::usage = "Evaluate one step of an expression.";
SetAttributes[StepEvaluate, HoldAll];
StepEvaluate[expr_] :=
  Module[{P},
    P = (P = Return[#, TraceScan] &) &;
    TraceScan[P, expr, TraceDepth -> 1]
  ]
  
  
PackageExport["StyledStringJoin"]
StyledStringJoin::usage = "Join string with possibly styles.";
StyledStringJoin[s__] := StringJoin @@ (ToString[#, StandardForm]& /@ {s})

PackageExport["StyledString"]
StyledString::syntxerr="";
StyledString[str_]:=Module[{l={},sub={},n,i,chars,flag={},depth=0,map},
map={
"it"->Italic,
"bd"->Bold,
"math"->{(FontFamily->"Times New Roman"),Italic}
};
n=StringLength[str];
chars=Characters[str];
For[i=1,i<=n,i++,
Switch[chars[[i]],
"`",
If[Length[sub]>0,AppendTo[l,
If[Length[flag]>0,
Style[StringJoin@@sub,(StringJoin@@flag)/.map],
StringJoin@@sub
]
];
sub={}];i++;While[chars[[i]]!="{",AppendTo[flag,chars[[i]]];i++;]
,
"{",
1
,
"}",
AppendTo[l,If[Length[flag]>0,
Style[StringJoin@@sub,(StringJoin@@flag)/.map],
StringJoin@@sub
]];flag={};sub={},
_,
AppendTo[sub,chars[[i]]];
]
];
StyledStringJoin@@l
];
StyledString["moment of `math{x}"]
