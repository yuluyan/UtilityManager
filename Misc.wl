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
StyledStringJoin[s__] := StringJoin @@ (ToString[#, StandardForm]& /@ s)
