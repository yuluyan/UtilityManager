(* ::Package:: *)

Package["UtilityManager`"]


PackageExport["RGBToHex"]

RGBToHex::usage = "Convert RGBColor to Hex String.";

RGBToHex[color_RGBColor] := 
  StringJoin["#", IntegerString[Round[Level[color, 1]*255], 16, 2]];
  
PackageExport["EvaluateStep"]
(*https://mathematica.stackexchange.com/questions/334/how-do-i-evaluate-only-one-step-of-an-expression/1447#1447*)

EvaluateStep::usage = "Evaluate one step of an expression.";

SetAttributes[EvaluateStep, HoldAll];

EvaluateStep[expr_] :=
  Module[{P},
    P = (P = Return[#, TraceScan] &) &;
    TraceScan[P, expr, TraceDepth -> 1]
  ]
