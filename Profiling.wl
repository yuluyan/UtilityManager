(* ::Package:: *)

Package["UtilityManager`"]


PackageExport["quickTiming"]

quickTiming::usage="Timing a expression.";

SetAttributes[quickTiming, {HoldFirst, SequenceHold}];
quickTiming[expr_]:= 
	Print["Time used: ", AbsoluteTiming[expr][[1]], " s."];


PackageExport["timing10"]
PackageExport["timing100"]
PackageExport["timing1000"]
PackageExport["timing10000"]
PackageExport["timingN"]

timing10::usage="Timing a expression for 10 times and give the average.";
timing100::usage="Timing a expression for 100 times and give the average.";
timing1000::usage="Timing a expression for 1000 times and give the average.";
timing10000::usage="Timing a expression for 10000 times and give the average.";
timingN::usage="Timing a expression for certain numbers and give the average.";

SetAttributes[timing10, {HoldFirst, SequenceHold}];
timing10[expr_]:= 
	Print[
		"Average time in 10 runs: ",
		ToString[Unevaluated[expr]],
		"  ", 
		AbsoluteTiming[Do[Evaluate[expr];,{10}];][[1]] / 10,
		" s."
	];
	
SetAttributes[timing100, {HoldFirst, SequenceHold}];
timing100[expr_]:= 
	Print[
		"Average time in 100 runs: ",
		ToString[Unevaluated[expr]],
		"  ", 
		AbsoluteTiming[Do[Evaluate[expr];,{100}];][[1]] / 100,
		" s."
	];
	
SetAttributes[timing1000, {HoldFirst, SequenceHold}];
timing1000[expr_]:= 
	Print[
		"Average time in 1000 runs: ",
		ToString[Unevaluated[expr]],
		"  ", 
		AbsoluteTiming[Do[Evaluate[expr];,{1000}];][[1]] / 1000,
		" s."
	];

SetAttributes[timing10000, {HoldFirst, SequenceHold}];
timing10000[expr_]:= 
	Print[
		"Average time in 10000 runs: ",
		ToString[Unevaluated[expr]],
		"  ", 
		AbsoluteTiming[Do[Evaluate[expr];,{10000}];][[1]] / 10000,
		" s."
	];
	
SetAttributes[timingN, {HoldFirst, SequenceHold}];
timingN[expr_, n_:1]:= 
	Print[
		"Average time in " <> ToString @ n <>" runs: ",
		ToString[Unevaluated[expr]],
		"  ", 
		AbsoluteTiming[Do[Evaluate[expr];,{n}];][[1]] / n,
		" s."
	];
	
