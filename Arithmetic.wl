(* ::Package:: *)

Package["UtilityManager`"]

PackageExport["MinAndPosition"]

MinAndPosition::usage="Give the position and value of the minimum of a list.";

minAndPosCompiled = Compile[{{list, _Real, 1}},
	Module[{idx = 1, min = First[list], len=Length[list]},
		min;
		Do[
			If[min > list[[i]], min = list[[idx = i]]],
		{i, 2, len}];
	{idx, min}
	], CompilationTarget -> "C", RuntimeOptions-> "Speed"
];

MinAndPosition[list_] := {IntegerPart[#[[1]]], #[[2]]}&[minAndPosCompiled[list]];


PackageExport["MaxAndPosition"]

MaxAndPosition::usage="Give the position and value of the maximum of a list.";

maxAndPosCompiled = Compile[{{list, _Real, 1}},
	Module[{idx = 1, max = First[list], len=Length[list]},
		max;
		Do[
			If[max < list[[i]], max = list[[idx = i]]],
		{i, 2, len}];
	{idx, max}
	], CompilationTarget -> "C", RuntimeOptions-> "Speed"
];

MaxAndPosition[list_] := {IntegerPart[#[[1]]], #[[2]]}&[maxAndPosCompiled[list]];
