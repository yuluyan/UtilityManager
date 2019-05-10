(* ::Package:: *)

BeginPackage["UtilityManager`"];

Begin["UtilityManager`Private`"];

PreemptProtect @ Quiet[

(* load dependancies *)

TakeDrop; (* This causes loading of Language`PairFunctions` *)
Needs["GeneralUtilities`"];

$umSymbols = GeneralUtilities`ClearPacletExportedSymbols["UtilityManager"];
General::umloaderr="Could not load the utility functions.";

initUM[] := (
	(* obtain files to load *)
	$basePath = DirectoryName[$InputFileName, 2];
	subPath[p__] := FileNameJoin[{$basePath, p}];
	$allFiles = Select[FileNames["*.wl", $basePath, Infinity], GeneralUtilities`FileQ];
	$ignoreFiles = Flatten @ {
		FileNames["*.wl", subPath /@ {".git"}, Infinity],
		{subPath["Kernel", "init.wl"], subPath["README.md"], subPath["LICENSE"]}
	};
	$files = Complement[$allFiles, $ignoreFiles];
	
	(* scan for scoped and exported symbols *)
	$lines = StringSplit[StringTrim @ FindList[$files, {"PackageScope", "PackageExport"}], {"[", "]", "\""}];
	$private = Cases[$lines, {"PackageScope", _, name_} :> name];
	$public =  Cases[$lines, {"PackageExport", _, name_} :> name];
	$public = Complement[$public, Names["System`*"]];

	(* create symbols in the right context *)
	createInContext[context_, names_] := Block[{$ContextPath = {}, $Context = context}, ToExpression[names, InputForm, Hold]];
	createInContext["UtilityManager`", $public];
	createInContext["UtilityManager`Private`", $private];

	(* introduce helper to make it easy to reload a single file *)
	UtilityManager`ReloadFile[subpath_] := Block[
		{$ContextPath = $contexts},
		Unprotect @@ $umSymbols;
		loadFile @ FileNameJoin[{$basePath, subpath}];
		Get @ FileNameJoin[{$basePath, subpath}];
		Protect @@ $umSymbols;
	];

	fileContext[file_] := Block[{dir, base},
		dir = FileNameTake[file, {-2}];
		base = FileBaseName @ If[StringEndsQ[dir, ".nb"], dir, FileNameTake[file]];
		StringJoin["UtilityManager`Private`", base, "`"]
	];

	$fileTimings = Association[];
	loadFile[file_] := Block[
		{$Context = fileContext[file], contents, time},
		contents = GeneralUtilities`FileString[file];
		If[!StringStartsQ[contents, "Package[\"UtilityManager`\"]"], Return[]];
		contents = StringDrop[contents, 27];
		time = First @ AbsoluteTiming @ Check[
			GeneralUtilities`$CurrentFileName = file;
			ToExpression[contents, InputForm];
			GeneralUtilities`$CurrentFileName = None;
		,		
			errs = GeneralUtilities`FindSyntaxErrors[file];
			If[errs =!= {}, Print[errs]];
			Message[General::umloaderr];
			Return[$Failed, PreemptProtect];
		];
		$fileTimings[FileNameTake[file]] = time;
	];
	
	$contexts = {"System`", "Developer`", "Internal`", "GeneralUtilities`", "UtilityManager`", "UtilityManager`Private`"};
	
	Block[{$ContextPath = $contexts}, 
		(* load all the ordinary .wl code files *)
		GeneralUtilities`CatchFailure[General, Scan[loadFile, $files]];
	];

);

initUM[];

Scan[Get, $files];

(* protect all symbols *)
SetAttributes[Evaluate @ $umSymbols, {Protected, ReadProtected}];	

(* end of PreemptProtect and Quiet *)
, {RuleDelayed::rhs, General::shdw}];

End[];

EndPackage[];
