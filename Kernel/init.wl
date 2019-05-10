(* ::Package:: *)

BeginPackage["UtilityManager`"];

Begin["UtilityManager`Private`"];

PreemptProtect @ Quiet[

(* load dependancies *)

TakeDrop; (* This causes loading of Language`PairFunctions` *)
Needs["GeneralUtilities`"];

$umSymbols = GeneralUtilities`ClearPacletExportedSymbols["UtilityManager"];
General::umloaderr="Could not load the utility functions.";

$basePath = DirectoryName[$InputFileName, 2];

(*Change this for yourself*)
$githubUser = "yuluyan";
$githubRepo = "UtilityManager";

$commitsURL = "https://api.github.com/repos/" <> $githubUser <> "/" <> $githubRepo <> "/commits";
$downloadURL = "https://github.com/" <> $githubUser <> "/" <> $githubRepo <> "/archive/master.zip";

(* update from github *)
checkUpdate::timeout = "Update checking timed out, please try again.";
checkUpdate[] := Module[{verFile, localVer, json, latest},
	json = TimeConstrained[
		Import[$commitsURL,"JSON"], 0.1,
		Message[checkUpdate::timeout]; Return[{False, ""}]
	];
	latest = Lookup[First @ json, "sha"];
	verFile = FileNameJoin[{$basePath, "latest.sha"}];
	{If[!FileExistsQ[verFile], True,
		localVer = Import[verFile, "Text"];
		localVer != latest
	], latest}
];
	
updateUtility[] := Module[
	{tmpDir, target, allfiles, files, folders},
	Check[
		tmpDir = CreateDirectory[];
		target = FileNameJoin[{tmpDir, "UtilityManagerUpdate.zip"}];
		If[$Notebooks,
			PrintTemporary @ Labeled[ProgressIndicator[Appearance -> "Necklace"], "Updating UtilityManager...", Right], 
			Print["Updating UtilityManager..."]
		];
		URLSave[$downloadURL, target];
		ExtractArchive[target, tmpDir];
		target = FileNameJoin[{tmpDir, "UtilityManager-master"}];
		allfiles = FileNames[All, target];
		files = Select[allfiles, GeneralUtilities`FileQ];
		folders = Complement[allfiles, files];
		folders = Select[folders, FileNameTake[#] != "Kernel"&];
		CopyFile[#, 
			FileNameJoin[{$basePath, FileNameTake[#, FileNameDepth[target] - FileNameDepth[#]]}],
			OverwriteTarget -> True
		]& /@ files;
		(
			Quiet @ DeleteDirectory[FileNameJoin[{$basePath, FileNameTake[#]}], DeleteContents -> True];
			CopyDirectory[#, FileNameJoin[{$basePath, FileNameTake[#]}]]
		)& /@ folders;
	,
		Return[$Failed]
	];
];

UtilityManager`UpdateUtility::updterr = "UtilityManager fail to update.";
UtilityManager`UpdateUtility[] := Module[{needUpdate, version},
	{needUpdate, version} = checkUpdate[];
	If[needUpdate,
		If[!FailureQ[updateUtility[]], 
			Export[FileNameJoin[{$basePath, "latest.sha"}], version, "Text"];,
			Message[UtilityManager`UpdateUtility::updterr];
		];
	];
];

initializeManager[] := (
	(* obtain files to load *)
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

	(* helper to reload a single file *)
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

UtilityManager`UpdateUtility[];

initializeManager[];

Scan[Get, $files];

SetAttributes[Evaluate @ $umSymbols, {Protected, ReadProtected}];	

(* end of PreemptProtect and Quiet *)
, {RuleDelayed::rhs, General::shdw}];

End[];

EndPackage[];
