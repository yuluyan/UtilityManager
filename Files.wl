(* ::Package:: *)

Package["UtilityManager`"]


PackageExport["$SaveImagePath"]

$SaveImagePath = Quiet @ NotebookDirectory[];


PackageExport["saveImg"]

saveImg::usage = "Saving image to img file.";
saveImg::nosave = "Please specify the location to save or save the notebook to have default location.";

SetAttributes[saveImg, HoldFirst];
Options[saveImg] = {ImageSize -> Automatic};
saveImg[g_, ext_, OptionsPattern[]] := (
	If[FailureQ[$SaveImagePath], 
		$SaveImagePath = Quiet @ NotebookDirectory[];
		If[FailureQ[$SaveImagePath],
			Message[saveImg::nosave];
			Return[$Failed];
		];
	];
	Export[
		FileNameJoin[{
			$SaveImagePath,
			ToString[SymbolName[Unevaluated @ g]] <> ext
		}], g, 
	ImageSize -> OptionValue[ImageSize]];
);

PackageExport["savePNG"]

savePNG::usage = "Saving image to .png file.";

SetAttributes[savePNG, HoldFirst];
Options[savePNG] = {ImageSize -> Automatic};
savePNG[g_,  OptionsPattern[]] :=
	saveImg[g, ".png", ImageSize -> OptionValue[ImageSize]];
	
PackageExport["savePDF"]

savePDF::usage = "Saving image to .pdf file.";

SetAttributes[savePDF, HoldFirst];
Options[savePDF] = {ImageSize -> Automatic};
savePDF[g_,  OptionsPattern[]] :=
	saveImg[g, ".pdf", ImageSize -> OptionValue[ImageSize]];


PackageExport["save"]

save::usage="Save data to Mathematica file.";
save::exist="Already saved. Set over write to True to overwrite.";
save::nosave = "Please save the notebook first.";

SetAttributes[save, HoldFirst];
save[data_, overwrite_:False] := Module[{nbdir, dir, name, path},
	nbdir = Quiet @ NotebookDirectory[];
	If[FailureQ[$SaveImagePath],
		Message[save::nosave];
		Return[$Failed];
	];
	dir = FileNameJoin[{nbdir, "data_" <> FileBaseName[NotebookFileName[]]}];
	name = ToString[SymbolName[Unevaluated @ data]] <> ".mx";
	path = FileNameJoin[{dir, name}];
	If[!DirectoryQ[dir],CreateDirectory[dir]];
	If[overwrite || (!FileExistsQ[path]),
		DumpSave[path, data],
		Message[save::exist]
	];
];


PackageExport["read"]

read::usage="Read data from Mathematica file.";
read::nonexist="Data not exist.";

SetAttributes[read, HoldFirst];
read[data_]:= Module[{nbdir, dir, name, path},
	nbdir = Quiet @ NotebookDirectory[];
	If[FailureQ[$SaveImagePath],
		Message[save::nosave];
		Return[$Failed];
	];
	dir = FileNameJoin[{nbdir, "data_" <> FileBaseName[NotebookFileName[]]}];
	name = ToString[SymbolName[Unevaluated @ data]] <> ".mx";
	path = FileNameJoin[{dir, name}];
	If[FileExistsQ[path],
		Get[path],
		Message[read::nonexist]
	];
];

