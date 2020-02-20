(* ::Package:: *)

Package["UtilityManager`"]


PackageExport["$SaveImagePath"]

$SaveImagePath = Quiet @ NotebookDirectory[];


PackageExport["saveImg"]

saveImg::usage = "Saving image to img file.";
saveImg::nosave = "Please specify the location to save or save the notebook to have default location.";

SetAttributes[saveImg, HoldFirst];
Options[saveImg] = {ImageSize -> Automatic, FileName -> Automatic};
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
			If[StringQ[OptionValue[FileName]],
				OptionValue[FileName] <> ext,
				ToString[SymbolName[Unevaluated @ g]] <> ext
			]
		}], g, 
	ImageSize -> OptionValue[ImageSize]];
);

PackageExport["savePNG"]

savePNG::usage = "Saving image to .png file.";

SetAttributes[savePNG, HoldFirst];
Options[savePNG] = Options[saveImg];
savePNG[g_,  OptionsPattern[]] :=
	saveImg[g, ".png", 
		ImageSize -> OptionValue[ImageSize], 
		FileName -> OptionValue[FileName]
	];
	
PackageExport["savePDF"]

savePDF::usage = "Saving image to .pdf file.";

SetAttributes[savePDF, HoldFirst];
Options[savePDF] = Options[saveImg];
savePDF[g_,  OptionsPattern[]] :=
	saveImg[g, ".pdf", 
		ImageSize -> OptionValue[ImageSize], 
		FileName -> OptionValue[FileName]
	];


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
read::nosave = "Please save the notebook first.";

SetAttributes[read, HoldFirst];
read[data_]:= Module[{nbdir, dir, name, path},
	nbdir = Quiet @ NotebookDirectory[];
	If[FailureQ[$SaveImagePath],
		Message[read::nosave];
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

