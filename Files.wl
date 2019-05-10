(* ::Package:: *)

Package["UtilityManager`"]


PackageExport["$SaveImagePath"]

$SaveImagePath = Quiet @ NotebookDirectory[];


PackageExport["saveImg"]

saveImg::usage = "Saving image to PNG file.";

SetAttributes[saveImg, HoldFirst];
Options[saveImg] = {ImageSize -> Automatic};
saveImg[g_, ext_:".png", OptionsPattern[]] := (
	If[!DirectoryQ[$SaveImagePath], 
		$SaveImagePath = NotebookDirectory[];
	];
	Export[
		FileNameJoin[{
			$SaveImagePath,
			ToString[SymbolName[Unevaluated@g]] <> ext
		}], g, 
	ImageSize -> OptionValue[ImageSize]];
);


PackageExport["save"]

save::usage="Save data to Mathematica file.";
save::exist="Already saved. Set over write to True to overwrite.";

SetAttributes[save, HoldFirst];
save[data_, overwrite_:False] := Module[{dir, name, path},
	dir = FileNameJoin[{NotebookDirectory[], "data_" <> FileBaseName[NotebookFileName[]]}];
	name = ToString[SymbolName[Unevaluated@data]] <> ".mx";
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
read[data_]:= Module[{dir, name, path},
	dir = FileNameJoin[{NotebookDirectory[], "data_" <> FileBaseName[NotebookFileName[]]}];
	name = ToString[SymbolName[Unevaluated@data]] <> ".mx";
	path = FileNameJoin[{dir, name}];
	If[FileExistsQ[path],
		Get[path],
		Message[read::nonexist]
	];
];

