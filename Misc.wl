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


PackageExport["$StyledStringTest"]
$StyledStringTest = "`red{red}`green{green}`blue{blue}`black{black}`white{white}`gray{gray}`cyan{cyan}`magenta{magenta}`yellow{yellow}`brown{brown}`orange{orange}`pink{pink}`purple{purple}`bd{bold}`it{italic}`ul{underline}`lg{larger}`sm{smaller}`math{math}`sz[25]{size25}`rgb[#12ee2e]{rgb12ee2e}`red{`it{`math{`sz[25]{recursive}}}}";

PackageExport["StyledString"]
StyledString::syntxerr = "";
StyledString::syntxerr1 = "`1` is not a valid text style command.";
StyledString::syntxerr2 = "Unknown syntax.";
StyledString[str_, commandSep_:"`"] := Module[
{
	components={},substring={},len,i,chars,flag={},flagStack={},
	simpleCommands,argumentCommands,
	replaceDeclaratives
},
	simpleCommands = {
		"red"->Red,
		"green"->Green,
		"blue"->Blue,
		"black"->Black,
		"white"->White,
		"gray"->Gray,
		"cyan"->Cyan,
		"magenta"->Magenta,
		"yellow"->Yellow,
		"brown"->Brown,
		"orange"->Orange,
		"pink"->Pink,
		"purple"->Purple,
		
		"bd"->Bold,
		"it"->Italic,
		"ul"->Underlined,
		"lg"->Larger,
		"sm"->Smaller,
		
		"math"->(FontFamily->"Times New Roman")
	};
	argumentCommands = <|
		"sz"->(FontSize->ToExpression[#1]&),
		"rgb"->(RGBColor[#1]&)
	|>;
	replaceDeclaratives[dec_]:=Module[{x, cmd, args, res},
		Table[
			If[MemberQ[Keys[simpleCommands],d], 
				d/.simpleCommands,
				If[StringContainsQ[d,"["~~__~~"]"],
					cmd = StringSplit[d, "["][[1]];
					args = StringSplit[StringCases[d, "["~~ x___ ~~"]" -> x][[1]], ","];
					res = Evaluate[argumentCommands[cmd]@@args];
					res
				,
				Message[StyledString::syntxerr1,d];Nothing
				]
			],
			{d, dec}
		]
	];
	len = StringLength[str];
	chars = Characters[str];
	For[i = 1, i <= len, i++,
		Switch[chars[[i]],
			commandSep,
			If[i == len, 
				Message[StyledString::syntxerr2],
				If[chars[[i + 1]] == commandSep,
					AppendTo[substring, "`"];
					i = i + 1;
					,
					If[Length[substring] > 0,
						AppendTo[components,
							If[Length[flagStack] > 0,
								Style[StringJoin @@ substring, replaceDeclaratives @ flagStack],
								StringJoin @@ substring
								]
						];
						substring = {};
					];
					i++;
					While[chars[[i]] != "{",
						AppendTo[flag, chars[[i]]];
						i++;
					];
					i--;
				]
			];
			,
			"{",
			If[Length[flag] > 0,
				AppendTo[flagStack, StringJoin @@ flag];
				flag = {};
			];
			,
			"}",
			AppendTo[components,
				If[Length[flagStack] > 0,
					Style[StringJoin @@ substring, replaceDeclaratives @ flagStack],
					StringJoin @@ substring
				]
			];
			If[Length[flagStack] > 0, 
				flagStack = Delete[flagStack, -1];
			];
			flag = {};
			substring = {},
			_,
			AppendTo[substring, chars[[i]]];
		];
	];
	If[Length[substring] > 0,
		AppendTo[components,
			If[Length[flagStack] > 0,
				Style[StringJoin @@ substring, replaceDeclaratives @ flagStack],
				StringJoin @@ substring
			]
		];
	];
	StyledStringJoin @@ components
];
