(* ::Package:: *)

Package["UtilityManager`"]


PackageExport["RGBToHex"]

RGBToHex::usage = "Convert RGBColor to Hex String.";

RGBToHex[color_RGBColor] := 
  StringJoin["#", IntegerString[Round[Level[color, 1]*255], 16, 2]];
