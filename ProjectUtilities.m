(*
Created on 2010/07/11

Imported from
/Users/pjl/Documents/nks/functions/CreateProjectNotebook.nb
*)

(* Mathematica Package *)
BeginPackage["ProjectUtilities`"]
(* Exported symbols added here with SymbolName::usage *)  

CreateProjectNotebook

Begin["`Private`"] 
(* Begin Private Context *)

CreateProjectNotebook[overviewtext_:""]:=NotebookPut[Notebook[
{Cell["Summary","SubsectionIcon"],
Cell["M version","SubsubsectionIcon"],
Cell[ToString[$Version],"Text"],
Cell["Date","SubsubsectionIcon"],
Cell[ToString[Date[]],"Text"],
Cell["Purpose","SubsubsectionIcon"],
Cell[overviewtext,"Text"],
Cell["Log book page","SubsubsectionIcon"],
Cell["Packages","SubsectionIcon"],
Cell["Functions","SubsectionIcon"],
Cell["...","SubsectionIcon"]},
WindowSize->{550,700},FontFamily->"Verdana",StyleDefinitions->"HelpBrowser.nb"]
]


End[] 
(* End Private Context *)

EndPackage[]
