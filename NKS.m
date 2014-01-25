(* Mathematica Package *)

BeginPackage["NKS`"]
(* Exported symbols added here with SymbolName::usage *)  

CyclicUnion
ApplyDefect
SearchForBackgroundsOfLengthStepRepeat
AllRepeatingBackgrounds
PadAround
HighlightEvenRuns
aph

Begin["`Private`"] (* Begin Private Context *) 

CyclicUnion[lists:{{(1|0)...}..}] :=
DeleteDuplicates@Sort[
	NestWhile[
		RotateRight,
		#,
		!(MemberQ[lists,#])&
	]& /@ 
	Flatten[
		Map[First[#]&,#]& /@ 
		(Map[First[#]&,#]& /@ 
			(Split/@(Sort/@(Sort[NestList[RotateRight,#,Length[#]-1]]&/@#)&/@(Map[Rest[Flatten[#]]&,#]&/@Split[Sort[{Length[#],#}&/@lists,#1[[1]]>#2[[1]]&],#1[[1]]==#2[[1]]&])))),
		1
	]
]

(* by jaiwant *)
(* how is "overlay" or "insert" an offset INTO THE DEFECT at which to begin the defect *) 
ApplyDefect[bg_, defect_, repeat_: 50, how_, offset_]  :=
 Module[{bgg = {}, bgl = Length@bg, rc = repeat, realOff}, 
  realOff = (Floor[rc/2] bgl) + offset; 
  bgg = Flatten@Table[bg, {rc}]; 
  If[offset > bgl + 1, Print["Offset must be <= bg+1 length"]; 
   Return[{}]]; 
  If[ToString[how] == "insert", 
   bgg = Flatten@Insert[bgg, defect, realOff], 
   If[ToString[how] == "overlay", 
    bgg[[realOff ;; (realOff + Length@defect - 1)]] = defect, 
    Print["unknown how"]]]; Return[bgg]]

(* a different algorithm that does the same thing as SearchForBackgroundsOfLength but in a much more efficient way *)
SearchForBackgroundsOfLengthStepRepeat[rule_, length_] :=
 Module[{e, ed, s, 
   sc}, (e = #; Clear[s]; 
     s = SparseArray[{{2^(length + 1) - 1} -> 0}]; sc = 1; 
     ed = FromDigits[Prepend[e, 1], 2]; s[[ed]] = sc; 
     e = CellularAutomaton[rule, e]; 
     ed = FromDigits[Prepend[e, 1], 2];(*Print[e,s[[ed]]];*)
     While[s[[ed]] == 0, sc++; s[[ed]] = sc; 
      e = CellularAutomaton[rule, e]; 
      ed = FromDigits[Prepend[e, 1], 2]]; {sc - s[[ed]] + 1, e}) & /@ 
   Tuples[{0, 1}, length]] 

(* my alias for jaiwant's strangely named function :) *)
AllRepeatingBackgrounds[rule_,length_]:=SearchForBackgroundsOfLengthStepRepeat[rule, length]

PadAround[m_, layers_: 1, value_: 0] := 
 Module[{paddedRightLeft}, 
  paddedRightLeft = 
   Join[Table[value, {layers}], #, Table[value, {layers}]] & /@ m;
  Join[
   Table[value, {layers}, {Length[paddedRightLeft[[1]]]}],
   paddedRightLeft,
   Table[value, {layers}, {Length[paddedRightLeft[[1]]]}]
   ]
  ]

HighlightEvenRuns[l_] :=
 Module[
  {p},
  p = Position[Partition[l, 2, 1], {0, 1} | {1, 0}];
  p = If[p == {}, 0, First@p];
  RotateRight[
   Flatten[If[EvenQ[Length@#], # /. {0 -> 2, 1 -> 3}, #] & /@ 
     Split[RotateLeft[l, p]]], p]
  ]

aph[ca_, ps_: 1, opts___] := 
 Framed@ArrayPlot[ca, 
   ColorRules -> {0 -> White, 1 -> Black, 2 -> Red, 3 -> Blue, 
     4 -> Orange}, PixelConstrained -> ps, Frame -> False, opts]


End[] (* End Private Context *)

EndPackage[]