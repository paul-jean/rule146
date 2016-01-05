(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["Rule146`"]
(* Exported symbols added here with SymbolName::usage *)  

HighlightEvenRuns
aph
ParticleInit

Begin["`Private`"] (* Begin Private Context *) 

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

ParticleInit[length_]:=RotateRight[Riffle[RandomInteger[1,Ceiling[length/2]],0],Floor[length/2]]


End[] (* End Private Context *)

EndPackage[]
