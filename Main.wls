(*  Main.wls  —  WolframScript implementation *)
(*  Generates a PBM file identical (byte for byte) to the Python/C#/Haskell output *)

(* ---------- helpers ---------- *)
ruleToBinaryList[r_Integer] := IntegerDigits[r, 2, 8]      (* MSB → LSB *)

calcCell[{a_, b_, c_}, rule_List] := Module[{idx},
  (* pattern 111 maps to rule[[1]], 110 → rule[[2]], …, 000 → rule[[8]] *)
  idx = 1 + (4 a + 2 b + c);            (* 111 → 8, 000 → 1  *)
  rule[[9 - idx]]                      (* invert to match mapping *)
]

nextGeneration[cells_List, rule_List] :=
  Module[{ext = Join[{0, 0}, cells, {0, 0}]},
    Table[
      calcCell[ext[[j - 1 ;; j + 1]], rule],
      {j, 2, Length[ext] - 1}
    ]
  ]

padRow[row_List, w_Integer] :=
  Module[{pad = (w - Length[row])/2 // Round},
    Join[ConstantArray[0, pad], row, ConstantArray[0, pad]]
  ]

(* ---------- main ---------- *)
lines = StringSplit[Import["input.txt", "Text"], "\n", 3];

{ruleNumber, initString, gens} =
  {ToExpression@lines[[1]], lines[[2]], ToExpression@lines[[3]]};

ruleBits   = ruleToBinaryList[ruleNumber];
cells0     = ToExpression /@ Characters[initString];       (* 0/1 list *)
finalWidth = StringLength[initString] + 2 gens;

(* build generations *)
rows = NestList[nextGeneration[#, ruleBits] &, cells0, gens - 1];

(* pad every row to finalWidth *)
rowsPadded = padRow[#, finalWidth] & /@ rows;

(* build PBM text *)
pbm =
  StringRiffle[
    Join[{StringJoin["P1\n", ToString[finalWidth], " ", ToString[gens]]},
      (StringJoin @@ ToString /@ #) & /@ rowsPadded], "\n"] <> "\n";

(* write output *)
If[! DirectoryQ["results"], CreateDirectory["results"]];

file = StringTemplate[
        "results/r`r`_g`g`_i`i`_wolfram.pbm"][
        <|"r" -> ruleNumber, "g" -> gens, "i" -> initString|>
      ];


Export[file, pbm, "Text"];   (* ASCII text, Unix new‑lines *)

(* optional confirmation *)
Print["Wrote ", file];
