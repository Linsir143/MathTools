(* ::Package:: *)

(* loader.wl *)
InitializeMathTools[] := Module[
  {zipUrl, destDir, localZip, dllPath},
  
  zipUrl = "https://raw.githubusercontent.com/Linsir143/MathTools/main/MathTools.zip";
  destDir = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "MathTools"}];
  If[Not[DirectoryQ[destDir]], CreateDirectory[destDir]];
  
  dllPath = FileNameJoin[{destDir, "MathToolsLib.dll"}];
  
  If[Not[FileQ[dllPath]],
    Print["[MathTools] Downloading libraries from GitHub..."];
    localZip = FileNameJoin[{destDir, "MathTools.zip"}];
    URLDownload[zipUrl, localZip];
    Print["[MathTools] Extracting libraries..."];
    ExtractArchive[localZip, destDir];
    DeleteFile[localZip];
    Print["[MathTools] Core libraries installed successfully!"];
  ];
  
  SetEnvironment["PATH" -> destDir <> ";" <> Environment["PATH"]];
  
  Print["[MathTools] Loading computational engine..."];
  
  cBivarResultant = LibraryFunctionLoad[dllPath, "BivariateResultant_LibraryLink", 
     {"UTF8String", "UTF8String", "UTF8String", "UTF8String", Integer}, "UTF8String"];
     
  cBivarDiscriminant = LibraryFunctionLoad[dllPath, "BivariateDiscriminant_LibraryLink", 
     {"UTF8String", "UTF8String", "UTF8String", Integer}, "UTF8String"];
     
  cMultivarResultant = LibraryFunctionLoad[dllPath, "MultivariateResultant_LibraryLink",
     {"UTF8String", "UTF8String", "UTF8String", "UTF8String"}, "UTF8String"];
     
  cMultivarDiscriminant = LibraryFunctionLoad[dllPath, "MultivariateDiscriminant_LibraryLink", 
     {"UTF8String", "UTF8String", "UTF8String"}, "UTF8String"];
     
  cNFPolyGCD = LibraryFunctionLoad[dllPath, "NFPolyGCD_LibraryLink", 
     {"UTF8String", "UTF8String", "UTF8String","UTF8String","UTF8String"}, "UTF8String"];
     
  cTragerFactor = LibraryFunctionLoad[dllPath, "NFPolyFactorization_LibraryLink", 
     {"UTF8String", "UTF8String", "UTF8String", "UTF8String"}, "UTF8String"];
  
  Print["[MathTools] MathToolsLib successfully initialized!"];
];



(* ================== \:7ed3\:5f0f\:51fd\:6570 ================== *)
(* OutputFormat = 0\:ff0c\:8fd4\:56de\:7cfb\:6570\:5217\:8868\:5b57\:7b26\:4e32\:ff0c\:5426\:5219\:8fd4\:56de\:591a\:9879\:5f0f\:5b57\:7b26\:4e32 *)
MyBivarResultant[polyA_, polyB_, var_, OutputFormat_: 0] := Module[
  {vars, remVar, denA, denB, intA, intB, degA, degB, strA, strB, elimStr, remStr, resStr, formatFlag, factor, coeffs},
  vars = Variables[{polyA, polyB}];
  remVar = If[Length[vars] >= 2, First[DeleteCases[vars, var]], y];
  {denA, intA} = MapAt[1/# &, FactorTermsList[polyA], {1}];
  {denB, intB} = MapAt[1/# &, FactorTermsList[polyB], {1}];
  degA = Exponent[polyA, var];
  degB = Exponent[polyB, var];
  strA = ToString[InputForm[intA]]; 
  strB = ToString[InputForm[intB]]; 
  elimStr = ToString[var]; 
  remStr = ToString[remVar];
  (* \:5224\:65ad\:8f93\:51fa\:683c\:5f0f\:6807\:5fd7 *)
  formatFlag = If[OutputFormat === 0, 0, 1];
  resStr = cBivarResultant[strA, strB, elimStr, remStr, formatFlag];
  factor = (denA^degB)*(denB^degA);
  (* \:6839\:636e\:683c\:5f0f\:9009\:62e9\:4e0d\:540c\:7684\:89e3\:6790\:5668 *)
  If[formatFlag === 1,
    (* \:5b57\:7b26\:4e32\:591a\:9879\:5f0f\:76f4\:63a5\:4ea4\:7531\:5185\:6838\:5316\:7b80\:89e3\:6790 *)
    ToExpression[resStr] / factor
    ,
    (* \:6781\:901f\:6570\:7ec4\:89e3\:6790 *)
    coeffs = ToExpression[resStr] / factor;
    coeffs . remVar ^ Range[0, Length@coeffs - 1]
  ]
];

(* ================== \:5224\:522b\:5f0f\:51fd\:6570 ================== *)
MyBivarDiscriminant[polyA_, var_, OutputFormat_: 0] := Module[
  {vars, remVar, denA, intA, degA, strA, elimStr, remStr, resStr, formatFlag, factor, coeffs},
  vars = Variables[polyA];
  remVar = If[Length[vars] >= 2, First[DeleteCases[vars, var]], y];
  {denA, intA} = MapAt[1/# &, FactorTermsList[polyA], {1}];
  degA = Exponent[polyA, var];
  strA = ToString[InputForm[intA]];
  elimStr = ToString[var]; 
  remStr = ToString[remVar];
  formatFlag = If[OutputFormat === 0, 0, 1];
  resStr = cBivarDiscriminant[strA, elimStr, remStr, formatFlag];
  (* \:5224\:522b\:5f0f\:53bb\:5206\:6bcd\:56e0\:5b50: f(x) = p(x)/denA => Disc(f) = Disc(p) / denA^(2*degA - 2) *)
  factor = denA^(2 * degA - 2);
  If[formatFlag === 1,
    ToExpression[resStr] / factor
    ,
    coeffs = ToExpression[resStr] / factor;
    coeffs . remVar ^ Range[0, Length@coeffs - 1]
  ]
];

(* ================== \:591a\:5143\:7ed3\:5f0f\:51fd\:6570 ================== *)
MyMultivarResultant[polyA_, polyB_, var_] := Module[
  {vars, remVars, remStr, denA, denB, intA, intB, degA, degB, strA, strB, elimStr, resStr, factor},
  vars = Variables[{polyA, polyB}];
  remVars = DeleteCases[vars, var];
  (* \:5bb9\:9519\:ff1a\:5982\:679c\:6ca1\:6709\:5269\:4f59\:53d8\:91cf\:ff0cFLINT \:89e3\:6790\:5668\:4ecd\:9700\:8981\:81f3\:5c11\:4e00\:4e2a\:5360\:4f4d\:53d8\:91cf *)
  remStr = If[Length[remVars] === 0, "y", StringRiffle[ToString /@ remVars, ","]];
  (* \:63d0\:53d6\:5206\:6bcd\:4e0e\:6574\:7cfb\:6570\:591a\:9879\:5f0f *)
  {denA, intA} = MapAt[1/# &, FactorTermsList[polyA], {1}];
  {denB, intB} = MapAt[1/# &, FactorTermsList[polyB], {1}];
  degA = Exponent[polyA, var];
  degB = Exponent[polyB, var];
  strA = ToString[InputForm[intA]]; 
  strB = ToString[InputForm[intB]]; 
  elimStr = ToString[var]; 
  (* \:8c03\:7528 C \:5c42\:63a5\:53e3\:8ba1\:7b97\:6574\:7cfb\:6570 Resultant *)
  resStr = cMultivarResultant[strA, strB, elimStr, remStr];
  (* \:7ed3\:5f0f\:53bb\:5206\:6bcd\:56e0\:5b50\:8865\:507f *)
  factor = (denA^degB) * (denB^degA);
  ToExpression[resStr] / factor
];

(* ================== \:591a\:5143\:5224\:522b\:5f0f\:51fd\:6570 ================== *)
MyMultivarDiscriminant[polyA_, var_] := Module[
  {remVars, remStr, denA, intA, degA, strA, elimStr, resStr, factor},
  remVars = DeleteCases[Variables[polyA], var];
  remStr = If[Length[remVars] === 0, "y", StringRiffle[ToString /@ remVars, ","]];
  (* \:63d0\:53d6\:5206\:6bcd\:4e0e\:6574\:7cfb\:6570\:591a\:9879\:5f0f *)
  {denA, intA} = MapAt[1/# &, FactorTermsList[polyA], {1}];
  degA = Exponent[polyA, var];
  strA = ToString[InputForm[intA]];
  elimStr = ToString[var]; 
  (* \:8c03\:7528 C \:5c42\:63a5\:53e3\:8ba1\:7b97\:6574\:7cfb\:6570 Discriminant *)
  resStr = cMultivarDiscriminant[strA, elimStr, remStr];
  (* \:5224\:522b\:5f0f\:53bb\:5206\:6bcd\:56e0\:5b50\:8865\:507f: f(x) = p(x)/denA => Disc(f) = Disc(p) / denA^(2*degA - 2) *)
  factor = denA^(2 * degA - 2);
  ToExpression[resStr] / factor
];

(* Q(a) \:4e0a\:4e00\:5143\:591a\:9879\:5f0f f(x), g(x) \:7684\:9996\:4e00 GCD, a \:7531\:6700\:5c0f\:591a\:9879\:5f0f m(a) = 0 \:786e\:5b9a *)
MyNFPolyGCD[f_, g_, m_]:= Module[
{vars, x, a, intf, intg, strf, strg, strm, strx, stra, resStr},
vars = Variables[{f,g}];
a = First@Variables@m;
x = First@DeleteCases[vars, a];
{intf, intg} = If[PolynomialExpressionQ[#, vars, IntegerQ], 
#, FactorTermsList[#][[-1]]]&/@{f, g};
{strf, strg, strm, strx, stra} = ToString[#, InputForm]&/@{intf, intg, m, x, a};
resStr = cNFPolyGCD[strf, strg, strm, strx, stra];
ToExpression@resStr
];

(* Q(a) \:4e0a\:4e00\:5143\:591a\:9879\:5f0f f(x) \:7684\:56e0\:5f0f\:5206\:89e3, a \:7531\:6700\:5c0f\:591a\:9879\:5f0f m(a) = 0 \:786e\:5b9a *)
MyNFPolyFactor[f_, m_]:= Module[
{vars, x, a, c, intf, strf, strm, strx, stra, resStr, res},
vars = Variables[f];
a = First@Variables@m;
x = First@DeleteCases[vars, a];
{c, intf} = If[PolynomialExpressionQ[#, vars, IntegerQ], 
{1, #}, FactorTermsList[#]]&@f;
{strf, strm, strx, stra} = ToString[#, InputForm]&/@{intf, m, x, a};
resStr = cTragerFactor[strf, strm, strx, stra];
res = ToExpression@resStr;
res[[1,1]] = res[[1,1]]*c;
res
];


InitializeMathTools[];
