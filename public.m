(* ::Package:: *)

ClearAll[Cyclicf,Symmetricf,Sumcyc,Sumsym];
Cyclicf[fun_,vars_]:=Module[{len=0},
len=Length[vars];
fun@@@Table[RotateLeft[vars,n],{n,0,len-1}]
];
Symmetricf[fun_,vars_]:=(fun@@@Permutations[vars]);
Listcyc[fun_,vars_]:=Cyclicf[fun,vars];
Sumcyc[fun_,vars_]:=Plus@@(Cyclicf[fun,vars]);
Sumsym[fun_,vars_]:=Plus@@(Symmetricf[fun,vars]);
Prodcyc[fun_,vars_]:=Times@@(Cyclicf[fun,vars]);
Prodsym[fun_,vars_]:=Times@@(Symmetricf[fun,vars]);

ToSumcyc[poly_]:=Module[{vars={},crules={},item=0,polytemp=poly,iter=20,varlen=0},
vars=Variables[poly];
varlen=Length[vars];
While[MonomialList[polytemp]!={0}&&iter>0,
crules=CoefficientRules[polytemp,vars];
item=item+Last@First@crules Times@@Power[vars,First@First@crules];
polytemp=polytemp-Total[Last@First@crules Times@@Power[vars,First@First@crules]/.(Thread[vars->#]&/@Table[RotateLeft[vars,n],{n,0,varlen-1}])]//Simplify;
iter--;
];
{item,polytemp}
]



(*\:65e0\:7406\:5206\:6570\:7684\:5206\:6bcd\:6709\:7406\:5316*)
ClearAll[RationalizeDenominator,
RationalizeDenominator1,
RationalizeDenominator2,
RationalizeDenominator3,
RationalizeDenominator4,
rat];
RationalizeDenominator[x_]:=Module[{y,nn,dd,f,g,c,k,blah},
(y=Together[x];
nn=Numerator[y];
dd=Denominator[y];
f=MinimalPolynomial[dd,t];
c=f/.t->0;
g=Factor[(c-f)/t];
{k,blah}=FactorTermsList[Expand[nn*(g/.t->dd)]];
Sign[c] ((k/GCD[k,c])*blah)/HoldForm[Evaluate@Abs[c/GCD[k,c]]]
)
];

RationalizeDenominator1[x_]:=FullSimplify[ToRadicals[RootReduce[x]]];

SetAttributes[RationalizeDenominator2,Listable];
RationalizeDenominator2[x_]:=Block[{den,t,len},den=Denominator[x];
If[NumberQ@den||LeafCount@x<=5,Return@x];
If[Head[den]===Plus,
len=Length[List@@Simplify[den]];
t=List@@den.#&/@Take[Tuples[{1,-1},len],2^(len-1)];
(Times@@DeleteCases[t,den]*Numerator[x])/Simplify[Times@@t]//Simplify,
Return@x
]
];

rat[p_] := If[FreeQ[Denominator[p], Power[_, Rational[_, _]]], 0, 1];
RationalizeDenominator3[x_]:=FullSimplify[x, ComplexityFunction -> rat];

RationalizeDenominator4[x_]:=x;
RationalizeDenominator4[(p_:1)*Power[a_+(b_:1)*Sqrt[v_],m_?Negative]]:=Expand[p*(a-b*Sqrt[v])^(-m)]/Expand[(a-b*Sqrt[v])*(a+b*Sqrt[v])]^(-m);
RationalizeDenominator4[(a_:1)*Power[v_,Rational[-1,2]]]:=a/v*HoldForm[Sqrt[v]];



(**)
worddef:=Style[WordDefinition[#],24,Bold,Red]&


ClearAll[exp,cotsum,Estermann];
exp[x_,m_]:=Exp[2\[Pi] I x/m];
exp[m_][x_]:=Exp[2\[Pi] I x/m];
cotsum[a_][h_,k_]:=k^a Sum[Cot[\[Pi] h m/k]Zeta[-a,m/k],{m,1,k-1}];
Estermann[0,a_,h_,k_]:=-1/2Zeta[-a]+I/2c[a][h,k];
Estermann[s_,a_,h_,k_]:=Sum[DivisorSigma[a,n]e[n h,k]/n^s,{n,1,\[Infinity]}];
