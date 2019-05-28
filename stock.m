(* ::Package:: *)

ClearAll[GetAllSHDailyFiles,GetAllSZDailyFiles];
GetAllSZDailyFiles[]:=Module[{allfiles,SZDIRECTORY="/home/math/.wine/drive_c/new_xdzq_tyrz/vipdoc/sz/lday"},
allfiles=FileNames["*day",SZDIRECTORY];
allfiles=Select[allfiles,StringContainsQ[#,"sz00"|"sz300"]&];
allfiles
];
GetAllSHDailyFiles[]:=Module[{allfiles,SZDIRECTORY="/home/math/.wine/drive_c/new_xdzq_tyrz/vipdoc/sh/lday"},
allfiles=FileNames["*day",SZDIRECTORY];
allfiles=Select[allfiles,StringContainsQ[#,"sh600"]&];
allfiles
];


ClearAll[GetStockDayData];
GetStockDayData[stockfilepath_]:=Module[{data={}},
data=BinaryReadList[stockfilepath,{"Integer32","Integer32","Integer32","Integer32","Integer32","Real32","Integer32","Integer32"}];
data[[All,6]]=IntegerPart[data[[All,6]]];
data
];


ClearAll[MA,EMA,SMA,LLV,HHV,REF,CROSS,DMA];
MA[lst__,n_]:=Module[{len=Length[lst],fst=First[lst]},
PadLeft[N@MovingAverage[lst,n],len,fst]
];
EMA[lst__,n_]:=N@ExponentialMovingAverage[lst,2/(n+1)];
SMA[lst__,n_,m_]:=N@ExponentialMovingAverage[lst,m/n];
LLV[lst__,n_]:=Module[{len=Length[lst],fst=First[lst]},
PadLeft[(Min[#])&/@Partition[lst,n,1],len,fst]
];
HHV[lst__,n_]:=Module[{len=Length[lst],fst=First[lst]},
PadLeft[(Max[#])&/@Partition[lst,n,1],len,fst]
];
REF[lst__,n_]:=Module[{len=Length[lst],fst=First[lst]},
PadLeft[Drop[lst,-n],len,fst]
];
CROSS[lst1__,lst2__]:=Module[{res},
res=Boole[Thread[Thread[lst1>lst2]&&Thread[RotateLeft[lst1,1]<RotateLeft[lst2,1]]]];
res[[-1]]=False;
RotateRight[res,1]
];
DMA[lst_,a_]:=N@ExponentialMovingAverage[lst,a];


ClearAll[KDJ]
(*\:5217\:8868\:6570\:636e\:6309OHLC\:7684\:987a\:5e8f\:5f15\:8fdb*)
KDJ[x__,n_]:=Module[{RSV={},min=0,max=0,k={},d={},j={}},
RSV=(
min=Min[#];
max=Max[#];
If[
max==min,100,100.0((#[[-1,-1]]-min)/(max-min))
]
)&/@Partition[PadLeft[x[[All,2;;-1]],Length[x]+n-1,{x[[1,2;;-1]]}],n,1];
k=NestList[{2/3 #[[1]]+1/3 RSV[[1+#[[2]]]],#[[2]]+1}&,{50,0},Length[x]][[2;;-1,1]];
d=NestList[{2/3 #[[1]]+1/3 k[[1+#[[2]]]],#[[2]]+1}&,{50,0},Length[x]][[2;;-1,1]];
j=3k-2d;
Thread[{k,d,j}]
]
