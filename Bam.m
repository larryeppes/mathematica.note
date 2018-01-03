BeginPackage["Bam`"]

makeXtoYwts::usage = "makeXtoYwts[exemplars]"
psi::usage = "psi[inValue,netIn]"
phi::usage = "phi[inVector_List,netInVector_List]"
energyBAM::usage = "energyBAM[xx,w,zz]"
bam::usage = "bam[initialX,initialY,x2yWeights,y2xWeights,printAll:False]"

Begin["`Private`"]    (* begin the private context *)


makeXtoYwts[exemplars_] :=
   Module[{temp},
    temp = Map[Outer[Times,#[[2]],#[[1]]]&,exemplars];
    Apply[Plus,temp]
    ];  (* end of Module *)
 

 

psi[inValue_,netIn_] := If[netIn>0,1,If[netIn<0,-1,inValue]];
phi[inVector_List,netInVector_List] :=
	MapThread[psi[#,#2]&,{inVector,netInVector}]; 

 
energyBAM[xx_,w_,zz_] := - (xx . w . zz) 


 
bam[initialX_,initialY_,x2yWeights_,y2xWeights_,printAll_:False] :=
  Module[{done,newX,newY,energy1,energy2},
    done = False;
    newX = initialX;
    newY = initialY;
    While[done == False,
	  newY = phi[newY,x2yWeights.newX];
	  If[printAll,Print[];Print[];Print["y = ",newY]];
	  energy1 = energyBAM[newY,x2yWeights,newX];
	  If[printAll,Print["energy = ",energy1]];
	  newX =  phi[newX,y2xWeights . newY];
	  If[printAll,Print[];Print["x = ",newX]];
	  energy2 =  energyBAM[newY,x2yWeights,newX];
	  If[printAll,Print["energy = ",energy1]];
	  If[energy1 == energy2,done=True,Continue];
	]; (* end of While *)
	Print[];Print[];
	Print["final y = ",newY,"  energy= ",energy1];
	Print["final x = ",newX,"  energy= ",energy2];
  ];  (* end of Module *)


End[]         (* end the private context *)

EndPackage[]  (* end the package context *)