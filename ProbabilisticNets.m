BeginPackage["ProbabilisticNets`"]

normalize::usage = "normalize[x_List]"
energyHop::usage = "energyHop[x,w]"
psi::usage = "psi[inValue,netIn]"
phi::usage = "phi[inVector_List,netInVector_List]"
makeHopfieldWts::usage = "makeHopfieldWts[trainingPats,printWts:True]"
discreteHopfield::usage = "discreteHopfield[wtVector,inVector,printAll:True]"
prob::usage = "prob[n,T]"
probPsi::usage = "probPsi[inValue_,netIn_,temp_]"
stochasticHopfield::usage = "stochasticHopfield[inVector,weights,numSweeps,temp]"
pnnTwoClass::usage = "pnnTwoClass[class1Exemplars,class2Exemplars,testInputs,sig]"


Begin["`Private`"]    (* begin the private context *)

normalize[x_List] := x/(Sqrt[x.x]//N)
 

energyHop[x_,w_] := -0.5 x . w . x;
 

psi[inValue_,netIn_] := If[netIn>0,1, 
						 If[netIn<0,-1,inValue]] 
 
  
phi[inVector_List,netInVector_List] :=
	MapThread[psi[#,#2]&,{inVector,netInVector}]
 

makeHopfieldWts[trainingPats_,printWts_:True] :=
	Module[{wtVector},
	 wtVector = 
	   Apply[Plus,Map[Outer[Times,#,#]&,trainingPats]];
	 If[printWts,
	 	Print[];
	 	Print[MatrixForm[wtVector]];
	 	Print[];,Continue
	 	];  (* end of If  *)
	 Return[wtVector];
	 ] (* end of Module *)
 

discreteHopfield[wtVector_,inVector_,printAll_:True] :=
	Module[{done, energy, newEnergy, netInput, 
				newInput, output},
	 done = False;
	 newInput = inVector;
	 energy = energyHop[inVector,wtVector];
	 If[printAll,
	 	Print[];Print["Input vector = ",inVector];
	 	Print[];
	 	Print["Energy = ",energy];
	 	Print[],Continue
	 	];   (* end of If  *)
	 While[!done,
	 	netInput = wtVector . newInput;
	 	output = phi[newInput,netInput];
	 	newEnergy = energyHop[output,wtVector];
	    If[printAll,
	 	 	Print[];Print["Output vector = ",output];
	 	 	Print[];
	 	 	Print["Energy = ",newEnergy];
	 	 	Print[],Continue
	 	 	];   (* end of If  *)
	 	If[energy==newEnergy,
	 		done=True,
	 		energy=newEnergy;newInput=output,
	 		Continue
	 		];   (* end of If  *)
	 	  ];  (* end of While  *)
	 	If[!printAll, 
	 	 Print[];Print["Output vector = ",output];
	 	 Print[];
	 	 Print["Energy = ",newEnergy];
	 	 Print[];
	 	  ];  (* end of If *)
	 	 ];  (* end of Module  *) 	 
 

prob[n_,T_] := 1/(1+E^(-n/T)) //N;
 

probPsi[inValue_,netIn_,temp_] := 
 If[Random[]<=prob[netIn,temp],1,psi[inValue,netIn]];

stochasticHopfield[inVector_,weights_,numSweeps_,temp_]:= 
  Module[ {input, net, indx, numUnits, indxList, output},
    numUnits=Length[inVector];
    indxList=Table[0,{numUnits}];
    input=inVector;
	For[i=1,i<=numSweeps,i++,
	Print["i= ",i];
      For[j=1,j<=numUnits,j++,
  			(* select unit *)
	    indx = Random[Integer,{1,numUnits}]; 
			(* net input to unit *)
	    net=input . weights[[indx]]; 
			(* update input vector *)
		output=probPsi[input[[indx]],net,temp];
	    input[[indx]]=output;
	    indxList[[indx]]+=1;
      ];  (* end For numUnits *)
    Print[ ];Print["New input vector = "];Print[input];
    ];   (* end For numSweeps *)
  Print[ ];Print["Number of times each unit was updated:"];
  Print[ ];Print[indxList];
  ];   (* end of Module *)
 

pnnTwoClass[class1Exemplars_,class2Exemplars_,
							testInputs_,sig_] :=
	Module[{weightsA,weightsB,inputsNorm,patternAout,
				patternBout,sumAout,sumBout},
		weightsA = Map[normalize,class1Exemplars];
		weightsB = Map[normalize,class2Exemplars];
		inputsNorm = Map[normalize,testInputs];
		sigma = sig;
		patternAout = 
			gaussOut[inputsNorm . Transpose[weightsA]];
		patternBout = 
			gaussOut[inputsNorm . Transpose[weightsB]];
		sumAout = Map[Apply[Plus,#]&,patternAout];
		sumBout = Map[Apply[Plus,#]&,patternBout];
		outputs = Sign[sumAout-sumBout];
		sigma=.;
		Return[outputs];
		]	



End[]         (* end the private context *)

EndPackage[]  (* end the package context *)