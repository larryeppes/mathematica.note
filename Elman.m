BeginPackage["Elman`"]


elman::usage = "elman[inNumber,hidNumber,outNumber,ioPairs,eta,alpha,numIters]"
elmanTest::usage = "elmanTest[hiddenWts,outputWts,ioPairVectors,conNumber,printAll:False]"
elmanComp::usage = "elmanComp[inNumber,hidNumber,outNumber,ioPairs,eta,alpha,numIters]"
elmanCompTest::usage = "elmanCompTest[hiddenWts,outputWts,ioPairVectors,conNumber,printAll:False]"


Begin["`Private`"]    (* begin the private context *)

elman[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,alpha_,numIters_] :=
  Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
			i,indx,hidLastDelta,outLastDelta,outDelta,errorList={},
			ioSequence, conUnits,hidDelta,outErrors},
  hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber+hidNumber}],{hidNumber}];
  outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
  hidLastDelta = Table[Table[0,{inNumber+hidNumber}],{hidNumber}];
  outLastDelta = Table[Table[0,{hidNumber}],{outNumber}];
  For[indx=1,indx<=numIters,indx++,  (* begin forward pass; select a sequence  *)
	ioSequence=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];
	conUnits = Table[0.5,{hidNumber}];     (* reset conUnits  *)
	For[i=1,i<=Length[ioSequence],i++,      (* process the sequence in order *)
   ioP = ioSequence[[i]];                (* pick out the next ioPair *)
   inputs=Join[conUnits,ioP[[1]] ];      (* join context and input units *)
	  outDesired=ioP[[2]];
	  hidOuts = sigmoid[hidWts.inputs];     (* hidden-layer outputs *)
	  outputs = sigmoid[outWts.hidOuts];    (* output-layer outputs *)
	  outErrors = outDesired-outputs;       (* calculate errors *)
	  outDelta= outErrors (outputs (1-outputs));
	  hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
	  outLastDelta= eta Outer[Times,outDelta,hidOuts]+alpha outLastDelta;
	  outWts += outLastDelta;               (* update weights *)
	  hidLastDelta = eta Outer[Times,hidDelta,inputs]+alpha hidLastDelta;
	  hidWts += hidLastDelta;
	  conUnits = hidOuts;                   (* update context units *)
		 (* put the sum of the squared errors on the list *)
	  AppendTo[errorList,outErrors.outErrors];
	  ]; (* end of For i *)
  ];     (* end of For indx *)
  Print["New hidden-layer weight matrix: "];
  Print[ ]; Print[hidWts];Print[ ];
  Print["New output-layer weight matrix: "];
  Print[ ]; Print[outWts];Print[ ];  
  elmanTest[hidWts,outWts,ioPairs,hidNumber];  
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
  ]						(* end of Module *)
 
 

elmanTest[hiddenWts_,outputWts_,ioPairVectors_,conNumber_,printAll_:False] :=
  Module[{inputs,hidden,outputs,desired,errors,i,j,
  			prntAll,conUnits,ioSequence,ioP},
    If[printAll,Print[];Print["ioPairs:"];Print[];Print[ioPairVectors]];
     			(* loop through the sequences  *)
	   For[i=1,i<=Length[ioPairVectors],i++,
     			(* select the next sequence *)
     ioSequence = ioPairVectors[[i]];
     			(* reset the context units  *)
     conUnits = Table[0.5,{conNumber}];
     			(* loop through the chosen sequence *)
     	For[j=1,j<=Length[ioSequence],j++,
  			ioP = ioSequence[[j]];
					(* join context and input units *)
	  		inputs=Join[conUnits,ioP[[1]] ];
   			desired=ioP[[2]];
   			hidden=sigmoid[hiddenWts.inputs];
   			outputs=sigmoid[outputWts.hidden];
   			errors= desired-outputs;
   				(* update context units *)
   			conUnits = hidden;
   			Print[];
   			Print["Sequence ",i, " input ",j];
   			Print[];Print["inputs:"];Print[];
   			Print[inputs];
   			If[printAll,Print[];Print["hidden-layer outputs:"];
   				Print[hidden];Print[];
   				];
   			Print["outputs:"];Print[];
   			Print[outputs];Print[];
   			Print["desired:"];Print[];Print[desired];Print[];
   			Print["Mean squared error:"];
   			Print[errors.errors/Length[errors]];
   			Print[];
   			];  (* end of For j *)
   	 	];	(* end of For i *)
    ]					(* end of Module *)
 
 
elmanComp[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,alpha_,numIters_] :=
  Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
			i,indx,hidLastDelta,outLastDelta,outDelta,errorList={}, 
			ioSequence, conUnits,hidDelta,outErrors},
  hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber+conNumber}],{hidNumber}];
  outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
  hidLastDelta = Table[Table[0,{inNumber+conNumber}],{hidNumber}];
  outLastDelta = Table[Table[0,{hidNumber}],{outNumber}];
  outErrors = Table[0,{outNumber}];
  For[indx=1,indx<=numIters,indx++,
	ioSequence=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]]; (* select a sequence  *)
	conUnits = Table[0.5,{conNumber}];       (* reset conUnits  *)
	For[i=1,i<=Length[ioSequence],i++,       (* process the sequence in order *)
	  ioP = ioSequence[[i]];                 (* pick out the next ioPair *)
	  inputs=Join[conUnits,ioP[[1]] ];       (* join context and input units *)
	  outDesired=ioP[[2]];
	  hidOuts = sigmoid[hidWts.inputs];      (* hidden-layer outputs *)
	  outputs = outWts.hidOuts;              (* output-layer outputs *)
	  outputs = sigmoid[outputs -	0.3 Apply[Plus,outputs] + .5 outputs]; 
	  outErrors = outDesired-outputs;        (* calculate errors *)
	  outDelta= outErrors (outputs (1-outputs));
	  hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
	  outLastDelta= eta Outer[Times,outDelta,hidOuts]+alpha outLastDelta;
	  outWts += outLastDelta;                 (* update weights *)
	  hidLastDelta = eta Outer[Times,hidDelta,inputs]+alpha hidLastDelta;
	  hidWts += hidLastDelta;                (* update weights *)
	  conUnits = hidOuts;                    (* update context units *)
		 (* put the sum of the squared errors on the list *)
	  AppendTo[errorList,outErrors.outErrors];
	  ]; (* end of For i *)
  ];     (* end of For indx *)
  Print["New hidden-layer weight matrix: "];
  Print[ ]; Print[hidWts];Print[ ];
  Print["New output-layer weight matrix: "];
  Print[ ]; Print[outWts];Print[ ];  
  elmanCompTest[hidWts,outWts,ioPairs,conNumber]; 
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
  ]						(* end of Module *)
 

elmanCompTest[hiddenWts_,outputWts_,ioPairVectors_,conNumber_,printAll_:False] :=
  Module[{inputs,hidden,outputs,desired,errors,i,j,prntAll,conUnits,ioSequence,ioP},
    If[printAll,Print[];Print["ioPairs:"];Print[];Print[ioPairVectors]];
   	For[i=1,i<=Length[ioPairVectors],i++,   (* loop through the sequences  *)
     ioSequence = ioPairVectors[[i]];       (* select the next sequence *)
     conUnits = Table[0.5,{conNumber}];     (* reset the context units  *)
     	For[j=1,j<=Length[ioSequence],j++,    (* loop through the chosen sequence *)
  			ioP = ioSequence[[j]];
	  		inputs=Join[conUnits,ioP[[1]] ];       (* join context and input units *)
   			desired=ioP[[2]];
   			hidden=sigmoid[hiddenWts.inputs];
   			outputs=outputWts.hidden;
   			outputs=sigmoid[outputs - 
   				0.3 Apply[Plus,outputs] + 0.5 outputs];
   			errors= desired-outputs;
   				(* update context units *)
   			conUnits = hidden;
   			Print[];
   			Print["Sequence ",i, " input ",j];
   			Print[];Print["inputs:"];Print[];
   			Print[inputs];
   			If[printAll,Print[];Print["hidden-layer outputs:"];
   				Print[hidden];Print[];
   				];
   			Print["outputs:"];Print[];
   			Print[outputs];Print[];
   			Print["desired:"];Print[];Print[desired];Print[];
   			Print["Mean squared error:"];
   			Print[errors.errors/Length[errors]];
   			Print[];
   			];  (* end of For j *)
   	 	];	(* end of For i *)
    ]					(* end of Module *)



End[]         (* end the private context *)

EndPackage[]  (* end the package context *)