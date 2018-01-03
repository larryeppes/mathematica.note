BeginPackage["Jordan`"]

jordan::usage = "jordan[inNumber,hidNumber,outNumber,ioPairs,eta,alpha,mu,numIters]"
jordanTest::usage = "jordanTest[hiddenWts,outputWts,ioPairVectors,
			mu, stateNumber,printAll:False]"
jordan2::usage = "jordan2[inNumber,hidNumber,outNumber,ioPairs,eta,alpha,mu,numIters]"
jordan2Test::usage = "jordan2Test[hiddenWts,outputWts,ioPairVectors,
			mu, stateNumber,printAll:False]"
jordan2a::usage = "jordan2a[inNumber_,hidNumber,outNumber,ioPairs,eta,alpha,mu,numIters]"
jordan2aTanh::usage = "jordan2aTanh[inNumber,hidNumber,outNumber,ioPairs,eta,alpha,mu,numIters]"
jordan2aTanhTest::usage = "jordan2aTanhTest[hiddenWts,outputWts,ioPairVectors,
			mu, stateNumber,printAll:False]"


Begin["`Private`"]    (* begin the private context *)

jordan[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,alpha_,mu_,numIters_] :=
  Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
			i,indx,hidLastDelta,outLastDelta,outDelta,errorList = {}, 
			ioSequence, stateUnits,hidDelta,outErrors},
  hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber+outNumber}],{hidNumber}];
  outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
  hidLastDelta = Table[Table[0,{inNumber+outNumber}],{hidNumber}];
  outLastDelta = Table[Table[0,{hidNumber}],{outNumber}];
  For[indx=1,indx<=numIters,indx++, (* begin forward pass *)
	ioSequence=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]]; (* select a sequence  *)
	stateUnits = Table[0.1,{outNumber}];   (* reset stateUnits  *)
	For[i=1,i<=Length[ioSequence],i++,     (* process the sequence in order *)
	  ioP = ioSequence[[i]];               (* pick out the next ioPair *)
	  inputs=Join[stateUnits,ioP[[1]] ];   (* join context and input units *)
	  outDesired=ioP[[2]];
	  hidOuts = sigmoid[hidWts.inputs];    (* hidden-layer outputs *)
	  outputs = sigmoid[outWts.hidOuts];   (* output-layer outputs *)
	  outErrors = outDesired-outputs;      (* calculate errors *)
	  outDelta= outErrors (outputs (1-outputs));
	  hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
	  outLastDelta= eta Outer[Times,outDelta,hidOuts]+alpha outLastDelta;
	  outWts += outLastDelta;              (* update weights *)
	  hidLastDelta = eta Outer[Times,hidDelta,inputs]+alpha hidLastDelta;
	  hidWts += hidLastDelta;              (* update weights *)
	  stateUnits = mu stateUnits + outputs; (* update state units *)
		 (* put the sum of the squared errors on the list *)
	  AppendTo[errorList,outErrors.outErrors];
	  ]; (* end of For i *)
  ];     (* end of For indx *)
  Print["New hidden-layer weight matrix: "];
  Print[]; Print[hidWts];Print[];
  Print["New output-layer weight matrix: "];
  Print[]; Print[outWts];Print[];  
  jordanTest[hidWts,outWts,ioPairs,mu,outNumber]; 
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
  ]						(* end of Module *)
 

jordanTest[hiddenWts_,outputWts_,ioPairVectors_,
			mu_, stateNumber_,printAll_:False] :=
  Module[{inputs,hidden,outputs,desired,errors,i,j,
  			prntAll,stateUnits,ioSequence,ioP},
    If[printAll,Print[];Print["ioPairs:"];Print[];Print[ioPairVectors]];
   	For[i=1,i<=Length[ioPairVectors],i++, (* loop through the sequences  *)
     ioSequence = ioPairVectors[[i]];    (* select the next sequence *)
     stateUnits = Table[0.1,{stateNumber}];  (* reset the context units  *)
     	For[j=1,j<=Length[ioSequence],j++,  (* loop through the chosen sequence *)
  			 ioP = ioSequence[[j]];
		 		 inputs=Join[stateUnits,ioP[[1]] ];   (* join context and input units *)
   			desired=ioP[[2]];
   			hidden=sigmoid[hiddenWts.inputs];
   			outputs=sigmoid[outputWts.hidden];
   			errors= desired-outputs;
   			stateUnits = mu stateUnits + outputs; (* update context units *)
   			Print[];
   			Print["Sequence ",i, " input ",j];
   			Print[];Print["inputs:"];Print[];
   			Print[inputs];
   			If[printAll,Print[];Print["hidden-layer outputs:"];
   				Print[hidden];Print[];  ];
   			Print["outputs:"];Print[];
   			Print[outputs];Print[];
   			Print["desired:"];Print[];Print[desired];Print[];
   			Print["Mean squared error:"];
   			Print[errors.errors/Length[errors]];
   			Print[];
   			];  (* end of For j *)
   	 	];	(* end of For i *)
    ]					(* end of Module *)
 
 
 
(* this version sets the state units equal to the desired output values, 
rather than the actual output values, during the training process  *)
jordan2[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,alpha_,mu_,numIters_] :=
  Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
			i,indx,hidLastDelta,outLastDelta,outDelta,errorList = {}, 
			ioSequence, stateUnits,hidDelta,outErrors},
  hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber+outNumber}],{hidNumber}];
  outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
  hidLastDelta = Table[Table[0,{inNumber+outNumber}],{hidNumber}];
  outLastDelta = Table[Table[0,{hidNumber}],{outNumber}];
  For[indx=1,indx<=numIters,indx++,    (* begin forward pass *)
	ioSequence=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]]; (* select a sequence  *)
	stateUnits = Table[0.1,{outNumber}];   (* reset stateUnits  *)
	For[i=1,i<=Length[ioSequence],i++,     (* process the sequence in order *)
	  ioP = ioSequence[[i]];               (* pick out the next ioPair *)
	  inputs=Join[stateUnits,ioP[[1]] ];   (* join context and input units *)
	  outDesired=ioP[[2]];
	  hidOuts = sigmoid[hidWts.inputs];    (* hidden-layer outputs *)
	  outputs = sigmoid[outWts.hidOuts];   (* output-layer outputs *)
	  outErrors = outDesired-outputs;      (* calculate errors *)
	  outDelta= outErrors (outputs (1-outputs));
	  hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
	  outLastDelta= eta Outer[Times,outDelta,hidOuts]+alpha outLastDelta;
	  outWts += outLastDelta;           (* update weights *)
	  hidLastDelta = eta Outer[Times,hidDelta,inputs]+alpha hidLastDelta;
	  hidWts += hidLastDelta;           (* update weights *)
	  stateUnits = mu stateUnits + outDesired;  (* update state units *)
	  AppendTo[errorList,outErrors.outErrors];
	  ]; (* end of For i *)
  ];     (* end of For indx *)
  Print["New hidden-layer weight matrix: "];
  Print[]; Print[hidWts];Print[];
  Print["New output-layer weight matrix: "];
  Print[]; Print[outWts];Print[];  
  jordan2Test[hidWts,outWts,ioPairs,mu,outNumber];  
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
  ]						(* end of Module *)
  
 
jordan2Test[hiddenWts_,outputWts_,ioPairVectors_,
			mu_, stateNumber_,printAll_:False] :=
  Module[{inputs,hidden,outputs,desired,errors,i,j,
  			prntAll,stateUnits,ioSequence,ioP},
    If[printAll,Print[];Print["ioPairs:"];Print[];
     				Print[ioPairVectors]];
	For[i=1,i<=Length[ioPairVectors],i++,     (* loop through the sequences  *)
     ioSequence = ioPairVectors[[i]];      (* select the next sequence *)
     stateUnits = Table[0.1,{stateNumber}]; (* reset the context units  *)
     	For[j=1,j<=Length[ioSequence],j++,   (* loop through the chosen sequence *)
  			ioP = ioSequence[[j]];
	  		inputs=Join[stateUnits,ioP[[1]] ];    (* join context and input units *)
   			desired=ioP[[2]];
   			hidden=sigmoid[hiddenWts.inputs];
   			outputs=sigmoid[outputWts.hidden];
   			errors= desired-outputs;
   			stateUnits = mu stateUnits + desired; (* update context units *)
   			Print[];
   			Print["Sequence ",i, " input ",j];
   			Print[];Print["inputs:"];Print[];
   			Print[inputs];
   			If[printAll,Print[];Print["hidden-layer outputs:"];
   				Print[hidden];Print[];];
   			Print["outputs:"];Print[];
   			Print[outputs];Print[];
   			Print["desired:"];Print[];Print[desired];Print[];
   			Print["Mean squared error:"];
   			Print[errors.errors/Length[errors]];
   			Print[];
   			];  (* end of For j *)
   	 	];	(* end of For i *)
    ]					(* end of Module *)
 

 
(* this is a modification of jordan2 in which the mean squared error 
is calculated over the entire training pass before being added to the list  *)
jordan2a[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,alpha_,mu_,numIters_] :=
  Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
			i,indx,hidLastDelta,outLastDelta,outDelta,errorList = {},
	cycleError,ioSequence, stateUnits,hidDelta,outErrors},
  hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber+outNumber}],{hidNumber}];
  outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
  hidLastDelta = Table[Table[0,{inNumber+outNumber}],{hidNumber}];
  outLastDelta = Table[Table[0,{hidNumber}],{outNumber}];
  For[indx=1,indx<=numIters,indx++,       (* begin forward pass *)
	ioSequence=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];(* select a sequence  *)
	stateUnits = Table[0.1,{outNumber}];      (* reset stateUnits  *)
	cycleError = 0.0;                         (* initialize error  *)
	For[i=1,i<=Length[ioSequence],i++,        (* process the sequence in order *)
	  ioP = ioSequence[[i]];                  (* pick out the next ioPair *)
	  inputs=Join[stateUnits,ioP[[1]] ];      (* join context and input units *)
	  outDesired=ioP[[2]];  
	  hidOuts = sigmoid[hidWts.inputs];       (* hidden-layer outputs *)
	  outputs = sigmoid[outWts.hidOuts];      (* output-layer outputs *)
	  outErrors = outDesired-outputs;         (* calculate errors *)
	  outDelta= outErrors (outputs (1-outputs));
	  hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
	  outLastDelta= eta Outer[Times,outDelta,hidOuts]+alpha outLastDelta;
	  outWts += outLastDelta;                  (* update weights *)
	  hidLastDelta = eta Outer[Times,hidDelta,inputs]+alpha hidLastDelta;
	  hidWts += hidLastDelta;                  (* update weights *)
	  stateUnits = mu stateUnits + outDesired; (* update state units *)
	  			                                     	(* compute mse for this sequence *)
	  cycleError=cycleError + outErrors.outErrors/Length[outErrors];
	  ]; (* end of For i *)
  AppendTo[errorList,cycleError/Length[ioSequence]];
  ];     (* end of For indx *)
  Print["New hidden-layer weight matrix: "];Print[ ]; Print[hidWts];Print[ ];
  Print["New output-layer weight matrix: "];Print[ ]; Print[outWts];Print[ ];  
  jordan2Test[hidWts,outWts,ioPairs,mu,outNumber]; 
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
  ]						(* end of Module *)
 
 
(* this is a modification of jordan2a using the Tanh function  *)
jordan2aTanh[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,alpha_,mu_,numIters_] :=
  Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
			i,indx,hidLastDelta,outLastDelta,outDelta,errorList = {}, 
	cycleError,ioSequence, stateUnits,hidDelta,outErrors},
  hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber+outNumber}],{hidNumber}];
  outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
  hidLastDelta = Table[Table[0,{inNumber+outNumber}],{hidNumber}];
  outLastDelta = Table[Table[0,{hidNumber}],{outNumber}];
  For[indx=1,indx<=numIters,indx++,            (* begin forward pass *)
	ioSequence=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];(* select a sequence *)
	stateUnits = Table[-0.9,{outNumber}];         (* reset stateUnits  *)
	cycleError = 0.0;                             (* initialize error  *)
	For[i=1,i<=Length[ioSequence],i++,            (* process the sequence in order *)
	  ioP = ioSequence[[i]];                      (* pick out the next ioPair *)
	  inputs=Join[stateUnits,ioP[[1]] ];          (* join context and input units *)
	  outDesired=ioP[[2]];
	  hidOuts = Tanh[hidWts.inputs];              (* hidden-layer outputs *)
	  outputs = Tanh[outWts.hidOuts];             (* output-layer outputs *)
	  outErrors = outDesired-outputs;             (* calculate errors *)
	  outDelta= outErrors  (1-outputs^2);
	  hidDelta=(1-hidOuts^2) Transpose[outWts].outDelta;
	  outLastDelta= eta Outer[Times,outDelta,hidOuts]+alpha outLastDelta;
	  outWts += outLastDelta;                     (* update weights *)
	  hidLastDelta = eta Outer[Times,hidDelta,inputs]+alpha hidLastDelta;
	  hidWts += hidLastDelta;                     (* update weights *)
	  stateUnits = mu stateUnits + outDesired;    (* update state units *)
	  cycleError=cycleError + outErrors.outErrors/Length[outErrors];
	  ]; (* end of For i *)
  AppendTo[errorList,cycleError/Length[ioSequence]];(* put the average mse on the list *)
  ];     (* end of For indx *)
  Print["New hidden-layer weight matrix: "];
  Print[]; Print[hidWts];Print[];
  Print["New output-layer weight matrix: "];
  Print[]; Print[outWts];Print[];  
  jordan2aTanhTest[hidWts,outWts,ioPairs,mu,outNumber]; 
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
  ]						(* end of Module *)
 
  
jordan2aTanhTest[hiddenWts_,outputWts_,ioPairVectors_,
			mu_, stateNumber_,printAll_:False] :=
  Module[{inputs,hidden,outputs,desired,errors,i,j,
  			prntAll,stateUnits,ioSequence,ioP},
    If[printAll,Print[];Print["ioPairs:"];Print[];	Print[ioPairVectors]];
    For[i=1,i<=Length[ioPairVectors],i++,         (* loop through the sequences  *)
     ioSequence = ioPairVectors[[i]];             (* select the next sequence *)
     	For[j=1,j<=Length[ioSequence],j++,          (* loop through the chosen sequence *)
  			ioP = ioSequence[[j]];
	  		inputs=Join[stateUnits,ioP[[1]] ];           (* join context and input units *)
   			desired=ioP[[2]];
   			hidden=Tanh[hiddenWts.inputs];
   			outputs=Tanh[outputWts.hidden];
   			errors= desired-outputs;
   			stateUnits = mu stateUnits + desired;       (* update context units *)
   			Print[];
   			Print["Sequence ",i, " input ",j];
   			Print[];Print["inputs:"];Print[];
   			Print[inputs];
   			If[printAll,Print[];Print["hidden-layer outputs:"];
   				Print[hidden];Print[];   ];
   			Print["outputs:"];Print[];
   			Print[outputs];Print[];
   			Print["desired:"];Print[];Print[desired];Print[];
   			Print["Mean squared error:"];
   			Print[errors.errors/Length[errors]];Print[];
   			];  (* end of For j *)
   	 	];	(* end of For i *)
    ]					(* end of Module *)



End[]         (* end the private context *)

EndPackage[]  (* end the package context *)