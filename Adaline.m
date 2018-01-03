BeginPackage["Adaline`"]

alcTest::usage = "alcTest[learnRate,numIters:250]"
calcMsc::usage = "calcMse[ioPairs,wtVec]"
alcXor::usage = "alcXor[learnRate,numInputs,ioPairs,numIters:250]"
testXor::usage = "testXor[ioPairs,weights]"
alcXorMin::usage = "alcXorMin[learnRate,numInputs,ioPairs,maxError]"

Begin["`Private`"]    (* begin the private context *)


alcTest[learnRate_,numIters_:250] := 
Module[{eta=learnRate,wts,k,inputs,wtList,outDesired,outputs,outError},
  wts = Table[Random[],{2}];	(* initialize weights *)
  Print["Starting weights = ",wts];
  Print["Learning rate = ",eta];
  Print["Number of iterations = ",numIters];
  inputs = {0,Random[Real,{0, 0.175}]};(* initialize input vector *)
  k=1;
  wtList=Table[
	inputs[[1]] = N[Sin[Pi k/8]]+Random[Real,{0, 0.175}];
	outDesired = N[2 Cos[Pi k/8]]; (* desired output *)
	outputs = wts.inputs;	(* actual output *)
	outError = outDesired-outputs; (* error *)
	wts += eta outError inputs; (* update weights *)
	inputs[[2]]=inputs[[1]]; (* shift input values *)
	k++;	wts,{numIters}];							(* end of Table *)
 Print["Final weight vector = ",wts];
 wtPlot=ListPlot[wtList,PlotJoined->True] (* plot the weights *)		
 ]		(* end of Module *)		
 
 
calcMse[ioPairs_,wtVec_] := 
	Module[{errors,inputs,outDesired,outputs},
		inputs = Map[First,ioPairs]; (* extract inputs *)
		outDesired = Map[Last,ioPairs];  (* extract desired outputs *)
		outputs = inputs . wtVec;  (* calculate actual outputs *)
		errors = Flatten[outDesired-outputs];
		Return[errors.errors/Length[ioPairs]]
		]
 
alcXor[learnRate_,numInputs_,ioPairs_,numIters_:250] :=
  Module[{wts,eta=learnRate,errorList,inputs,outDesired,ourError,outputs},
	SeedRandom[6460];		(* seed random number gen.*)
	wts = Table[Random[],{numInputs}];	(* initialize weights *)
	errorList=Table[      (* select ioPair at random *)
		{inputs,outDesired} = ioPairs[[Random[Integer,{1,4}]]];
		outputs = wts.inputs;	(* actual output *)
		outError = First[outDesired-outputs]; (* error *)
		wts += eta outError inputs; 
		outError,{numIters}];	(* end of Table *)
	ListPlot[errorList,PlotJoined->True];
	Return[wts];
	];  (* end of Module *)
 

testXor[ioPairs_,weights_] :=
	Module[{errors,inputs,outDesired,outputs,wts,mse},
		inputs = Map[First,ioPairs]; (* extract inputs *)
		outDesired = Map[Last,ioPairs];  (* extract desired outputs *)
		outputs = inputs . weights;  (* calculate actual outputs *)
		errors = outDesired-outputs;
		mse=
		  Flatten[errors] . Flatten[errors]/Length[ioPairs];
		Print["Inputs = ",inputs];
		Print["Outputs = ",outputs];
		Print["Errors = ",errors];
		Print["Mean squared error = ",mse]
		]		
 

alcXorMin[learnRate_,numInputs_,ioPairs_,maxError_] :=
  Module[{wts,eta=learnRate,errorList,inputs,outDesired,
  			meanSqError,done,k,ourError,outputs,errorPlot},
	wts = Table[Random[],{numInputs}];	(* initialize weights *)
	meanSqError = 0.0;
	errorList={};	
	For[k=1;done=False,!done,k++,	(* until done *)
							(* select ioPair at random *)
	  {inputs,outDesired} = ioPairs[[Random[Integer,{1,4}]]];
	  outputs = wts.inputs;	(* actual output *)
	  outError = First[outDesired-outputs]; (* error *)
	  wts += eta outError inputs; (* update weights *)
	  If[Mod[k,4]==0,meanSqError=calcMse[ioPairs,wts];
		  AppendTo[errorList, meanSqError];	];						
	  If[k>4 && meanSqError<maxError,done=True,Continue];	(* test for done *)
		];							(* end of For *)
	errorPlot=ListPlot[errorList,PlotJoined->True];
	Return[wts];
	] (* end of Module *)							



End[]         (* end the private context *)

EndPackage[]  (* end the package context *)