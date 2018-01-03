BeginPackage["FunctionalLink`"]

fln::usage = "fln[inNumber,outNumber,ioPairs,eta,alpha,numIters]"
flnTest::usage = "flnTest[outputWts,ioPairVectors]"

Begin["`Private`"]    (* begin the private context *)


fln[inNumber_,outNumber_,ioPairs_,eta_,alpha_,numIters_] :=
  Module[{outWts,ioP,inputs,outputs,outDesired, 
			outVals,outLastDelta,outDelta,outErrors},
  outVals={};
  outWts = Table[Table[Random[Real,{-0.1,0.1}],{inNumber}],{outNumber}];
  outLastDelta = Table[Table[0,{inNumber}],{outNumber}];
  errorList = Table[
					(* begin forward pass *)
	  ioP=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];
	 inputs=ioP[[1]];
	 outDesired=ioP[[2]];
	 outputs = outWts.inputs; (* output-layer outputs *)
					(* calculate errors *)
	 outErrors = outDesired-outputs;
	 outDelta= outErrors;
						(* update weights *)
 	outLastDelta= eta Outer[Times,outDelta,inputs]+alpha outLastDelta;
	 outWts += outLastDelta;
		outErrors.outErrors, (* this puts the error on the list *)
		{numIters}]	;	 (* this many times, Table ends here *)
 Print["New output-layer weight matrix: "];
 Print[]; Print[outWts];Print[];  
 outVals=flnTest[outWts,ioPairs];  
 errorPlot = ListPlot[errorList, PlotJoined->True];
 Return[{outWts,errorList,errorPlot,outVals}];
 ]						(* end of Module *)
 

flnTest[outputWts_,ioPairVectors_] :=
  Module[{inputs,hidden,outputs,desired,errors,i,len,
  			errorTotal,errorSum},
   inputs=Map[First,ioPairVectors];
   desired=Map[Last,ioPairVectors];
   len = Length[inputs];
   outputs=inputs.Transpose[outputWts];
   errors= desired-outputs;
   For[i=1,i<=len,i++,
		    (*Print["Input ",i," = ",inputs[[i]]];*)
		  Print[" Output ",i," = ",outputs[[i]]," desired = ",
			  desired[[i]]," Error = ",errors[[i]]];Print[];
		  ];			(* end of For *)
	  (*Print["errors= ",errors];Print[];*)
   errorSum = Apply[Plus,errors^2,2]; (* second level *)
   (*Print["errorSum= ",errorSum];Print[];*)
   errorTotal = Apply[Plus,errorSum];
   (*Print["errorTotal= ",errorTotal];*)
   Print["Mean Squared Error = ",errorTotal/len];
   Return[outputs];
   ]					(* end of Module *)


End[]         (* end the private context *)

EndPackage[]  (* end the package context *)