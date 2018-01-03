BeginPackage["Backpropagation`"]

sigmoid::usage = "sigmoid[x,opts___Rule]"
bpnTest::usage = "bpnTest[hiddenWts,outputWts,ioPairVectors,opts___Rule]"
bpnStandard::usage = "bpnStandard[inNumber, hidNumber, outNumber,ioPairs, eta, numIters]"
bpnBias::usage = "bpnBias[inNumber, hidNumber, outNumber,ioPairs, eta, numIters]"
bpnMomentum::usage = "bpnMomentum[inNumber,hidNumber,outNumber,ioPairs,eta,
     alpha,numIters]"
bpnMomentumSmart::usage = "bpnMomentumSmart[inNumber,hidNumber,outNumber,ioPairs,eta,
                                alpha,numIters]"
bpnCompete::usage = "bpnCompete[inNumber,hidNumber,outNumber,ioPairs,eta,numIters]"

Options[sigmoid] = {xShift->0,yShift->0,temperature->1};
Options[bpnTest] = {printAll->False,bias->False}; 

Begin["`Private`"]    (* begin the private context *)

sigmoid[x_,opts___Rule] :=
        Module[{xshft,yshft,temp},
                xshft = xShift /. {opts} /. Options[sigmoid];
                yshft = yShift /. {opts} /. Options[sigmoid];
                temp = temperature /. {opts} /. Options[sigmoid];
                yshft+1/(1+E^(-(x-xshft)/temp)) //N
                ]
 
bpnTest[hiddenWts_,outputWts_,ioPairVectors_,opts___Rule] :=
  Module[{inputs,hidden,outputs,desired,errors,i,len,
     prntAll,errorTotal,errorSum,biasVal},
   prntAll= printAll /. {opts} /. Options[bpnTest];
   biasVal = bias /. {opts} /. Options[bpnTest];
   inputs=Map[First,ioPairVectors];
   len=Length[inputs];
   If[biasVal,inputs=Map[Append[#,1.0]&,inputs] ];
   desired=Map[Last,ioPairVectors];
   hidden=sigmoid[inputs.Transpose[hiddenWts]];
   If[biasVal,hidden = Map[Append[#,1.0]&,hidden] ];
   outputs=sigmoid[hidden.Transpose[outputWts]];
   errors= desired-outputs;
   If[prntAll,Print["ioPairs:"];Print[ ];Print[ioPairVectors];
                                Print[ ];Print["inputs:"];Print[ ];Print[inputs];
                                Print[ ];Print["hidden-layer outputs:"];
                                Print[hidden];Print[ ];
                                Print["output-layer outputs:"];Print[ ];
                                Print[outputs];Print[ ];Print["errors:"];
                                Print[errors];Print[ ];  ]; (* end of If *)
   For[i=1,i<=len,i++,Print[" Output ",i," = ",outputs[[i]]," desired = ",
                   desired[[i]]," Error = ",errors[[i]]];Print[];  ];                   (* end of For *)
   errorSum = Apply[Plus,errors^2,2]; (* second level *)
   errorTotal = Apply[Plus,errorSum];
   Print["Mean Squared Error = ",errorTotal/len];
   ]                                    (* end of Module *)
 

bpnStandard[inNumber_, hidNumber_, outNumber_,ioPairs_, eta_, numIters_] :=
  Module[{errors,hidWts,outWts,ioP,inputs,outDesired,hidOuts,
                        outputs, outErrors,outDelta,hidDelta},
        hidWts = Table[Table[Random[Real,{-0.1,0.1}],{inNumber}],{hidNumber}];
        outWts = Table[Table[Random[Real,{-0.1,0.1}],{hidNumber}],{outNumber}];
        errors = Table[
                        (* select ioPair *)                                                                             
          ioP=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];
          inputs=ioP[[1]];
          outDesired=ioP[[2]];
                        (* forward pass *)
          hidOuts = sigmoid[hidWts.inputs];
          outputs = sigmoid[outWts.hidOuts];
                        (* determine errors and deltas *)
          outErrors = outDesired-outputs;
          outDelta= outErrors (outputs (1-outputs));
          hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
                        (* update weights *)
          outWts += eta Outer[Times,outDelta,hidOuts];
          hidWts += eta Outer[Times,hidDelta,inputs];
                        (* add squared error to Table *)
          outErrors.outErrors,{numIters}];  (* end of Table *)
        Return[{hidWts,outWts,errors}];
        ];                                                                      (* end of Module *)

 

bpnBias[inNumber_, hidNumber_, outNumber_,ioPairs_, eta_, numIters_] :=
  Module[{errors,hidWts,outWts,ioP,inputs,outDesired,hidOuts,
                        outputs, outErrors,outDelta,hidDelta},
        hidWts = Table[Table[Random[Real,{-0.1,0.1}],{inNumber+1}],{hidNumber}];
        outWts = Table[Table[Random[Real,{-0.1,0.1}],{hidNumber+1}], {outNumber}];
        errorList = Table[
                        (* select ioPair *)                                                                             
          ioP=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];
          inputs=Append[ioP[[1]],1.0]; (* bias mod *)
          outDesired=ioP[[2]];
                        (* forward pass *)
          hidOuts = sigmoid[hidWts.inputs];
          outInputs = Append[hidOuts,1.0];  (* bias mod *)
          outputs = sigmoid[outWts.outInputs];
                        (* determine errors and deltas *)
          outErrors = outDesired-outputs;
          outDelta= outErrors (outputs (1-outputs));
          hidDelta=(outInputs (1-outInputs)) * Transpose[outWts].outDelta;
                        (* update weights *)
          outWts += eta Outer[Times,outDelta,outInputs];
          hidWts += eta Drop[Outer[Times,hidDelta,inputs],-1];  (* bias mod *)
                        (* add squared error to Table *)
          outErrors.outErrors,{numIters}];  (* end of Table *)
          Print["New hidden-layer weight matrix: "];
  Print[]; Print[hidWts];Print[];
  Print["New output-layer weight matrix: "];
  Print[]; Print[outWts];Print[];  
  bpnTest[hidWts,outWts,ioPairs];   (* check how close we are *)
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
        ];                                                                      (* end of Module *)
 

bpnMomentum[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,
     alpha_,numIters_] :=
  Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
    hidLastDelta,outLastDelta,outDelta,hidDelta,outErrors},
    hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber}],{hidNumber}];
    outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
    hidLastDelta = Table[Table[0,{inNumber}],{hidNumber}];
    outLastDelta = Table[Table[0,{hidNumber}],{outNumber}];
    errorList = Table[
                                                 (* begin forward pass *)
             ioP=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];
             inputs=ioP[[1]];
        outDesired=ioP[[2]];
             hidOuts = sigmoid[hidWts.inputs];  (* hidden-layer outputs *)
             outputs = sigmoid[outWts.hidOuts]; (* output-layer outputs *)
                                                (* calculate errors *)
             outErrors = outDesired-outputs;
             outDelta= outErrors (outputs (1-outputs));
             hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
                                              (* update weights *)
             outLastDelta= eta Outer[Times,outDelta,hidOuts]+alpha outLastDelta;
             outWts += outLastDelta;
             hidLastDelta = eta Outer[Times,hidDelta,inputs]+
                                                        alpha hidLastDelta;
             hidWts += hidLastDelta;
                    outErrors.outErrors, (* this puts the error on the list *)
                    {numIters}] ;        (* this many times, Table ends here *)
  Print["New hidden-layer weight matrix: "];
  Print[]; Print[hidWts];Print[];
  Print["New output-layer weight matrix: "];
  Print[]; Print[outWts];Print[];  
  bpnTest[hidWts,outWts,ioPairs,bias->False,printAll->False];
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
  ]                                             (* end of Module *)
 

bpnMomentumSmart[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,
                                alpha_,numIters_] :=
  Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
                         hidLastDelta,outLastDelta,outDelta,hidDelta,outErrors},
    hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber}],{hidNumber}];
    outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
    hidLastDelta = Table[Table[0,{inNumber}],{hidNumber}];
    outLastDelta = Table[Table[0,{hidNumber}],{outNumber}];
    errorList = Table[
                                              (* begin forward pass *)
            ioP=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];
            inputs=ioP[[1]];
            outDesired=ioP[[2]];
            hidOuts = sigmoid[hidWts.inputs];  (* hidden-layer outputs *)
        outputs = sigmoid[outWts.hidOuts]; (* output-layer outputs *)
                                        (* calculate errors *)
            outErrors = outDesired-outputs;
            If[First[Abs[outErrors]]>0.1,
              outDelta= outErrors (outputs (1-outputs));
              hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
                                              (* update weights *)
              outLastDelta= eta Outer[Times,outDelta,hidOuts]+
                                                            alpha outLastDelta;
        outWts += outLastDelta;
              hidLastDelta = eta Outer[Times,hidDelta,inputs]+
                                                        alpha hidLastDelta;
              hidWts += hidLastDelta,Continue]; (* end of If *)
                  outErrors.outErrors, (* this puts the error on the list *)
                  {numIters}]   ;        (* this many times, Table ends here *)
  Print["New hidden-layer weight matrix: "];
  Print[]; Print[hidWts];Print[];
  Print["New output-layer weight matrix: "];
  Print[]; Print[outWts];Print[];  
  bpnTest[hidWts,outWts,ioPairs,bias->False,printAll->False];
  errorPlot = ListPlot[errorList, PlotJoined->True];
  Return[{hidWts,outWts,errorList,errorPlot}];
  ]                                             (* end of Module *)
 

bpnCompete[inNumber_,hidNumber_,outNumber_,ioPairs_,eta_,numIters_] :=
 Module[{hidWts,outWts,ioP,inputs,hidOuts,outputs,outDesired, 
                         outInputs,hidEps,outEps,outDelta,hidPos, outPos, hidDelta,outErrors},
  hidWts = Table[Table[Random[Real,{-0.5,0.5}],{inNumber}]{hidNumber}];
  outWts = Table[Table[Random[Real,{-0.5,0.5}],{hidNumber}],{outNumber}];
  errorList = Table[    (* begin forward pass *)
  ioP=ioPairs[[Random[Integer,{1,Length[ioPairs]}]]];
         inputs=ioP[[1]]; 
         outDesired=ioP[[2]];
         hidOuts = sigmoid[hidWts.inputs];
         outputs = sigmoid[outWts.hidOuts]; 
         outErrors = outDesired-outputs;      (* calculate errors *)
         outDelta= outErrors (outputs (1-outputs));
        hidDelta=(hidOuts (1-hidOuts)) Transpose[outWts].outDelta;
                                            (* index of max delta *)
        outPos = First[Flatten[Position[Abs[outDelta],Max[Abs[outDelta]]]]];
         outEps = outDelta[[outPos]];  (* max value *)
         outDelta=Table[-1/4 outEps,{Length[outDelta]}]; (* new outDelta table *)
         outDelta[[outPos]] = outEps;  (* reset this one  *)
                                    (* index of max delta *)
         hidPos = First[Flatten[Position[Abs[hidDelta],Max[Abs[hidDelta]]]]];
         hidEps = hidDelta[[hidPos]];  (* max value *)
         hidDelta=Table[-1/4 hidEps,{Length[hidDelta]}]; (* new outDelta table *)
                hidDelta[[hidPos]] = hidEps;  (* reset this one  *)
         outWts +=eta Outer[Times,outDelta,hidOuts];
         hidWts += eta Outer[Times,hidDelta,inputs];
                outErrors.outErrors, (* this puts the error on the list *)
                {numIters}]     ;        (* this many times, Table ends here *)
 Print["New hidden-layer weight matrix: "];
 Print[ ]; Print[hidWts];Print[ ];
 Print["New output-layer weight matrix: "];
 Print[ ]; Print[outWts];Print[ ];  
 bpnTest[hidWts,outWts,ioPairs,bias->False,printAll->False]; 
 errorPlot = ListPlot[errorList, PlotJoined->True];
 Return[{hidWts,outWts,errorList,errorPlot}];
 ]                                              (* end of Module *)     
 


End[]         (* end the private context *)

EndPackage[]  (* end the package context *)
