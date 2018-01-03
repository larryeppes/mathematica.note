BeginPackage["TravelingSalesperson`"]

nOutOfN::usage = "nOutOfN[weights,externIn,numUnits,lambda,deltaT,
numIters,printFreq,reset:False]"
tsp::usage = "tsp[weights,externIn,numUnits,lambda,deltaT,
numIters,printFreq,reset:False]"

Begin["`Private`"]    (* begin the private context *)

nOutOfN[weights_,externIn_,numUnits_,lambda_,deltaT_,
numIters_,printFreq_,reset_:False]:=
	Module[{iter,l,dt,indx,ins},
	  dt=deltaT;
	  l=lambda;
	  iter=numIters;
	  ins=externIn;
	  		(* only reset if starting over *)
	  If[reset,ui=Table[Random[],{numUnits}];
	  		   vi = g[l,ui],Continue];  (* end of If *)
	  	Print["initial ui = ",N[ui,2]];Print[];
	  	Print["initial vi = ",N[vi,2]];
	  For[iter=1,iter<=numIters,iter++,
	    indx = Random[Integer,{1,numUnits}];
	    ui[[indx]] = ui[[indx]]+
	    	dt (vi . Transpose[weights[[indx]]] +
            ui[[indx]] + ins[[indx]]);
        vi[[indx]] = g[l,ui[[indx]]];
	    If[Mod[iter,printFreq]==0,
	    	Print[];Print["iteration = ",iter];
	    	Print["net inputs = "];
	    	Print[N[ui,2]];
	    	Print["outputs = "];
	    	Print[N[vi,2]];Print[];
	    	];  (* end of If *)
	    ];  (* end of For *)
		Print[];Print["iteration = ",--iter];
	    Print["final outputs = "];
	    Print[vi];
	   ];  (* end of Module *)
 
 

tsp[weights_,externIn_,numUnits_,lambda_,deltaT_,
numIters_,printFreq_,reset_:False]:=
	Module[{iter,l,dt,indx,ins,utemp},
	  dt=deltaT;
	  l=lambda;
	  iter=numIters;
	  ins=externIn;
	  		(* only reset if starting over *)
	  If[reset,
	     utemp = ArcTanh[(2.0/Sqrt[numUnits])-1]/l;
	     ui=Table[
	       utemp+Random[Real,{-utemp/10,utemp/10}],
	               {numUnits}];  (* end of Table *)
	  		   vi = g[l,ui],Continue];  (* end of If *)
	  	Print["initial ui = ",N[ui,2]];Print[];
	  	Print["initial vi = ",N[vi,2]];
	  For[iter=1,iter<=numIters,iter++,
	    indx = Random[Integer,{1,numUnits}];
	    ui[[indx]] = ui[[indx]]+
	    	dt (vi . Transpose[weights[[indx]]] +
            ui[[indx]] + ins[[indx]]);
        vi[[indx]] = g[l,ui[[indx]]];
	    If[Mod[iter,printFreq]==0,
	    	Print[];Print["iteration = ",iter];
	    	Print["net inputs = "];
	    	Print[N[ui,2]];
	    	Print["outputs = "];
	    	Print[N[vi,2]];Print[];
	    	];  (* end of If *)
	    ];  (* end of For *)
		Print[];Print["iteration = ",--iter];
	    Print["final outputs = "];
	    Print[MatrixForm[Partition[N[vi,2],Sqrt[numUnits]]]];
	   ];  (* end of Module *)
 



End[]         (* end the private context *)

EndPackage[]  (* end the package context *)