BeginPackage["GeneticAlgorithms`"]

f::usage = "f[x]"
flip::usage = "flip[x]"
newGenerate::usage = "newGenerate[pmutate,keyPhrase,pop,numGens]"
decodeBGA::usage = "decodeBGA[chromosome]"
selectOne::usage = "selectOne[foldedFitnessList,fitTotal]"
myXor::usage = "myXor[x,y]"
mutateBGA::usage = "mutateBGA[pmute,allel]"
crossOver::usage = "crossOver[pcross,pmutate,parent1,parent2]"
initPop::usage = "initPop[psize,csize]"
displayBest::usage = "displayBest[fitnessList,number2Print]"
bga::usage = "bga[pcross,pmutate,popInitial,fitFunction,numGens,printNum]"
sigmoid::usage = "sigmoid[x]"
initXorPop::usage = "initXorPop[psize,csize,ioPairs]"
decodeXorChrom::usage = "decodeXorChrom[chromosome]"
gaNetFitness::usage = "gaNetFitness[hiddenWts,outputWts,ioPairVectors]"
crossOverXor::usage = "crossOverXor[pcross,pmutate,parent1,parent2]"
gaXor::usage = "gaXor[pcross,pmutate,popInitial,numReplace,ioPairs,numGens,printNum]"
decodeXorGenotype::usage = "decodeXorGenotype[genotype]"
encodeNetGa::usage = "encodeNetGa[weight,len]"
randomPop::usage = "randomPop[psize,csize,ioPairs,numGens]"


Begin["`Private`"]    (* begin the private context *)


f[x_] := 1+Cos[x]/(1+0.01 x^2)
 
 
flip[x_] := If[Random[]<=x,True,False]
 
 
newGenerate[pmutate_,keyPhrase_,pop_,numGens_] :=
  Module[{i,newPop,parent,diff,matches,
  		  index,fitness},
  	newPop=pop;
    For[i=1,i<=numGens,i++,
      diff = Map[(keyPhrase-#)&,newPop];
      matches = Map[Count[#,0]&,diff];
      fitness = Max[matches];
      index = Position[matches,fitness];
      parent = newPop[[First[Flatten[index]]]];
      Print["Generation ",i,": ",FromCharacterCode[parent],
      			" Fitness= ",fitness];
      newPop = Table[Map[mutateLetter[pmutate,#]&,parent],{100}];
      ];  (* end of For *)
     ];  (* end of Module *)  
 
 
decodeBGA[chromosome_] :=
  Module[{pList,lchrom,values,phenotype},
	lchrom = Length[chromosome];
		(* convert from binary to decimal *)
	pList = Flatten[Position[chromosome,1] ];
	values = Map[2^(lchrom-#)&,pList];
	decimal = Apply[Plus,values];
		(* scale to proper range *)
	phenotype = decimal (0.07820136852394916911)-40;
	Return[phenotype];
	  ];   (* end of Module *)
 
 
 
selectOne[foldedFitnessList_,fitTotal_] :=
  Module[{randFitness,elem,index},
  	randFitness = Random[] fitTotal;
  	elem = Select[foldedFitnessList,#>=randFitness&,1];
  	index = 
  	 Flatten[Position[foldedFitnessList,First[elem]]];
  	Return[First[index]];
  	];  (* end of Module *)
 
 
myXor[x_,y_] := If[x==y,0,1];
 
 
mutateBGA[pmute_,allel_] := 
	If[flip[pmute],myXor[allel,1],allel];
 
 
crossOver[pcross_,pmutate_,parent1_,parent2_] :=
  Module[{child1,child2,crossAt,lchrom },
  			(* chromosome length *)
  	lchrom = Length[parent1];
	If[ flip[pcross],
			(* True: select cross site at random *)
		crossAt = Random[Integer,{1,lchrom-1}];
			(* construct children  *)
		child1 = Join[Take[parent1,crossAt], Drop[parent2,crossAt]];
		child2 = Join[Take[parent2,crossAt],	Drop[parent1,crossAt]],			
		  (* False: return parents as children *)
		child1 = parent1;
		child2 = parent2;
	   ];  (* end of If *)
		  (* perform mutation *)
	 child1 = Map[mutateBGA[pmutate,#]&,child1];
	 child2 = Map[mutateBGA[pmutate,#]&,child2];
	 Return[{child1,child2}];
	 ];  (* end of Module *)
 
 
 
initPop[psize_,csize_] := 
	Table[Random[Integer,{0,1}],{psize},{csize}];
 
 
displayBest[fitnessList_,number2Print_] :=
	Module[{i,sortedList},
		sortedList = Sort[fitnessList,Greater];
		For[i=1,i<=number2Print,i++,
			Print["fitness = ",sortedList[[i]] ];
			]; (* end of For i *)
		];  (* end of Module *)
 
 
bga[pcross_,pmutate_,popInitial_,fitFunction_,numGens_,printNum_] :=
  Module[{i,newPop,parent1,parent2,diff,matches,
  		  oldPop,reproNum,index,fitList,fitListSum,
  		  fitSum,pheno,pIndex,pIndex2,f,children},
  	oldPop=popInitial;                 (* initialize first population *)
  	reproNum = Length[oldPop]/2;       (* calculate number of reproductions *)
  	f = fitFunction;                   (* assign the fitness function *)
    For[i=1,i<=numGens,i++,           (* perform numGens generations *)
      pheno = Map[decodeBGA,oldPop];  (* decode the chromosomes *)
	  fitList = f[pheno];                (* determine the fitness of each phenotype *)
	  Print[" "];                        (* print out the best individuals *)
	  Print["Generation ",i,"  Best ",printNum];
	  Print[" "];
	  displayBest[fitList,printNum];
      fitListSum = FoldList[Plus,First[fitList],Rest[fitList]];
      fitSum = Last[fitListSum];      (* find the total fitness *)
      newPop = Flatten[Table[      (* determine the new population *)
         pIndex1 = selectOne[fitListSum,fitSum];  (* select parent indices *)
		 pIndex2 = selectOne[fitListSum,fitSum];
		 parent1 = oldPop[[pIndex1]];        (* identify parents *)
		 parent2 = oldPop[[pIndex2]];
		 children = crossOver[pcross,pmutate,parent1,parent2]; (* crossover and mutate *)
		 children,{reproNum}],1     (* add children to list; flatten to first level *)
		 ];  (* end of Flatten[Table] *)
  	  oldPop = newPop;  	       (* new becomes old for next gen *)
      ];  (* end of For i*)
     ];  (* end of Module *)     
 
 
 
sigmoid[x_] := 1./(1+E^(-x));
 
 
initXorPop[psize_,csize_,ioPairs_] :=
  Module[{i,iPop,hidWts,outWts,mseInv},
  				(* first the chromosomes *)
	iPop = Table[
		{Table[Random[Integer,{0,1}],{csize}],(* h1 *)
		  Table[Random[Integer,{0,1}],{csize}],(* h2 *)
		  Table[Random[Integer,{0,1}],{csize}] (* o1 *)
		 },  {psize}  ]; (* end of Table *)
		 		(* then decode and eval fitness *)
		 		(* use For loop for clarity *)
	   For[i=1,i<=psize,i++,
	   			(* make hidden weight matrix *)
	   	 hidWts = Join[iPop[[i,1]],iPop[[i,2]] ];
	   	 hidWts = Partition[hidWts,20];
	   	 hidWts = Map[decodeXorChrom,hidWts];
	   	 hidWts = Partition[hidWts,2];
	   	 		(* make output weight matrix *)
	   	 outWts = Partition[iPop[[i,3]],20];
	   	 outWts = Map[decodeXorChrom,outWts];
	   	 		(* get mse for this network *)
	   	 mseInv = gaNetFitness[hidWts,outWts,ioPairs];
	   	 		(* prepend mseInv *)
	   	 PrependTo[iPop[[i]],mseInv];
	   	 ];  (* end For *)		
	 Return[iPop];
	 ];  (* end of Module *)
 
 
decodeXorChrom[chromosome_] :=
  Module[{pList,lchrom,values,p,decimal},
	lchrom = Length[chromosome];
		(* convert from binary to decimal *)
	pList = Flatten[Position[chromosome,1] ];
	values = Map[2^(lchrom-#)&,pList];
	decimal = Apply[Plus,values];
		(* scale to proper range *)
	p = decimal (9.536752259018191355*10^-6)-5;
	Return[p];
	  ];   (* end of Module *)
 
 
 
gaNetFitness[hiddenWts_,outputWts_,ioPairVectors_] :=
  Module[{inputs,hidden,outputs,desired,errors,
  	len,errorTotal,errorSum},
   inputs=Map[First,ioPairVectors];
   desired=Map[Last,ioPairVectors];
   len = Length[inputs];
   hidden=sigmoid[inputs.Transpose[hiddenWts]];
   outputs=sigmoid[hidden.Transpose[outputWts]];
   errors= desired-outputs;
   errorSum = Apply[Plus,errors^2,2]; (* second level *)
   errorTotal = Apply[Plus,errorSum];
   		(* inverse of mse *)
   Return[len/errorTotal];
   ]					(* end of Module *)		
 
 
crossOverXor[pcross_,pmutate_,parent1_,parent2_] :=
  Module[{child1,child2,crossAt,lchrom,
  			i,numchroms,chroms1,chroms2},
  			(* strip off mse *)
  	chroms1 = Rest[parent1];
  	chroms2 = Rest[parent2];
  			(* chromosome length *)
  	lchrom = Length[chroms1[[1]]];
  			(* number of chromosomes in each list *)
  	numchroms = Length[chroms1];
  	For[i=1,i<=numchroms,i++,     (* for each chrom *)
	  If[ flip[pcross],
		crossAt = Random[Integer,{1,lchrom-1}]; (* True: select cross site at random *)
			(* construct children  *)
		chroms1[[i]] = Join[Take[chroms1[[i]],crossAt],Drop[chroms2[[i]],crossAt]];
		chroms2[[i]] = Join[Take[chroms2[[i]],crossAt],	Drop[chroms1[[i]],crossAt]],			
		Continue];   (* False: don't change chroms[[i]].  End of If *)
		  (* perform mutation *)
	    chroms1[[i]] =  Map[mutateBGA[pmutate,#]&,chroms1[[i]]];
	    chroms2[[i]] = Map[mutateBGA[pmutate,#]&,chroms2[[i]]];
	    ];  (* end of For i *)
	 Return[{chroms1,chroms2}];
	 ];  (* end of Module *)
 
 
 
gaXor[pcross_,pmutate_,popInitial_,numReplace_,ioPairs_,numGens_,printNum_] :=
  Module[{i,j,newPop,parent1,parent2,diff,matches,
  		  oldPop,reproNum,index,fitList,fitListSum,
  		  fitSum,pheno,pIndex,pIndex2,f,children,hids,outs,mseInv},
  		  	(* initialize first population sorted by fitness value  *)
  	oldPop= Sort[popInitial,Greater[First[#],First[#2]]&];
  	reproNum = numReplace;       (* calculate number of reproductions *)
     For[i=1,i<=numGens,i++,
      fitList = Map[First,oldPop];    (* list of fitness values*) 
  	  		                               (* make the folded list of fitness values *)
      fitListSum = FoldList[Plus,First[fitList],Rest[fitList]];
      fitSum = Last[fitListSum];      (* find the total fitness *)
	  newPop = Drop[oldPop,-reproNum]; (* new population; eliminate reproNum worst *)
	  For[j=1,j<=reproNum/2,j++,       (* make reproNum new children *)
      			(* select parent indices *)
      	pIndex1 = selectOne[fitListSum,fitSum];
      	pIndex2 = selectOne[fitListSum,fitSum];
      	parent1 = oldPop[[pIndex1]];    (* identify parents *)
	  	parent2 = oldPop[[pIndex2]];
	  	children = crossOverXor[pcross,pmutate,parent1,parent2];(*cross and mutate*)
		{hids,outs} = decodeXorGenotype[children[[1]] ]; (* fitness of children *)
		mseInv = gaNetFitness[hids,outs,ioPairs];
		children[[1]] = Prepend[children[[1]],mseInv];
		{hids,outs} = decodeXorGenotype[children[[2]] ];
		mseInv = gaNetFitness[hids,outs,ioPairs];
		children[[2]] = Prepend[children[[2]],mseInv];
        newPop = Join[newPop,children]; (* add children to new population *)
		];  (* end of For j *)
  	  oldPop =	Sort[newPop,Greater[First[#],First[#2]]&];(* for next gen *)
  	  		(* print best mse values (1/mseInv) *)
  	  Print[ ];Print["Best of generation ",i];
  	  For[j=1,j<=printNum,j++,Print[(1.0/oldPop[[j,1]])]; ];
      ];  (* end of For i*)
     Return[oldPop];
     ];  (* end of Module *)     
 
 
 
decodeXorGenotype[genotype_] :=
	  Module[{hidWts,outWts}, 
	   	 hidWts = Join[genotype[[1]],genotype[[2]] ];
	   	 hidWts = Partition[hidWts,20];
	   	 hidWts = Map[decodeXorChrom,hidWts];
	   	 hidWts = Partition[hidWts,2];
	   	 		(* make output weight matrix *)
	   	 outWts = Partition[genotype[[3]],20];
	   	 outWts = Map[decodeXorChrom,outWts];
	   	 Return[{hidWts,outWts}];
	   	 ];
 
 
encodeNetGa[weight_,len_] :=
  Module[{pList,values,dec,chromosome,i},
  	i=len;
  	l=Table[0,{i}];
		(* scale to proper range *)
	dec = Round[(weight+5.)/(9.536752259018191355*10^-6)];
	While[dec!=0&&dec!=1,
	  l=ReplacePart[l,Mod[dec,2],i];
	  dec=Quotient[dec,2];
	  --i;
	  ];
   l=ReplacePart[l,dec,i] 
	  ];   (* end of Module *)
 
 
randomPop[psize_,csize_,ioPairs_,numGens_] :=
  Module[{i,pop},
     For[i=1,i<=numGens,i++,
       pop = initXorPop[psize,csize,ioPairs];
       pop = Sort[pop,Greater[First[#],First[#2]]&];
       Print[ ];
       Print["Random generation ",i];
       Print[(1.0/pop[[1,1]])];
       ];
     ];


End[]         (* end the private context *)

EndPackage[]  (* end the package context *)