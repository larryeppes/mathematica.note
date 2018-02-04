BeginPackage["Art`"]

vmag1::usage = "vmag1[v]"
vmag2::usage = "vmag2[v]"
resetflag1::usage = "resetflag1[outp,inp,rho]"
resetflag2::usage = "resetflag2[u,p,c,rho]"
winner::usage = "winner[p,val]"
compete::usage = "compete[f2Activities]"
art1Init::usage = "art1Init[f1dim,f2dim,b1,d1,el,del1,del2]"
art1::usage = "art1[f1dim,f2dim,a1,b1,c1,d1,el,rho,f1Wts,f2Wts,inputs]"
art2F1::usage = "art2F1[in,a,b,d,tdWts,f1d,winr:0]"
art2Init::usage = "art2Init[f1dim,f2dim,d,del1]"
art2::usage = "art2[f1dim,f2dim,a1,b1,c1,d,theta,rho,f1Wts,f2Wts,inputs]"


Begin["`Private`"]    (* begin the private context *)

(* vmag for ART1 networks *)
vmag1[v_] := Count[v,1]
 
 
(*  vmag for ART2 networks  *)
vmag2[v_] = Sqrt[v . v]
 
 
(* reset for ART1 *)
resetflag1[outp_,inp_,rho_] := 
		If[vmag1[outp]/vmag1[inp]<rho,True,False]
 
 
(* reset for ART2 *)
resetflag2[u_,p_,c_,rho_]:=
	Module[{r,flag},
		r = (u + c p) / (vmag2[u] + vmag2[c p]);
		If[rho/vmag2[r] > 1,flag=True,flag=False];
		Return[flag];
		];  
 
 
winner[p_,val_] := First[First[Position[p,val]]]
 
 
compete[f2Activities_] :=
  Module[{i,x,f2dim,maxpos},
    x=f2Activities;
    maxpos=First[First[Position[x,Max[f2Activities]]]];
    f2dim = Length[x];
    For[i=1,i<=f2dim,i++,
		   If[i!=maxpos,x[[i]]=0;Continue]  (* end of If  *)
		 ]; (* end of For  *)
	Return[x];
	];  (* end of Module *)

 
art1Init[f1dim_,f2dim_,b1_,d1_,el_,del1_,del2_] :=
  Module[{z12,z21},
   z12 = Table[Table[(b1-1)/d1 + del1,{f2dim}],{f1dim}];
   z21 = Table[Table[(el/(el-1+f1dim)-del2),{f1dim}],{f2dim}];
   Return[{z12,z21}];
   ]; (* end of Module *)
 
 
 
art1[f1dim_,f2dim_,a1_,b1_,c1_,d1_,el_,rho_,f1Wts_,f2Wts_,inputs_] :=
  Module[{droplistinit,droplist,notDone=True,i,nIn=Length[inputs],reset,
  		  n,sf1,t,xf2,uf2,v,windex,matchList,newMatchList,tdWts,buWts},
   droplistinit = Table[1,{f2dim}];   (* initialize droplist *)
   tdWts=f1Wts; buWts=f2Wts;
   matchList =    (* construct list of F2 units and encoded input patterns *)
	      Table[{StringForm["Unit ",n]},{n,f2dim}];
   While[notDone==True,newMatchList = matchList; (* process until stable *)
      For[i=1,i<=nIn,i++,in = inputs[[i]];       (* process inputs in sequence *)
        droplist = droplistinit;reset=True;      (* initialize *)
    	While[reset==True,                          (* cycle until no reset *)
    	   xf1 = in/(1+a1*(in+b1)+c1);              (* activities *)
    	   sf1 = Map[If[#>0,1,0]&,xf1];             (* F1 outputs *)
    	   t= buWts . sf1;                          (* F2 net-inputs *)
    	   t = t droplist;                          (* turn off inhibited units *)
    	   xf2 = compete[t];	                       (* F2 activities *)
    	   uf2 = Map[If[#>0,1,0]&,xf2];             (* F2 outputs *)
    	   windex = winner[uf2,1];                  (* winning index *)
    	   v= tdWts . uf2;		                        (* F1 net-inputs *)
    	   xf1 =(in+ d1*v-b1)/(1+a1*(in+d1*v)+c1);  (* new F1 activities *)
    	   sf1 = Map[If[#>0,1,0]&,xf1];             (* new F1 outputs *)
    	   reset = resetflag1[sf1,in,rho];          (* check reset *)
    	   If[reset==True,droplist[[windex]]=0;     (* update droplist *)
    	   		Print["Reset with pattern ",i," on unit ",windex],Continue];
    	   ]; (* end of While reset==True *)
    	Print["Resonance established on unit ",windex," with pattern ",i];
     tdWts=Transpose[tdWts];   (* resonance, so update weights,top down first *)
		   tdWts[[windex]]=sf1;  
		   tdWts=Transpose[tdWts];
		   buWts[[windex]] = el/(el-1+vmag1[sf1]) sf1; (* then bottom up *)
     matchList[[windex]] =	                      (* update matching list *)
  		             Reverse[Union[matchList[[windex]],{i}]];
		 ];  (* end of For i=1 to nIn *)
	  If[matchList==newMatchList,notDone=False;    (* see if matchList is static *)
	  		Print["Network stable"],
	  		Print["Network not stable"];
	  		newMatchList = matchList];];  (* end of While notDone==True *)
   Return[{tdWts,buWts,matchList}];
   ];    (* end of Module *)    	
 
 
 
art2F1[in_,a_,b_,d_,tdWts_,f1d_,winr_:0] :=
  Module[{w,x,u,v,p,q,i},
    w=x=u=v=p=q=Table[0,{f1d}];
    For[i=1,i<=2,i++,
      w = in + a u;
      x = w / vmag2[w];
      v = f[x] + b f[q];
      u = v / vmag2[v];
      p = If[winr==0,u,
     			u + d Transpose[tdWts][[winr]] ];
     q = p / vmag2[p];
     ];  (* end of For i *)
    Return[{u,p}];
    ]  (* end of Module *)
 
 

art2Init[f1dim_,f2dim_,d_,del1_] :=
  Module[{z12,z21},
   z12 = Table[Table[0 ,{f2dim}],{f1dim}];
   z21 = Table[Table[del1/((1-d)*Sqrt[f1dim] ),
		        {f1dim}],{f2dim}];
   Return[{z12,z21}];
   ]; (* end of Module *)
 
 
 
art2[f1dim_,f2dim_,a1_,b1_,c1_,d_,theta_,rho_,f1Wts_,f2Wts_,inputs_] :=
  Module[{droplistinit,droplist,notDone=True,i,nIn= Length[inputs],reset,
  		  u,p,t,xf2,uf2,v,windex,matchList,newMatchList,tdWts,buWts},
    droplistinit = Table[1,{f2dim}];     (* initialize droplist *)
    tdWts = f1Wts; buWts = f2Wts;
    u = p = Table[0,{f1dim}];
    		(* construct list of F2 units and encodedinput patterns *)
	  matchList = Table[{StringForm["Unit ``",n]},{n,f2dim}];
    While[notDone==True,newMatchList = matchList;  (* process until stable *)
      For[i=1,i<=nIn,i++,          (* process each input pattern in sequence *)
        droplist = droplistinit;   (* initialize droplist for new input *)
        reset=True;
        in = inputs[[i]];          (* next input pattern *)
        windex = 0;                (* initialize  *)
    	While[reset==True,            (* cycle until no reset *)
    	   {u,p} = art2F1[in,a1,b1,d,tdWts,f1dim,windex];
    	   t= buWts . p;		            (* F2 net-inputs *)
    	   t = t droplist;            (* turn off inhibited units *)
    	   xf2 = compete[t];	         (* F2 activities *)
    	   uf2 = Map[g,xf2];          (* F2 outputs *)
     	  windex = winner[uf2,d];   (* winning index *)
		     {u,p} = art2F1[in,a1,b1,d,tdWts,f1dim,windex];
    	   reset = resetflag2[u,p,c1,rho];  (* check reset *)
    	   If[reset==True,droplist[[windex]]=0;	(* update droplist *)
    	   		Print["Reset with pattern ",i," on unit ",windex],Continue];
    	   ]; (* end of While reset==True *)
   Print["Resonance established on unit ",	windex," with pattern ",i];
   tdWts=Transpose[tdWts];  (* resonance, so update weights *)
		 tdWts[[windex]]=u/(1-d);  tdWts=Transpose[tdWts];  
		 buWts[[windex]] = u/(1-d);
		 matchList[[windex]] =     (* update matching list *)
      Reverse[Union[matchList[[windex]],{i}]];
		 ];  (* end of For i=1 to nIn *)
	  If[matchList==newMatchList,notDone=False;   (* see if matchList is static *)
	  		Print["Network stable"],Print["Network not stable"];
	  		newMatchList = matchList];
	  ];  (* end of While notDone==True *)
   Return[{tdWts,buWts,matchList}];
   ];    (* end of Module *)    	



End[]         (* end the private context *)

EndPackage[]  (* end the package context *)