(* Copyright E.M.Clarke and Xudong Zhao, Jan 22, 1991 *)

(* QUANTIFICATION PACKAGE *)



BeginPackage["quantify`", "UserInterface`", "SystemInterface`"]

(* Functions provided to the user:

	all (quantify.m)

	some (quantify.m)

   Functions needed in other packages but not
   available to the user:

	Const (quantify.m)

	Var (quantify.m)

	Skolemize (quantify.m)

	PositivePosition (quantify.m)

	NegativePosition (quantify.m)

	AddQuantifiers (quantify.m)

	QuantifierNames (quantify.m)
	
	instantiate (unify.m)

	unify (unify.m)

	apply (unify.m)

	conjunct (unify.m)

	disjunct (unify.m)

	CurrentLemma (unify.m)

	MatchState (unify.m)

*)

Begin["QuanPrivate`"]

<<quantify.m;

<<unify.m;

End[]

EndPackage[]