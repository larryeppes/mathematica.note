(* Copyright E.M.Clarke and Xudong Zhao, Jan 22, 1991 *)

(* THE PACKAGE FOR SIMPLIFICATION OF FORMULAS *)

BeginPackage["simplify`", "UserInterface`", "SystemInterface`"]

(*  Functions provided to the user:

	seq (operator.m)

	or (operator.m)

	and (operator.m)

	imp (operator.m)

	not (operator.m)

	eqv (operator.m)

	if (operator.m)

	WeakSimplify (simplify.m)

	StrongSimplify (simplify.m)

    Functions needed in other packages but not
    available to the user:

	NIL (operator.m)

	SimplifyIfChanged (simplify.m)

	SimplifyMessage (simplify.m)

	RulesFrom (simplify.m)

*)

Begin["SimpPrivate`"]


<<operator.m;

<<simplify.m;

<<expression_rules.m;

End[]

EndPackage[]
