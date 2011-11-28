structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  type pos = int*int

  datatype Type = Int | Bool | TyVar of string

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x [] =  NONE
    | lookup x ((y,v)::table) = 
        if x=y then SOME v else lookup x table


		
  (* combine two symbol tables and check for duplicates *)
  fun combineTables [] table2 p = table2
    | combineTables ((x,v)::table1) table2 p =
        case lookup x table2 of
          SOME _ => raise Error ("Repeated identifier "^x,p)
        | NONE => (x,v) :: combineTables table1 table2 p

  (* check that type expression is valid and return its type *)
  fun checkType te ttable =
    case te of
      Cat.Int _   		=> Int
	| Cat.Bool _  		=> Bool
	| Cat.TyVar (s,pos) 	=> 
		case lookup s ttable of 
		  SOME _ => TyVar s
		| NONE => raise Error ("Unknown type "^s, pos)

  (* Check pattern and return vtable *)
  fun checkPat pat ty ttable pos =
    case (pat,ty) of
      (Cat.NumP _, Int) => []
    | (Cat.VarP (x,p), ty) => [(x,ty)]
	
    | (Cat.TrueP _, Bool) => []
    | (Cat.FalseP _, Bool) => []
    | (Cat.NullP _, _) => []
    | (Cat.TupleP (ps, pos), TyVar s) =>
		let
			val ts = lookup s ttable
		in
			case ts of
			  SOME ts => checkTuple ps ts ttable pos
			| NONE => raise Error ("Unknown tuple type "^s, pos)
		end
    | _ => raise Error ("Pattern doesn't match type", pos)
  
  and checkTuple (p::ps) (t::ts) ttable pos =
		(checkPat p t ttable pos) @ (checkTuple ps ts ttable pos)
    | checkTuple [] [] _ pos = []
    | checkTuple _ _ _ pos = raise Error ("Incorrect length of tuple pattern", pos)
	
  (* check expression and return type *)
  fun checkExp exp vtable ftable ttable =

    case exp of
      Cat.Num (n,pos) => Int
	  
    | Cat.Var (x,pos) =>
       (case lookup x vtable of
		  SOME t => t
        | _ => raise Error ("Unknown variable "^x,pos))

    | Cat.Plus (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
              checkExp e2 vtable ftable ttable) of
          (Int,Int) => Int
        | _ => raise Error ("Non-int argument to +",pos))
		
    | Cat.Minus (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
              checkExp e2 vtable ftable ttable) of
          (Int,Int) => Int
        | _ => raise Error ("Non-int argument to -",pos))
		
    | Cat.Apply (f,e1,pos) =>
       (case lookup f ftable of
	  SOME (t1,t2) =>
	    if t1 = (checkExp e1 vtable ftable ttable)
            then t2
            else raise Error ("Argument does not match declaration of "^f,pos)
        | _ => raise Error ("Unknown function "^f,pos))
    | Cat.Read (n,pos) => Int
    | Cat.Write (e1,pos) =>
       (case checkExp e1 vtable ftable ttable of
          Int => Int
        | _ => raise Error ("Non-int argument to write",pos))

	
	(* jseidelin: If-then-else type check *)
	| Cat.If (e1,e2,e3,pos) =>
		let
			val t1 = checkExp e1 vtable ftable ttable
			val t2 = checkExp e2 vtable ftable ttable
			val t3 = checkExp e3 vtable ftable ttable
		in
			if t1 = Bool then
				if t2 = t3 then
					t2
				else
					raise Error ("Different types for then- and else-expressions",pos)
			else
				raise Error ("Non-bool argument to if-condition",pos)
		end
		
	(* jseidelin: Case-of type check *)
	| Cat.Case (e,ms,pos) =>
		let
			val te  = checkExp e vtable ftable ttable
			val tm = checkMatch ms te vtable ftable ttable pos
		in
			tm
			(* if te = tm then te
			else raise Error ("Match and expression have different type",pos)
			*)
		end

	(* jseidelin: True type check *)
	| Cat.True pos => Bool
	
	(* jseidelin: False type check *)
	| Cat.False pos => Bool
	
	(* jseidelin: Null type check *)
	| Cat.Null (x,pos) => 
		(case lookup x ttable of
			SOME t => TyVar x
			| _ => raise Error ("Unknown type "^x,pos))

	(* jseidelin: Comparison type check *)
	| Cat.Equal (e1,e2,pos) =>
		if (checkExp e1 vtable ftable ttable) = (checkExp e2 vtable ftable ttable) then
			Bool		
		else
			raise Error ("Different types for = operator",pos)

	(* jseidelin: Less-than type check *)
	| Cat.Less (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
              checkExp e2 vtable ftable ttable) of
          (Int,Int) => Bool
        | _ => raise Error ("Non-int argument to <",pos))
	
	(* jseidelin: Not operator type check *)
	| Cat.Not (e,pos) =>
        if Bool = checkExp e vtable ftable ttable then
			Bool
		else
			raise Error ("Non-bool argument to not",pos)
	
	(* jseidelin: And operator type check *)
	| Cat.And (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
              checkExp e2 vtable ftable ttable) of
          (Bool,Bool) => Bool
        | _ => raise Error ("Non-bool argument to and",pos))
	
	(* jseidelin: Or operator type check *)
	| Cat.Or (e1,e2,pos) =>
       (case (checkExp e1 vtable ftable ttable,
              checkExp e2 vtable ftable ttable) of
          (Bool,Bool) => Bool
        | _ => raise Error ("Non-bool argument to or",pos))
		
	(* jseidelin: Tuple type check *)
	| Cat.MkTuple (es, t, pos) =>
		(case lookup t ttable of
			SOME ts => 	if checkExpTypes es ts vtable ftable ttable then
							TyVar t
						else
							raise Error ("Error in tuple types "^t,pos)
			| _ => raise Error ("Unknown tuple type "^t,pos))

	(* jseidelin: Let-in type check *)
	| Cat.Let (d, e, pos) => 
		let 
			val lv = checkDec d vtable ftable ttable pos
		in
			checkExp e (lv @ vtable) ftable ttable
		end
	
  and checkMatch [(p,e)] tce vtable ftable ttable pos =
        let
			val vtable1 = checkPat p tce ttable pos
        in
			checkExp e (vtable1 @ vtable) ftable ttable
        end
    | checkMatch ((p,e)::ms) tce vtable ftable ttable pos =
        let
			val vtable1 = checkPat p tce ttable pos
			val te = checkExp e (vtable1 @ vtable) ftable ttable
			val tm = checkMatch ms tce vtable ftable ttable pos
        in
			if te = tm then 
				te
			else 
				raise Error ("Match branches have different type",pos)
        end
    | checkMatch [] tce vtable ftable ttable pos =
        raise Error ("Empty match",pos)


  (* jseidelin: check a list of expressions against a list of types *)
  and checkExpTypes (e::es) (t::ts) vt ft tt =
		let
			val et = checkExp e vt ft tt
		in
			et = t andalso checkExpTypes es ts vt ft tt
		end
	| checkExpTypes [] [] _ _ _ = true
	| checkExpTypes _ _ _ _ _ = false

  
  (* jseidelin: check a declaration list and return a vtable *)
  and checkDec ((p,e,_)::ds) vt ft tt pos = 
		let
			val t = checkExp e vt ft tt
			val vt1 = checkPat p t tt pos
		in
			checkDec ds (vt1 @ vt) ft tt pos (* skal erstatte hvis variablen eksisterer allerede *)
		end
    | checkDec [] vt _ _ _ = vt


  (* jseidelin: add (name,value) symbol to table *)
  fun bind table x t = (x,t)::table
  
  (* jseidelin: check a list of type expressions *)
  fun checkTypes [] ttable = []
    | checkTypes (t::ts) ttable = (checkType t ttable) :: (checkTypes ts ttable)
  

  fun getFunDecs [] ttable ftable = ftable
    | getFunDecs ((f, targ, tresult, m, pos)::fs) ttable ftable =
		if List.exists (fn (g,_)=>f=g) ftable then
			raise Error ("Duplicate declaration of function "^f,pos)
        else 
			getFunDecs fs ttable
			((f, (checkType targ ttable, checkType tresult ttable))
			 :: ftable)

  (* jseidelin: get type declarations *)
  fun getTyDecs [] ttable = ttable
    | getTyDecs ((t, ts, pos)::tdecs) ttable =
		if List.exists (fn (x,_)=>x=t) ttable then 
			raise Error ("Duplicate declaration of type "^t,pos)
        else
			getTyDecs tdecs (bind ttable t (checkTypes ts (bind ttable t []) )) (* går det godt? rekursiv type dekl. *)

  fun checkFunDec ftable ttable (f, targ, tresult, m, pos) =
    let
      val argtype = checkType targ ttable
      val resulttype = checkType tresult ttable
      val bodytype = checkMatch m argtype [] ftable ttable pos
    in
		if resulttype = bodytype then 
			resulttype
		else 
			raise Error ("Body type doesn't match declaration",pos)
    end

  fun checkProgram (tyDecs, funDecs, e) =
    let
      val ttable = getTyDecs tyDecs []
      val ftable = getFunDecs funDecs ttable []
      val _ = List.map (checkFunDec ftable ttable) funDecs
    in
      (checkExp e [] ftable ttable; ())
    end
end
