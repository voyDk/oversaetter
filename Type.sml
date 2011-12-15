structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  type pos = int*int

  datatype Type = Int | Char | IntRef | CharRef

  fun convertType (S100.Int _)
	= Int
    | convertType (S100.Char _)
        = Char
    
  fun getName (S100.Val (f,p))
	= f
    | getName (S100.Ref (f,p))
	= f

  fun getType t (S100.Val (f,p))
	= convertType t
    | getType (S100.Int _) (S100.Ref (f,p))
	= IntRef (* convertType ? *)
    | getType (S100.Char _) (S100.Ref (f,p))
	= CharRef (* convertType ? *)

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x []
        = NONE
    | lookup x ((y,v)::table)
        = if x=y then SOME v else lookup x table

  fun checkExp e vtable ftable =
    case e of
      S100.NumConst _ => Int
    | S100.CharConst _ => Char
    | S100.StringConst _ => Char (* ??? not yet implemented [Char] *)
    | S100.LV lv => checkLval lv vtable ftable
    | S100.Assign (lv,e1,p) =>
        let
	  val t1 = checkLval lv vtable ftable
	  val t2 = checkExp e1 vtable ftable
	in
	  if t1=t2 then t2
	  else raise Error ("Type mismatch in assignment",p)
	end
    | S100.Plus (e1,e2,p) =>
        (case (checkExp e1 vtable ftable,
	       checkExp e2 vtable ftable) of
	   (Int, Int) => Int
	 | (Int, IntRef)  => IntRef
         | (IntRef, Int)  => IntRef
         | (Int, CharRef) => CharRef
         | (CharRef, Int) => CharRef
(* not needed, the last pattern match takes care of it *)
(*
	 | (IntRef, IntRef)   => raise Error("Type mismatch in the assignment",p)
	 | (CharRef, CharRef) => raise Error("Type mismatch in the assignment",p)
	 | (IntRef, CharRef)  => raise Error("Type mismatch in the assignment",p)
	 | (CharRef, IntRef)  => raise Error("Type mismatch in the assignment",p)
*)
	 | (_,_) => raise Error("Type mismatch in the assignment",p)
	)
    | S100.Minus (e1,e2,p) =>
        (case (checkExp e1 vtable ftable,
	       checkExp e2 vtable ftable) of
	   (Int, Int) => Int
	 | (Int, IntRef)  => raise Error("Type mismatch in the assignment",p)
	 | (Int, CharRef) => raise Error("Type mismatch in the assignment",p) 
         | (IntRef, Int)  => IntRef
         | (CharRef, Int) => CharRef
	 | (IntRef, IntRef)   => Int
	 | (CharRef, CharRef) => Int
	 | (IntRef, CharRef)  => raise Error("Type mismatch in the assignment",p)
	 | (CharRef, IntRef)  => raise Error("Type mismatch in the assignment",p)
	 | (_,_) => raise Error("Type mismatch in the assignment",p)
	)
    | S100.Less (e1,e2,p) =>
        if checkExp e1 vtable ftable = checkExp e2 vtable ftable
	then Int else raise Error ("Can't compare different types",p)
    | S100.Equal (e1,e2,p) =>
        if checkExp e1 vtable ftable = checkExp e2 vtable ftable
	then Int else raise Error ("Can't compare different types",p)
    | S100.Call (f,es,p) =>
        (case lookup f ftable of
	   NONE => raise Error ("Unknown function: "^f,p)
	 | SOME (parT,resultT) =>
	     let
	       val argT = List.map (fn e => checkExp e vtable ftable) es
	     in
	       if parT = argT then resultT
	       else raise Error ("Arguments don't match declaration of "^f, p)
	     end)

  and checkLval lv vtable ftable =
    case lv of
      S100.Var (x,p) =>
        (case lookup x vtable of
	   SOME t => t
	 | NONE => raise Error ("Unknown variable: "^x,p))
    | S100.Deref (x,p) =>
	(case lookup x vtable of
	     SOME t => t
	   | NONE => raise Error("Unknown variable: "^x,p))
    | S100.Lookup (x,e,p) => raise Error ("Lookup not yet implemented in Type.sml",p)

  fun extend [] _ vtable = vtable
    | extend (S100.Val (x,p)::sids) t vtable =
        (case lookup x vtable of
	   NONE => extend sids t ((x,t)::vtable)
	 | SOME _ => raise Error ("Double declaration of "^x,p))
    | extend (S100.Ref (x,p)::sids) t vtable = 
      case t of 
        Char => (case lookup x vtable of
                       NONE => (extend sids Char ((x,CharRef)::vtable))
                     | SOME _ => raise Error ("Double declaration of "^x,p))
      | Int  => (case lookup x vtable of
                        NONE => (extend sids Int ((x,IntRef)::vtable))
                      | SOME _ => raise Error ("Double declaration of "^x,p))
      (* | _ => raise Error ("Invalid type specified "^x,p) *) (* not needed, uncomment to prevent pattern not exhaustive *)

  fun checkDecs [] = []
    | checkDecs ((t,sids)::ds) =
        extend (List.rev sids) (convertType t) (checkDecs ds)


  fun checkStat s vtable ftable =
    case s of
      S100.EX e => (checkExp e vtable ftable; ())
    | S100.If (e,s1,p) =>
        if checkExp e vtable ftable = Int
	then checkStat s1 vtable ftable
	else raise Error ("Condition should be integer",p)
    | S100.IfElse (e,s1,s2,p) =>
        if checkExp e vtable ftable = Int
	then (checkStat s1 vtable ftable;
	      checkStat s2 vtable ftable)
	else raise Error ("Condition should be integer",p)
    | S100.While (e,s,p) =>
        if checkExp e vtable ftable = Int
	then checkStat s vtable ftable
	else raise Error ("Condition should be integer",p)
    | S100.Return (e,p) => (* check e return type against expected return type *)
	let
	    val t1 = checkExp e vtable ftable
	    val t2 = Int (* get type of current function getType (lookup x ftable) *)
	in
	    if t1 = t2 then () (* return type of function eg t1 or t2 *)
	    else raise Error("Return type mismatch",p)
	end
    | S100.Block (decs,stats,p) =>
	let
	    val vtable1 = checkDecs decs (* Checking decs and bind new names to vtable *)
	in
            checkStats stats vtable1 ftable

(*	    checkStats stats vtable1 ftable *)
(*	    raise Error ("Block (Scope) not yet implemented in Type.sml",p) *)
	end
	    
(*	check decs - ok
	add decs to vtable1 - ok
	check stats with vtable1	
*)	

  and checkStats [] _ _ = ()
    | checkStats (stat::stats) vtable ftable = 
	(checkStats stats vtable ftable; 
         checkStat stat vtable ftable)

  fun checkFunDec (t,sf,decs,body,p) ftable =
        checkStat body (checkDecs decs) ftable

  fun getFuns [] ftable = ftable
    | getFuns ((t,sf,decs,_,p)::fs) ftable =
        case lookup (getName sf) ftable of
	  NONE =>
            let
              val parT = (List.map (#2) (checkDecs decs))
	      val resultT = getType t sf
	    in
              getFuns fs ((getName sf, (parT,resultT)) :: ftable)
	    end
	| SOME _ => raise Error ("Redeclaration of "^ getName sf,p)

  fun checkProg fs =
    let
      val ftable = getFuns fs [("getint",([],Int)),
			       ("putint",([Int],Int)),
			       ("getstring",([Int],CharRef)),  (* ind: int, ud string *)
			       ("putstring",([CharRef],CharRef)), (* ind: string, ud: string *)
			       ("walloc",([Int], IntRef)), (* ind: int, ud: intref *)
			       ("balloc",([Int], CharRef)) (* ind: int, ud charref *)
			      ]
    in
      List.app (fn f => checkFunDec f ftable) fs;
      case lookup "main" ftable of
	NONE => raise Error ("No main function found",(0,0))
      | SOME ([],Int) => ()
      | _ => raise Error ("main function has illegal type",(0,0))
    end

end
