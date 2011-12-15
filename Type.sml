structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  type pos = int*int

  datatype Type = Int | Char | IntRef | CharRef

  fun convertType (S100.Int _)  = Int
    | convertType (S100.Char _) = Char
    
  fun getName (S100.Val (f,p))	= f
    | getName (S100.Ref (f,p))	= f

  fun valToRef Int _  = IntRef
    | valToRef Char _ = CharRef
    | valToRef _ p    = raise Error ("Not a ref type",p)

  fun refToVal IntRef _  = Int
    | refToVal CharRef _ = Char
    | refToVal _ p = raise Error ("Not a val type",p)

  fun flattenType Char = Int
    | flattenType t = t
 
  fun getType t (S100.Val (f,p)) = convertType t
    | getType t (S100.Ref (f,p)) = valToRef (convertType t) p

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x []
        = NONE
    | lookup x ((y,v)::table)
        = if x=y then SOME v else lookup x table

  fun checkExp e vtable ftable =
    case e of
      S100.NumConst _ => Int
    | S100.CharConst _ => Char
    | S100.StringConst _ => CharRef (* Strings are null-terminated char arrays  *)
    | S100.LV lv => checkLval lv vtable ftable
    | S100.Assign (lv,e1,p) =>
        let
	  val t1 = checkLval lv vtable ftable
	  val t2 = checkExp e1 vtable ftable
	in
          case (flattenType t1, flattenType t2) of
            (Int, Int)         => Int
          | (IntRef, IntRef)   => IntRef
          | (CharRef, CharRef) => CharRef
          | _                  => raise Error ("Type mismatch in assignment", p)
	end
    | S100.Plus (e1,e2,p) =>
        (case (flattenType (checkExp e1 vtable ftable),
	       flattenType (checkExp e2 vtable ftable)) of
	   (Int, Int)     => Int
	 | (Int, IntRef)  => IntRef
         | (IntRef, Int)  => IntRef
         | (Int, CharRef) => CharRef
         | (CharRef, Int) => CharRef
	 | _              => raise Error("Type mismatch in the assignment",p)
	)
    | S100.Minus (e1,e2,p) =>
        (case (flattenType (checkExp e1 vtable ftable),
	       flattenType (checkExp e2 vtable ftable)) of
	   (Int, Int) => Int
         | (IntRef, Int)  => IntRef
         | (CharRef, Int) => CharRef
	 | (IntRef, IntRef)   => Int
	 | (CharRef, CharRef) => Int
	 | _ => raise Error("Type mismatch in the assignment",p)
	)
    | S100.Less (e1,e2,p) =>
        if flattenType (checkExp e1 vtable ftable) = flattenType (checkExp e2 vtable ftable)
	then Int else raise Error ("Can't compare different types",p)
    | S100.Equal (e1,e2,p) =>
        if flattenType (checkExp e1 vtable ftable) = flattenType (checkExp e2 vtable ftable)
	then Int else raise Error ("Can't compare different types",p)
    | S100.Call (f,es,p) =>
        (case lookup f ftable of
	   NONE => raise Error ("Unknown function: "^f,p)
	 | SOME (parT,resultT) =>
	     let
	       val argT = List.map (fn e => checkExp e vtable ftable) es
               val flattenParT = List.map flattenType parT
               val flattenArgT = List.map flattenType argT
	     in
	       if flattenParT = flattenArgT then resultT
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
	    SOME t => refToVal t p
	  | NONE => raise Error("Unknown variable: "^x,p))
       | S100.Lookup (x,e,p) => 
         (case lookup x vtable of
            NONE => raise Error ("Unknown variable: "^x,p)
          | SOME t => if flattenType (checkExp e vtable ftable) = Int
                      then refToVal t p
                      else raise Error ("Array index must be of type char or int",p))


  fun extend [] _ vtable = vtable
    | extend (S100.Val (x,p)::sids) t vtable =
        (case lookup x vtable of
	   SOME _ => raise Error ("Double declaration of "^x,p)
         | NONE => extend sids t ((x,t)::vtable))

    | extend (S100.Ref (x,p)::sids) t vtable = 
      (case lookup x vtable of
         SOME _ => raise Error ("Double declaration of "^x,p)
       | NONE  => extend sids t ((x, valToRef t p)::vtable))


  fun checkDecs [] = []
    | checkDecs ((t,sids)::ds) =
        extend (List.rev sids) (convertType t) (checkDecs ds)

  (* vi ændrer retur typen fra () til Option, så vi kan tjekke
     retur type i checkFunDecs *)
  fun checkStat t s vtable ftable =
    case s of
      S100.EX e => (checkExp e vtable ftable; NONE)
    | S100.If (e,s1,p) =>
        if flattenType (checkExp e vtable ftable) = Int
	then (checkStat t s1 vtable ftable; NONE)
	else raise Error ("Condition should be integer",p)
    | S100.IfElse (e,s1,s2,p) =>
        if flattenType (checkExp e vtable ftable) = Int
	then case (checkStat t s1 vtable ftable,
	           checkStat t s2 vtable ftable) of
               (SOME t1, SOME t2) => SOME t1
             | _                  => NONE
	else raise Error ("Condition should be integer",p)
    | S100.While (e,s,p) =>
        if flattenType (checkExp e vtable ftable) = Int
	then (checkStat t s vtable ftable; NONE)
	else raise Error ("Condition should be integer",p)
    | S100.Return (e,p) => 
	let
	    val t1 = checkExp e vtable ftable
	in
            if flattenType t1 = flattenType t
            then SOME t (* Den erklæret type *)
            else raise Error ("Return type mismatch",p)
	end
    | S100.Block (decs,stats,p) =>
	let
	    val vtable1 = checkDecs decs @ vtable (* extend vtable *)
            fun checkReturnVal (stat, returnVal) =
                case checkStat t stat vtable1 ftable of
                  SOME x => SOME x
                | NONE => returnVal
	in
            List.foldl checkReturnVal NONE stats 
	end
	    

  fun checkFunDec (t,sf,decs,body,p) ftable =
      case checkStat (getType t sf) body (checkDecs decs) ftable of
        NONE => raise Error ("Missing return statement", p)
      | _ => () (* Matcher, hvis vi har en retur værdi *)

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
			       ("getstring",([Int],CharRef)),  
			       ("putstring",([CharRef],CharRef)), 
			       ("walloc",([Int], IntRef)), 
			       ("balloc",([Int], CharRef)) 
			      ]
    in
      List.app (fn f => checkFunDec f ftable) fs;
      case lookup "main" ftable of
	NONE => raise Error ("No main function found",(0,0))
      | SOME ([],Int) => ()
      | _ => raise Error ("main function has illegal type",(0,0))
    end

end
