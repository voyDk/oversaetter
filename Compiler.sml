(* Compiler for 100 *)
(* Compile by mosmlc -c Compiler.sml *)

structure Compiler :> Compiler =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  (* Name generator.  Call with, e.g., t1 = "tmp"^newName () *)
  val counter = ref 0

  fun newName () = (counter := !counter + 1;
                  "_" ^ Int.toString (!counter)^ "_")

  (* Number to text with spim-compatible sign symbol *)
  fun makeConst n = if n>=0 then Int.toString n
                    else "-" ^ Int.toString (~n)

  fun lookup x [] = NONE
    | lookup x ((y,v)::table) = if x=y then SOME v else lookup x table

  fun isIn x [] = false
    | isIn x (y::ys) = x=y orelse isIn x ys

  (* link register *)
  val RA = "31"
  (* Register for stack pointer *)
  val SP = "29"
  (* Register for heap pointer *)
  val HP = "28"
  (* Register for frame pointer *)
  val FP = "25"

  (* Suggested register division *)
  val maxCaller = 15   (* highest caller-saves register *)
  val maxReg = 24      (* highest allocatable register *)

  datatype Location = Reg of string (* value is in register *) |
                      Addr of string (* value is at address *)

  (* compile expression *)
  fun compileExp e vtable ftable place =
    case e of
      S100.NumConst (n,pos) =>
        if n<32768 then
	  (Type.Int,[Mips.LI (place, makeConst n)])
	else
	  (Type.Int,
	   [Mips.LUI (place, makeConst (n div 65536)),
	   Mips.ORI (place, place, makeConst (n mod 65536))])
    | S100.CharConst (c,pos) =>
	  (Type.Char, [Mips.LI (place,(Char.toString c))]) (* lægger char på place. *)
    | S100.StringConst (c,pos) =>
      let
        val t1 = "_stringConst_"^newName()
      in
	(*
		Gemmer string, c, på den næste tilgængelige adresse.
		Lægger reference til adresse i place
	*)
        (Type.CharRef, [Mips.LA (place, t1), Mips.LABEL t1, Mips.ASCIIZ c]) 
      end
    | S100.LV lval =>
        let
	  val (code,ty,loc) = compileLval lval vtable ftable
	in
	(*
		Gemmer lVal på place

		Char		: Som Int. Flyt værdien fra register til place
		IntRef		: Hent word (32 bit) fra stak ind i register, place. 
		CharRef		: Hent byte (8 bit) fra stak ind i register, place.
		_, Reg x	: Default opførsel som Int
		_, Addr x	: Default opførsel som Intref
	*)
	  case (ty,loc) of
	    (Type.Int, Reg x) =>
	      (Type.Int,
	       code @ [Mips.MOVE (place,x)])
	  | (Type.Char, Reg x) =>
              (Type.Char, 
               code @ [Mips.MOVE (place,x)])
          | (Type.IntRef, Addr x) =>
              (Type.Int,
               code @ [Mips.LW (place,x,makeConst 0)])
          | (Type.CharRef, Addr x) =>
              (Type.Char,
               code @ [Mips.LB (place,x,makeConst 0)])
          | (_, Reg x) =>
              (Type.Int,
               code @ [Mips.MOVE (place,x)])
          | (_, Addr x) =>
              (Type.Int,
               code @ [Mips.LW (place,x,makeConst 0)])
	end
    | S100.Assign (lval,e,p) =>
        let
          val t = "_assign_"^newName()
	  val (code0,ty,loc) = compileLval lval vtable ftable
	  val (_,code1) = compileExp e vtable ftable t
	in
	(*
		lVal = Exp	: Sætter lVal til værdien af Exp

		Char		: Lægger værdien af exp, t, ind i register x og place
		IntRef		: Henter word (32 bit) fra stak på adresse t og lægger ind i x.
				  Lægger værdi af t i place.
		CharRef		: Henter byte (8 bit) fra stak på adresse t og lægger ind i x.
				  Lægger værdi af t i place.
	*)
	  case (ty,loc) of
	    (Type.Int, Reg x) =>
	      (Type.Int,
	       code0 @ code1 @ [Mips.MOVE (x,t), Mips.MOVE (place,t)])
          | (Type.Char, Reg x) =>
              (Type.Int,
               code0 @ code1 @ [Mips.MOVE (x,t), Mips.MOVE (place,t)])
          | (Type.IntRef, Addr x) =>
              (Type.Int,
               code0 @ code1 @ [Mips.LW (x,t,makeConst 0), Mips.MOVE (place, t)])
          | (Type.CharRef, Addr x) =>
              (Type.Char,
               code0 @ code1 @ [Mips.LB (x,t,makeConst 0), Mips.MOVE (place, t)])
          | _ => raise Error ("Unknown assignment type",p)
	end
    | S100.Plus (e1,e2,pos) =>
        let
	  val t1 = "_plus1_"^newName()
	  val t2 = "_plus2_"^newName()
          val (ty1,code1) = compileExp e1 vtable ftable t1
          val (ty2,code2) = compileExp e2 vtable ftable t2
	in
	(*
		(Int, Int)     => Int
		(Int, IntRef)  => IntRef	: Returnerer adresse i base (intref) + word offset (int)
						  Ganger offset (Int) med word længden (4)
						  Lægger dette til adressen (t)
		(IntRef, Int)  => IntRef	: Som ovenfor
		(Int, CharRef) => CharRef	: Returnerer adresse i base (charref) + byte offset (int)
						  Lægger offset (Int) til adressen (t)
		(CharRef, Int) => CharRef	: Som ovenfor
	*)
	  case (ty1,ty2) of
	    (Type.Int, Type.Int) =>
	      (Type.Int,
	       code1 @ code2 @ [Mips.ADD (place,t1,t2)])
	    | (Type.Int, Type.IntRef) =>
	      (Type.IntRef,
	       code1 @ code2 @ [Mips.SLL (t1,t1,"2"), Mips.ADD(place, t1, t2)])
	    | (Type.IntRef, Type.Int) =>
	      (Type.IntRef,
	       code1 @ code2 @ [Mips.SLL (t1,t1,"2"), Mips.ADD(place, t1, t2)])
	    | (Type.Int, Type.CharRef) =>
	      (Type.CharRef,
	       code1 @ code2 @ [Mips.ADD (place,t1,t2)])
	    | (Type.CharRef, Type.Int) =>
	      (Type.CharRef,
	       code1 @ code2 @ [Mips.ADD (place,t1,t2)])
	    | (_, _) => 
		raise Error ("Type mismatch in assignment", pos)
	end
    | S100.Minus (e1,e2,pos) =>
        let
	  val t1 = "_minus1_"^newName()
	  val t2 = "_minus2_"^newName()
	  val t3 = "_slt_"^newName()
	  val l1 = "_noswap_"^newName()

          val (ty1,code1) = compileExp e1 vtable ftable t1
          val (ty2,code2) = compileExp e2 vtable ftable t2
	in
	(*
		(Int, Int) => Int		: Pre-implementeret
		(IntRef, Int)  => IntRef	: Returnerer adresse i base (intref) - word offset (int)
						  Ganger offset (Int) med word længden (4)
						  Trækker dette fra adressen (t)
		(CharRef, Int) => CharRef	: Returnerer adresse i base (charref) + byte offset (int)
						  Trækker offset (Int) fra adressen (t)
		(IntRef1, IntRef2)   => Int	: Returnerer antal words mellem intrefs
						  For at sikre at afstanden mellem de to words er positiv byttes de rundt hvis IntRef1 < IntRef2.
						  Trækker Intref1 fra Intref2 og resultatet i place. Dividerer place med word længden (4).
						  
		(CharRef1, CharRef2) => Int	: Returnerer antal bytes mellem charrefs
						  For at sikre at afstanden mellem de to CharRefs er positiv byttes de rundt hvis CharRef1 < CharRef2.
						  Trækker CharRef1 fra CharRef2 og resultatet i place. Dividerer place med word længden (4).
	*)
	  case (ty1,ty2) of
	    (Type.Int, Type.Int) =>
	      (Type.Int,
	       code1 @ code2 @ [Mips.SUB (place,t1,t2)])
	    | (Type.IntRef, Type.Int) =>
	      (Type.IntRef,
	       code1 @ code2 @ [Mips.SLL (t1,t1,"2"), Mips.SUB (place,t1,t2)])
	    | (Type.CharRef, Type.Int) =>
	      (Type.CharRef,
	       code1 @ code2 @ [Mips.SUB (place,t1,t2)])
	    | (Type.IntRef, Type.IntRef) =>
		(Type.Int,
	       	 code1 @ code2 @ [Mips.SLT (t3, t1, t2),
				  Mips.BNE (t3, "0", l1),
				  Mips.XOR (t1, t1, t2),
				  Mips.XOR (t2, t1, t2),
				  Mips.XOR (t1, t1, t2),
				  Mips.LABEL (l1),
				  Mips.SUB (place,t1,t2),
				  Mips.SRA (place, place, "2")])
	    | (Type.CharRef, Type.CharRef) =>
		(Type.Int,
	       	 code1 @ code2 @ [Mips.SLT (t3, t1, t2),
				  Mips.BNE (t3, "0", l1),
				  Mips.XOR (t1, t1, t2),
				  Mips.XOR (t2, t1, t2),
				  Mips.XOR (t1, t1, t2),
				  Mips.LABEL (l1),
				  Mips.SUB (place,t1,t2)])
	    | (_, _) => 
		raise Error ("Type mismatch in assignment", pos)
	end
    | S100.Less (e1,e2,pos) =>
        let
	  val t1 = "_less1_"^newName()
	  val t2 = "_less2_"^newName()
          val (ty1,code1) = compileExp e1 vtable ftable t1
          val (ty2,code2) = compileExp e2 vtable ftable t2
	in
	(* TODO
		exp1 < exp2	: Tjekker exp1 er skarpt mindre end exp2.
				  Returnerer 1 hvis sand, ellers 0

		int1, int2		: Sammenligner værdier. Returnerer 1 hvis int1 < int2
		char1, char2		: Sammenligner værdier. Returnerer 1 hvis char1 < char2
		intref1, intref2	: Sammenligner adresser. Returnerer 1 hvis inref1 < intref2
		charref1, charref2	: Sammenligner adresser. Returnerer 1 hvis charref1 < charref2
	*)
	  case (ty1,ty2) of
	      (Type.Int, Type.Int) =>
		(Type.Int, code1 @ code2 @ [Mips.SLT (place,t1,t2)])
	    | (Type.Char, Type.Char) =>
		(Type.Int, code1 @ code2 @ [Mips.SLT (place,t1,t2)])
	    | (Type.IntRef, Type.IntRef) =>
		(Type.Int, code1 @ code2 @ [Mips.SLT (place,t1,t2)])
	    | (Type.CharRef, Type.CharRef) =>
		(Type.Int, code1 @ code2 @ [Mips.SLT (place,t1,t2)])
	    | (_, _) => 
		raise Error ("Type mismatch in assignment", pos)
	end
    | S100.Equal (e1,e2,pos) =>
        let
	  val t1 = "_equal1_"^newName()
	  val t2 = "_equal2_"^newName()
	  val l = "_equal_branch_"^newName()
          val (ty1, code1) = compileExp e1 vtable ftable t1
          val (ty2, code2) = compileExp e2 vtable ftable t2
	in
	(* TODO
		exp1 < exp2	: Tjekker exp1 er skarpt mindre end exp2.
				  Returnerer 1 hvis sand, ellers 0

		int1, int2		: Sammenligner værdier. Returnerer 1 hvis int1 == int2
		char1, char2		: Sammenligner værdier. Returnerer 1 hvis char1 == char2
		intref1, intref2	: Sammenligner adresser. Returnerer 1 hvis inref1 == intref2
		charref1, charref2	: Sammenligner adresser. Returnerer 1 hvis charref1 == charref2
	*)
	  case (ty1,ty2) of
	      (Type.Int, Type.Int) =>
		(Type.Int, code1 @ code2 @ [Mips.LI (place,"0"),
					    Mips.BNE(t1, t2, l),
		                            Mips.LI(place,"1"),
		                            Mips.LABEL l])
	    | (Type.Char, Type.Char) =>
		(Type.Int, code1 @ code2 @ [Mips.LI (place,"0"),
					    Mips.BNE(t1, t2, l),
		                            Mips.LI(place,"1"),
		                            Mips.LABEL l])
	    | (Type.IntRef, Type.IntRef) =>
		(Type.Int, code1 @ code2 @ [Mips.LI (place,"0"),
					    Mips.BNE(t1, t2, l),
		                            Mips.LI(place,"1"),
		                            Mips.LABEL l])
	    | (Type.CharRef, Type.CharRef) =>
		(Type.Int, code1 @ code2 @ [Mips.LI (place,"0"),
					    Mips.BNE(t1, t2, l),
		                            Mips.LI(place,"1"),
		                            Mips.LABEL l])
	    | (_, _) => 
		raise Error ("Type mismatch in assignment", pos)

	end
    | S100.Call (f,es,pos) =>
	let
	  val rTy = case lookup f ftable of
		      SOME (_,t) => t
		    | NONE => raise Error ("unknown function "^f,pos)
	  val (code1,args) = compileExps es vtable ftable
	  fun moveArgs [] r = ([],[],0)
	    | moveArgs (arg::args) r =
	        let
		  val (code,parRegs,stackSpace) = moveArgs args (r+1)
		  val rname = makeConst r
		in
	          if r<=maxCaller then
		    (Mips.MOVE (rname,arg) :: code,
		     rname :: parRegs,
		     stackSpace)
		  else
		    (Mips.SW (arg,SP,makeConst stackSpace) :: code,
		     parRegs,
		     stackSpace + 4)
		end
	  val (moveCode, parRegs, stackSpace) = moveArgs args 2
	in
	  (rTy,
	   if stackSpace>0 then
	     [Mips.ADDI (SP,SP,makeConst (~stackSpace))]
	     @ code1 @ moveCode @
	     [Mips.JAL (f, parRegs),
	      Mips.MOVE (place,"2"),
	      Mips.ADDI (SP,SP,makeConst stackSpace)]
	   else
	     code1 @ moveCode @
	     [Mips.JAL (f, parRegs),
	      Mips.MOVE (place,"2")])
	end

  and compileExps [] vtable ftable = ([], [])
    | compileExps (e::es) vtable ftable =
        let
	  val t1 = "_exps_"^newName()
          val (_,code1) = compileExp e vtable ftable t1
	  val (code2, regs) = compileExps es vtable ftable
	in
	  (code1 @ code2, t1 :: regs)
	end

  and compileLval lval vtable ftable =
    case lval of
      S100.Var (x,p) =>
        (case lookup x vtable of
	   SOME (ty,y) => ([],ty,Reg y)
	 | NONE => raise Error ("Unknown variable "^x,p))
      | S100.Deref (x,p) => 
         (case lookup x vtable of
            SOME (ty,y) => ([],ty,Addr y)
          | NONE => raise Error ("Unknown variable "^x,p))            
      | S100.Lookup (x,e,p) => 
         let
             val t = "_exp_"^newName()
             val t2 = "_offset_"^newName()
             val t3 = "_add_"^newName()
             val t4 = "_lw_"^newName()

             val (_,code0) = compileExp e vtable ftable t
         in
             (case lookup x vtable of
                SOME (Type.IntRef,y) => (code0 @ 
                                         [Mips.SLL (t2,t,"2"), 
                                          Mips.ADD (t3,t2,y),
                                          Mips.LW (t4,t3,"0")],Type.IntRef,Addr t4)
              | SOME (Type.CharRef,y) => (code0 @
                                          [Mips.ADD (t3,t,y),
                                           Mips.LW (t4,t3,"0")],Type.CharRef,Addr t4)
              | SOME (_,_) => raise Error ("Invalid type "^x,p)
              | NONE => raise Error ("Unknown variable "^x,p))
         end

  fun compileStats [] vtable ftable exitLabel = []
    |   compileStats (s::ss) vtable ftable exitLabel = 
        let
          val codeS = compileStat s vtable ftable exitLabel
          val codeSS = compileStats ss vtable ftable exitLabel
        in
          codeS @ codeSS
        end
  and compileStat s vtable ftable exitLabel =
    case s of
      S100.EX e => #2 (compileExp e vtable ftable "0")
    | S100.If (e,s1,p) =>
        let
	  val t = "_if_"^newName()
	  val l1 = "_endif_"^newName()
	  val (_,code0) = compileExp e vtable ftable t
	  val code1 = compileStat s1 vtable ftable exitLabel
	in
	  code0 @ [Mips.BEQ (t,"0",l1)] @ code1 @ [Mips.LABEL l1]
	end
    | S100.IfElse (e,s1,s2,p) =>
        let
	  val t = "_if_"^newName()
	  val l1 = "_else_"^newName()
	  val l2 = "_endif_"^newName()
	  val (_,code0) = compileExp e vtable ftable t
	  val code1 = compileStat s1 vtable ftable exitLabel
	  val code2 = compileStat s2 vtable ftable exitLabel
	in
	  code0 @ [Mips.BEQ (t,"0",l1)] @ code1
	  @ [Mips.J l2, Mips.LABEL l1] @ code2 @ [Mips.LABEL l2]
	end
    | S100.While (e,s,p) =>
      let 
        val t = "_while_"^newName()
        val l1 = "_endwhile_"^newName()
        val (_,code0) = compileExp e vtable ftable t
        val code1 = compileStat s vtable ftable exitLabel
      in
	(*
		while exp do stats
		
		Indsæt start label
		Indsæt kode fra exp
		Tjek om exp (t) == 0 (false), hvis ja - hop til slut
		Ellers
			Kode for stats
			Hop til start
	*)
        [Mips.LABEL t] @ code0 @ [Mips.BEQ (t,"0",l1)] 
        @ code1 @ [Mips.J t, Mips.LABEL l1]
      end
    | S100.Return (e,p) =>
        let
	  val t = "_return_"^newName()
	  val (_,code0) = compileExp e vtable ftable t
	in
	(*
		return exp

		Evaluer exp (giver returværdi, t)
		Læg returværdi, t, i register 2 (v0)
		Hop til slut	
	*)
	  code0 @ [Mips.MOVE ("2",t), Mips.J exitLabel]
	end
    | S100.Block (ds,ss,p) => (* *)
	(*
		Som første del af CompileFun
	*)
      let
	fun moveArgs [] r = ([], [], 0)
	  | moveArgs ((t,ss)::ds) r =
	    moveArgs1 ss (Type.convertType t) ds r
	and moveArgs1 [] t ds r = moveArgs ds r
	  | moveArgs1 (s::ss) t ds r =
	    let
	      val y = newName ()
	      val (x,ty,loc) = (case s of
			          S100.Val (x,p) => (x, t, x^y)
				| S100.Ref (x,p) => (case (Type.convertTypeType t p) of 
                                                       S100.Int _ => (x, Type.IntRef, x^y)
                                                     | S100.Char _ => (x, Type.CharRef, x^y)))
	      val rname = Int.toString r
	      val (code, vtable, stackSpace) = moveArgs1 ss t ds (r+1)
	    in
	      if r<=maxCaller then
		(Mips.MOVE (loc, rname) :: code,
		 (x,(ty,loc)) :: vtable,
		 stackSpace)
	      else
		(Mips.LW (loc, FP, makeConst stackSpace) :: code,
		 (x,(ty,loc)) :: vtable,
		 stackSpace + 4)
	    end
 	val (parcode,vtable1,stackParams) (* move parameters to arguments *)
          = moveArgs ds 2
        val t1 = newName();
      in
	(*
		Opretter en blok (blok start label)
		Tilføjer decs (ds) til midlertidig vtable
		Compilerer Stats (ss) med den midlertidige vtable
		Slutter blok (blok slut label)
	*)
        [Mips.LABEL ("_block_begin_" ^ t1)] @
        compileStats ss (vtable1 @ vtable) ftable ("_block_exit_" ^ t1)
        @ [Mips.LABEL ("_block_exit_" ^ t1)] 
      end
      
      

  (* code for saving and restoring callee-saves registers *)
  fun stackSave currentReg maxReg savecode restorecode offset =
    if currentReg > maxReg
    then (savecode, restorecode, offset)  (* done *)
    else stackSave (currentReg+1)
                   maxReg
                   (Mips.SW (makeConst currentReg,
                                 SP,
                                 makeConst offset)
                    :: savecode) (* save register *)
                   (Mips.LW (makeConst currentReg,
                                 SP,
                                 makeConst offset)
                    :: restorecode) (* restore register *)
                   (offset+4) (* adjust offset *)


  (* compile function declaration *)
  and compileFun ftable (typ, sf, args, body, (line,col)) =
        let
	  val fname = Type.getName sf
	  val rty = Type.getType typ sf
	  fun moveArgs [] r = ([], [], 0)
	    | moveArgs ((t,ss)::ds) r =
	        moveArgs1 ss (Type.convertType t) ds r
	  and moveArgs1 [] t ds r = moveArgs ds r
	    | moveArgs1 (s::ss) t ds r =
	       let
		 val y = newName ()
		(*
			Udvidet således at der kendes forskel på Int og Ref

			S100.Ref	: Konverterer Type -> S100 type
		*)
	         val (x,ty,loc) = (case s of
			            S100.Val (x,p) => (x, t, x^y)
				  | S100.Ref (x,p) => (case (Type.convertTypeType t p) of 
                                                       S100.Int _ => (x, Type.IntRef, x^y)
                                                     | S100.Char _ => (x, Type.CharRef, x^y)))
		 val rname = Int.toString r
		 val (code, vtable, stackSpace) = moveArgs1 ss t ds (r+1)
	       in
		   if r<=maxCaller then
		     (Mips.MOVE (loc, rname) :: code,
		      (x,(ty,loc)) :: vtable,
		      stackSpace)
		   else
		     (Mips.LW (loc, FP, makeConst stackSpace) :: code,
		      (x,(ty,loc)) :: vtable,
		      stackSpace + 4)
	       end
	  val (parcode,vtable,stackParams) (* move parameters to arguments *)
            = moveArgs args 2
          val body = compileStat body vtable ftable (fname ^ "_exit")
          val (body1, _, maxr,spilled)  (* call register allocator *)
            = RegAlloc.registerAlloc
                (parcode @ body) [] 2 maxCaller maxReg 0
          val (savecode, restorecode, offset) = (* save/restore callee-saves *)
                stackSave (maxCaller+1) (maxr+1) [] [] (4*spilled)
		(* save one extra callee-saves register for saving SP *)
	  val ctext = if spilled>0
		  then "Spill of "^makeConst spilled ^ " variables occurred"
		  else ""
        in
            [Mips.COMMENT ctext,
             Mips.LABEL fname]  (* function label *)
	  @ (if stackParams>0 then [Mips.MOVE (FP,SP)] else [])
	  @ [Mips.ADDI (SP,SP,makeConst (~4-offset)), (* move SP down *)
             Mips.SW (RA, SP, makeConst offset)] (* save return address *)
          @ savecode  (* save callee-saves registers *)
          @ body1  (* code for function body *)
	  @ [Mips.LABEL (fname^"_exit")] (* exit label *)
          @ restorecode  (* restore callee-saves registers *)
          @ [Mips.LW (RA, SP, makeConst offset), (* restore return addr *)
             Mips.ADDI (SP,SP,makeConst (offset+4)), (* move SP up *)
             Mips.JR (RA, [])] (* return *)
        end

  (* compile program *)
  fun compile funs =
    let
      val ftable =
	  Type.getFuns funs [("getint",([],Type.Int)),
			     ("putint",([Type.Int],Type.Int)),
                             ("walloc",([Type.Int],Type.IntRef)),
                             ("balloc",([Type.Int],Type.CharRef)),
                             ("getstring",([Type.Int],Type.CharRef)),
                             ("putstring",([Type.CharRef],Type.CharRef))]
      val funsCode = List.concat (List.map (compileFun ftable) funs)
    in
      [Mips.TEXT "0x00400000",
       Mips.GLOBL "main",
       Mips.LA (HP, "_heap_")]    (* initialise heap pointer *)
      @ [Mips.JAL ("main",[]),    (* run program *)
	 Mips.LI ("2","10"),      (* syscall control = 10 *)
         Mips.SYSCALL]            (* exit *)
      @ funsCode		  (* code for functions *)

      @ [Mips.LABEL "putint",     (* putint function *)
	 Mips.ADDI(SP,SP,"-8"),
	 Mips.SW ("2",SP,"0"),    (* save used registers *)
	 Mips.SW ("4",SP,"4"),
	 Mips.MOVE ("4","2"),
	 Mips.LI ("2","1"),       (* print_int syscall *)
	 Mips.SYSCALL,
	 Mips.LI ("2","4"),       (* print_string syscall *)
	 Mips.LA("4","_cr_"),
	 Mips.SYSCALL,            (* write CR *)
	 Mips.LW ("2",SP,"0"),    (* reload used registers *)
	 Mips.LW ("4",SP,"4"),
	 Mips.ADDI(SP,SP,"8"),
	 Mips.JR (RA,[]),

	 Mips.LABEL "getint",     (* getint function *)
	 Mips.LI ("2","5"),       (* read_int syscall *)
	 Mips.SYSCALL,
	 Mips.JR (RA,[]),

         Mips.LABEL "walloc",
         Mips.LI ("4","4"),
         Mips.LI ("2","9"),       (* sbrk service call *)
         Mips.SYSCALL,
         Mips.JR (RA,[]),

         Mips.LABEL "balloc",
         Mips.LI ("4","3"),       (* argument value *)
         Mips.MOVE ("8", "4"),    
         Mips.LABEL "_remaind_",     (* loop divides the argument by 4 *)
         Mips.ADDI ("9", "0", "1"),  (* it is used to calculate the minimum *)                                      
         Mips.SLT ("10", "8", "9"),  (* sized argument that is word aligned *)
         Mips.BNE ("10", "0", "_remaind_exit"),
         Mips.ADDI ("11","11","1"),
         Mips.ADDI ("12", "0", "4"),
         Mips.SUB ("8", "8", "12"),
         Mips.J "_remaind_",
         Mips.LABEL "_remaind_exit",
         Mips.SLL ("11","11","2"), 

         Mips.LI ("2", "9"),      (* sbrk service call *)
         Mips.SYSCALL,
         Mips.JR (RA,[]),

	 Mips.DATA "",
	 Mips.ALIGN "2",
	 Mips.LABEL "_cr_",       (* carriage return string *)
	 Mips.ASCIIZ "\n",
	 Mips.ALIGN "2",

	 Mips.LABEL "_heap_",     (* heap space *)
	 Mips.SPACE "100000"]
    end

end
