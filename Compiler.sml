(* Compiler for Cat *)
(* Compile by mosmlc -c Compiler.sml *)
(* Then recompile CC by mosmlc CC.sml -o CC *)

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

  fun lookup x [] pos = raise Error ("Name "^x^" not found", pos)
    | lookup x ((y,v)::table) pos = if x=y then v else lookup x table pos

  fun isIn x [] = false
    | isIn x (y::ys) = x=y orelse isIn x ys

  (* link register *)
  val RA = "31"
  (* Register for stack pointer *)
  val SP = "29"
  (* Register for heap pointer *)
  val HP = "28"

  (* Suggested register division *)
  val maxCaller = 15   (* highest caller-saves register *)
  val maxReg = 26      (* highest allocatable register *)

  (* compile pattern *)
  fun compilePat p v vtable fail =
    case p of
      Cat.NumP (n,pos) =>
        let
	      val t = "_constPat_"^newName()
        in
          if n<32768 then
	        ([Mips.LI (t, makeConst n),
	        Mips.BNE (v,t,fail)],
	        vtable)
	      else
	        ([Mips.LUI (t, makeConst (n div 65536)),
	        Mips.ORI (t, t, makeConst (n mod 65536)),
	        Mips.BNE (v,t,fail)],
	        vtable)
	    end
		
    | Cat.VarP (x,pos) =>
        let
          val xt = "_patVar_"^x^"_"^newName()
        in
          ([Mips.MOVE (xt,v)], (x,xt)::vtable)
        end

	(* Bool true pattern: matcher på værdien 1 *)
	| Cat.TrueP pos =>
        let
	      val t = "_constPat_"^newName()
        in
	      ([Mips.LI (t, "1"), Mips.BNE (v,t,fail)], vtable)
	    end
		
	(* Bool false pattern: matcher på værdien 0 *)
    | Cat.FalseP pos =>
		([Mips.BNE (v,"0",fail)], vtable)
		
	(* Null pattern: matcher på værdien 0 *)
    | Cat.NullP pos =>
		([Mips.BNE (v,"0",fail)], vtable)
		
	(* Tuppel pattern: checker først om værdien er null (v = 0)
	   Hvis ikke, køres tuplens patterns igennem en for en, værdierne
	   hentes fra hoben og de enkelte patterns checked rekursivt. *)
    | Cat.TupleP (ps,pos) => 
		let
			fun compileTuple [] v n vtable = ([], vtable)
			  | compileTuple (p::ps) v n vtable = 
				let
					val t = "_tuplePat_"^newName()
					val code1 = [Mips.LW (t, v, makeConst n)]
					val (code2,vt1) = compilePat p t vtable fail
					val (code3,vt2) = compileTuple ps v (n+4) vt1
				in
					(code1 @ code2 @ code3, vt1 @ vt2)
				end
		in
			let 
				val (code,vt) = (compileTuple ps v 0 vtable) 
			in
				([Mips.BEQ (v, "0", fail)] @ code, vt)
			end
		end
		
	
  (* compile expression *)
  fun compileExp e vtable place =
	case e of
		Cat.Num (n,pos) =>
			if n<32768 then
				[Mips.LI (place, makeConst n)]
			else
				[Mips.LUI (place, makeConst (n div 65536)),
				 Mips.ORI (place, place, makeConst (n mod 65536))]
				 
		| Cat.Var (x,pos) => 
			[Mips.MOVE (place, lookup x vtable pos)]

		(* True: gemmer værdien 1 i place *)	
		| Cat.True pos =>
			[Mips.LI (place, "1")]
			
		(* False: gemmer værdien 0 i place *)	
		| Cat.False pos =>
			[Mips.LI (place, "0")]
		
		(* Null: gemmer værdien 0 i place *)	
		| Cat.Null (x,pos) => 
			[Mips.LI (place, "0")]
		
		(* Tuple: gemmer først den aktuelle hobpointer i place og pointeren
		   flyttes frem for at gøre plads til tuplen på hoben. Tuplens udtryk 
		   oversættes så rekursivt og deres værdier gemmes i hoben. *)
		| Cat.MkTuple (es, x, pos) =>
			let
				val t = "_tuple_"^x^"_"^newName()
				val n = length es
				fun compileTuple [] hs n = []
				  | compileTuple (e::es) hs n =
						let
							val code = compileExp e vtable t
						in
							code @ [Mips.SW (t, hs, makeConst n)]
							@ compileTuple es hs (n+4)
						end
			in
				  [Mips.MOVE (place, HP),
				   Mips.ADDI (HP, HP, makeConst (4 * n))]
				@ (compileTuple es place 0)
			end

		(* Let-in: udtrykkets deklarationer oversættes via compileDec.
		   Hvis deklarationens pattern er en variabel, oversættes det 
		   tilhørende udtryk og variabel+værdi tilføjes til vtable.
		   Hvis det er en tuppel, oversættes dennes patterns rekursivt.
		   Andre patterns ignoreres, da disse er er konstante værdier.
		   Til slut oversættes kroppen med det resulterende vtable. *)
		| Cat.Let (ds, e, pos) =>
			let
				fun compileDec [] vtable = ([], vtable)
				  | compileDec ((p,e,pos)::ds) vtable =
						case p of
						  Cat.VarP (x,pos) => 
							let
								val t = "_let_var_"^x^"_"^newName()
								val code1 = compileExp e vtable t
								val vtable1 = (x,t)::vtable
								val (code2, vtable2) = compileDec ds vtable1
							in
								(code1 @ code2, vtable2)
							end
						| Cat.TupleP (ps,pos) => 
							(case e of
							  Cat.MkTuple (es, x, pos) => 
								let
									fun fd (p::ps,e::es) = (p,e,pos)::fd(ps,es)
									  | fd _ = []
									val tds = fd (ps,es)
									val (code1, vt1) = compileDec tds vtable
									val (code2, vt2) = compileDec ds vt1
								in
									(code1 @ code2, vt2)
								end
							| _ => raise Error ("Bad tuple!", pos))
							
						| _ => compileDec ds vtable

				val (code1, vtable1) = compileDec ds vtable
			in
				 code1 @ (compileExp e vtable1 place)
			end
		
		| Cat.Plus (e1,e2,pos) =>
			let
				val t1 = "_plus1_"^newName()
				val t2 = "_plus2_"^newName()
				val code1 = compileExp e1 vtable t1
				val code2 = compileExp e2 vtable t2
			in
				code1 @ code2 @ [Mips.ADD (place,t1,t2)]
			end
			
		| Cat.Minus (e1,e2,pos) =>
			let
				val t1 = "_minus1_"^newName()
				val t2 = "_minus2_"^newName()
				val code1 = compileExp e1 vtable t1
				val code2 = compileExp e2 vtable t2
			in
				code1 @ code2 @ [Mips.SUB (place,t1,t2)]
			end
		
		(* And: place sættes først til 1. Efter hvert udtryk checkes om 
		   resultatet er true eller false, hvis false hoppes der til slut.
		   Dvs. det andet udtryk evalueres kun hvis det første er sandt.
		   Hvis der ikke hoppes ved det andet udtryk sættes place til 0. *)	
		| Cat.And (e1,e2,pos) =>
			let
				val t = "_and_"^newName()
				val code1 = compileExp e1 vtable t
				val code2 = compileExp e2 vtable t
				val l = "_and_exit_"^newName()
			in
				  code1 
				@ [Mips.LI (place, "0"),
				   Mips.BEQ (t,"0",l)]
				@ code2 
				@ [Mips.BEQ (t,"0",l),
				   Mips.LI (place, "1"),
				   Mips.LABEL l]
			end
		
		(* Or: place sættes først til 1. Hvis det første udtryk er sandt, 
		   hoppes til slut. Hvis det er falsk, sættes place til 0 og det 
		   andet udtryk checkes. Hvis dette også er falsk hoppes til slut,
		   ellers sættes place til 1. *)	
		| Cat.Or (e1,e2,pos) =>
			let
				val t = "_or_"^newName()
				val code1 = compileExp e1 vtable t
				val code2 = compileExp e2 vtable t
				val l = "_or_exit_"^newName()
			in
				  code1 
				@ [Mips.LI (place, "1"),
				   Mips.BNE (t,"0",l),
				   Mips.LI (place, "0")]
				@ code2 
				@ [Mips.BEQ (t,"0",l),
				   Mips.LI (place, "1"),
				   Mips.LABEL l]
			end
		
		(* Not: place sættes først til 0. Hvis udtrykket er sandt hoppes til 
		   slut, ellers sættes place til 1. *)	
		| Cat.Not (e,pos) =>
			let
				val t = "_not_"^newName()
				val code = compileExp e vtable t
				val l = "_not_branch_"^newName()
			in
				code 
				@ [Mips.LI (place,"0"),
				   Mips.BNE (t,"0",l),
				   Mips.LI (place,"1"),
				   Mips.LABEL l]
			end
		
		(* Equal: place sættes først til 0. Hvis udtryk 1 ikke er lig udtryk 2 
		   hoppes til slut, eller sættes place til 1. *)	
		| Cat.Equal (e1,e2,pos) =>
			let
				val t1 = "_equal1_"^newName()
				val t2 = "_equal2_"^newName()
				val code1 = compileExp e1 vtable t1
				val code2 = compileExp e2 vtable t2
				val l = "_equal_branch_"^newName()
			in
                          [Mips.SEQ (place,t1,t2)]
(*
				code1 @ code2 
				@ [Mips.LI (place,"0"),
				   mips.BNE (t1,t2,l),
				   Mips.LI (place,"1"),
				   Mips.LABEL l]
*)
			end
		
		(* Less: place sættes blot til slt udført på de to udtryk. *)	
		| Cat.Less (e1,e2,pos) =>
			let
				val t1 = "_less1_"^newName()
				val t2 = "_less2_"^newName()
				val code1 = compileExp e1 vtable t1
				val code2 = compileExp e2 vtable t2
			in
				code1 @ code2 @ [Mips.SLT (place,t1,t2)]
			end
		
		(* If-then-else: betingelsen (e1) evalueres og hvis resultatet (t1) 
		   er falsk hoppes til else-udtrykkes der evalueres og derefter går
		   til slut. Hvis t1 er sand evalueres then-udtrykket og der hoppes 
		   derefter til slut. *)	
		| Cat.If (e1,e2,e3,pos) =>
			let
				val t1 = "_if_"^newName()
				val t2 = "_then_"^newName()
				val t3 = "_else_"^newName()
				val code1 = compileExp e1 vtable t1
				val code2 = compileExp e2 vtable place
				val code3 = compileExp e3 vtable place
				val l1 = "_elseexp_"^newName()
				val l2 = "_endif_"^newName()
			in
				code1 
				@ [Mips.BEQ ("0",t1,l1)]
				@ code2
				@ [Mips.J l2,
				   Mips.LABEL l1]
				@ code3
				@ [Mips.LABEL l2]
			end
		
		(* Case-of: Først evalueres case-argumentet og resultatet gemmes i t.
		   Patterns oversættes så med compileMatch der kaldes med t værdien.
		   Hvis der ikke findes et match, hoppes til failLabel hvor der dannes 
		   en runtime error, ellers bliver match-resultatet gemt i place og 
		   der hoppes til endLabel. *)	
		| Cat.Case (e,ms,pos) =>
			let
				val t = "_case_arg_"^newName()
				val endLabel = "_case_end_"^newName()
				val failLabel = "_fail_"^newName()
				val code1 = compileExp e vtable t
				val code2 = compileMatch ms t place endLabel failLabel vtable
				val (line,_) = pos
				val errorCode = 
					[Mips.LABEL failLabel,
					 Mips.LI ("5",makeConst line),
					 Mips.J "_Error_"] 
			in
				code1 @ code2 @ errorCode @ [Mips.LABEL endLabel]
			end
			

		| Cat.Apply (f,e,pos) =>
			let
				val t1 = "_apply_"^newName()
				val code1 = compileExp e vtable t1
			in
				code1 @
				[Mips.MOVE ("2",t1), 
				 Mips.JAL (f,["2"]), 
				 Mips.MOVE (place,"2")]
			end
			
		| Cat.Read pos =>
			[Mips.LI ("2","5"), (* read_int syscall *)
			 Mips.SYSCALL,
			 Mips.MOVE (place,"2")]
			 
		| Cat.Write (e,pos) =>
			compileExp e vtable place
			@ [Mips.MOVE ("4",place),
			   Mips.LI ("2","1"),  (* write_int syscall *)
			   Mips.SYSCALL,
			   Mips.LA ("4","_cr_"),
			   Mips.LI ("2","4"),  (* write_string syscall *)
			   Mips.SYSCALL]
		
  and compileMatch [] arg res endLabel failLabel vtable = [Mips.J failLabel]
    | compileMatch ((p,e)::m) arg res endLabel failLabel vtable =
        let
			val next = "_match_"^newName()
			val (code1, vtable1) = compilePat p arg vtable next
			val code2 = compileExp e vtable1 res
			val code3 = compileMatch m arg res endLabel failLabel vtable
		in
			code1 
			@ code2 
			@ [Mips.J endLabel, 
			   Mips.LABEL next] 
			@ code3
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
                   (offset-4) (* adjust offset *)


  (* compile function declaration *)
  and compileFun (fname, argty, resty, m, (line,col)) =
		let
			val atmp = fname ^"_arg_"^ newName()
			val rtmp = fname ^"_res_"^ newName()
			val exit = fname ^"_return_"^ newName()
			val fail = fname ^"_fail_"^ newName()
			val parcode = 
				[Mips.MOVE (atmp, "2")] (* move R2 to argument *)
			val returncode = 
				[Mips.LABEL exit, 
				 Mips.MOVE ("2",rtmp)] (* move return value to R2 *)
			val errorcode = 
				[Mips.LABEL fail,
				 Mips.LI ("5",makeConst line),
				 Mips.J "_Error_"] (* if match fails *)
			val body = compileMatch m atmp rtmp exit fail []
			val (body1, _, maxr) = (* call register allocator *)
				RegAlloc.registerAlloc (parcode @ body @ returncode) ["2"] 2 maxCaller maxReg 
			val (savecode, restorecode, offset) = (* save/restore callee-saves *)
				stackSave (maxCaller+1) maxr [] [] (~8)
        in
            [Mips.COMMENT "",
             Mips.LABEL fname,  (* function label *)
             Mips.SW (RA, SP, "-4")] (* save return address *)
          @ savecode  (* save callee-saves registers *)
          @ [Mips.ADDI (SP,SP,makeConst offset)] (* move SP down *)
          @ body1  (* code for function body *)
          @ [Mips.ADDI (SP,SP,makeConst (~offset))] (* move SP up *)
          @ restorecode  (* restore callee-saves registers *)
          @ [Mips.LW (RA, SP, "-4"), (* restore return addr *)
             Mips.JR (RA, [])] (* return *)
		  @ errorcode
        end
  
  (* compile program *)
  fun compile (tys, funs, e) =
    let
      val funsCode = List.concat (List.map compileFun funs)
      val mainCode = compileExp e [] "dead" @ [Mips.J "_stop_"]
      val (code1, _, _)
             = RegAlloc.registerAlloc mainCode [] 2 maxCaller maxReg
    in
      [Mips.TEXT "0x00400000",
       Mips.GLOBL "main",
       Mips.LABEL "main",
       Mips.LA (HP, "_heap_")]    (* initialise heap pointer *)
      @ code1                     (* run program *)
      @ funsCode		  (* code for functions *)
      @ [Mips.LABEL "_stop_",
         Mips.LI ("2","10"),      (* syscall control = 10 *)
         Mips.SYSCALL,            (* exit *)
         Mips.LABEL "_Error_",    (* code for reporting match errors *)
	 Mips.LA ("4","_ErrorString_"),
	 Mips.LI ("2","4"), Mips.SYSCALL, (* print string *)
	 Mips.MOVE ("4","5"),
	 Mips.LI ("2","1"), Mips.SYSCALL, (* print line number *)
	 Mips.LA ("4","_cr_"),
	 Mips.LI ("2","4"), Mips.SYSCALL, (* print CR *)
	 Mips.J "_stop_",
	 Mips.DATA "",
	 Mips.LABEL "_cr_",       (* carriage return string *)
	 Mips.ASCIIZ "   \n",
	 Mips.LABEL "_ErrorString_",
	 Mips.ASCIIZ "Match failed near line ",
	 Mips.ALIGN "2",
	 Mips.LABEL "_heap_",
	 Mips.SPACE "100000"]
    end

end
