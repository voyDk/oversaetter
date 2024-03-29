signature Type =
sig

  datatype Type = Int | Char | IntRef | CharRef

  exception Error of string*(int*int)

  val checkProg : S100.Prog -> unit

  val getFuns : S100.Prog -> (string * (Type list * Type)) list
		          -> (string * (Type list * Type)) list

  val convertS100Type : S100.Type -> Type
  val convertTypeType : Type -> int*int -> S100.Type 		
  val getType : S100.Type -> S100.Sid -> Type
  val getName : S100.Sid -> string

end
