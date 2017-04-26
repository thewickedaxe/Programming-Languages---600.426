(* This exception is used to allow a language to dynamically signal that it *)
(* does not have a typechecker.  If it is raised, it should be raised by a *)
(* Typechecker module's typecheck function. *)
exception TypecheckerNotImplementedException

module type LANGUAGE = sig
  val name: string

  module Ast: sig
    type expr
		type fbtype
  end

  module Parser: sig
    type token
    val main: 
     (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast.expr
  end

  module Lexer: sig
    val token: Lexing.lexbuf -> Parser.token
  end

	module Typechecker: sig
		val typecheck: Ast.expr -> Ast.fbtype
		val typecheck_default_enabled: bool
	end
    
  module Options: sig
    val set_debug: bool -> unit
    val get_debug: unit -> bool
  end

  module Pp: sig
    val pretty_print: Ast.expr -> string
    val pp: Ast.expr -> string -> string
		val pretty_print_type: Ast.fbtype -> string
		val pp_type: Ast.fbtype -> string -> string
  end
	
  module Interpreter: sig
    val eval: Ast.expr -> Ast.expr
  end
end;;

