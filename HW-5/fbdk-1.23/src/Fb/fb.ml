module Language = struct
  
  let name = "Fb"
  module Parser = Fbparser
  module Lexer = Fblexer
  module Ast = Fbast
  module Pp = Fbpp
  module Options = Fboptions
  module Interpreter = Fbinterp
	module Typechecker = Fbtype

end;;

module Application = Application.Make(Language);;

Application.main ();;
