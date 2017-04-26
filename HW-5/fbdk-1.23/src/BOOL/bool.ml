module Language = struct
  
  let name = "BOOL"
  module Parser = Boolparser
  module Lexer = Boollexer
  module Ast = Boolast
  module Pp = Boolpp
  module Options = Booloptions
  module Interpreter = Boolinterp
	module Typechecker = Booltype

end;;

module Application = Application.Make(Language);;

Application.main ();;
