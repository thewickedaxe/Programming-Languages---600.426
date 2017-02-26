module Language = struct
  
  let name = "AFbV"
  module Parser = Afbvparser
  module Lexer = Afbvlexer
  module Ast = Afbvast
  module Pp = Afbvpp
  module Options = Afbvoptions
  module Interpreter = Afbvinterp
	module Typechecker = Afbvtype

end;;

module Application = Application.Make(Language);;

Application.main ();;
