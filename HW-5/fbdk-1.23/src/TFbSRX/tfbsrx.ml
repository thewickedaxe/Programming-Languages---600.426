module Language = struct
  
  let name = "TFbSRX"
  module Parser = Tfbsrxparser
  module Lexer = Tfbsrxlexer
  module Ast = Tfbsrxast
  module Pp = Tfbsrxpp
  module Options = Tfbsrxoptions
  module Interpreter = Tfbsrxinterp
	module Typechecker = Tfbsrxtype

end;;

module Application = Application.Make(Language);;

Application.main ();;
