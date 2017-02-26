module type S =
sig
	val main: unit -> unit
end

module Make(Lang: Fbdk.LANGUAGE) =
struct
	
	let toplevel_loop typechecking_enabled show_types =
        (* Prints exceptions and associated stack traces *)
        let print_exception ex = 
            Printf.printf "Exception: %s\n" (Printexc.to_string ex);
            Printexc.print_backtrace stdout;
            flush stdout
        in
        (* Parse the stdin. Our lexers are set up (manually) to generate an Exit *)
        (* exception when Eof is encountered. We abort the top loop in this case. *)
        (* Other parse errors are caught and reported, but we dont abort the top loop *)
        (* TODO: As noted currently it is the lexers themselves that generate this exception. It *)
        (* feels better to have the parsers do this instead. It wont change anything here. But *)
        (* changes are required in each lexer and parser to produce explicit tokens and do the *)
        (* raise *)
        let safe_parse () = 
            try
                let lexbuf = Lexing.from_channel stdin in
                Some ( Lang.Parser.main Lang.Lexer.token lexbuf )
            with   Exit -> exit 0  
                 | ex -> print_exception ex; None
        in
        (* Type check if enabled and return the result. The result is a false *)
        (* only if it is enabled and type checking throws an exception (fails) *)   
        let safe_typecheck ast =
            try
				if typechecking_enabled then
                    let exprtype = Lang.Typechecker.typecheck ast in
					if show_types then
                        ( Printf.printf " : %s\n" (Lang.Pp.pp_type exprtype "    ") ; true )
					else
                        true
                else
                    true
            with 
                  Fbdk.TypecheckerNotImplementedException -> true
                | ex -> print_exception ex ; false
        in
        (* Interpret and print. Exceptions are caught and reported. But the toploop is not aborted *)
        let safe_interpret_and_print ast =
            try
                let result = Lang.Interpreter.eval ast in
                Printf.printf "==> %s\n" (Lang.Pp.pp result "    ");
            with ex -> 
                print_exception ex
        in
		Printf.printf "\t%s version %s\t"
			Lang.name Version.version;
		Printf.printf "\t(typechecker %s)\n\n"
			(if typechecking_enabled then "enabled" else "disabled");
		flush stdout;
		while true do
			Printf.printf "# ";
			flush stdout;
			let parse_result = safe_parse () in
            match parse_result with 
                  None -> ()
                | Some ast -> 
                    if ( safe_typecheck ast ) then safe_interpret_and_print ast else ()
			;
            flush stdout
		done
        
	
	let run_file filename =
		let fin = open_in filename in
		let lexbuf = Lexing.from_channel fin in
		let ast = Lang.Parser.main Lang.Lexer.token lexbuf in
		let result = Lang.Interpreter.eval ast in
		Printf.printf "%s\n" (Lang.Pp.pretty_print result);
		flush stdout
	
	let print_version () =
		Printf.printf "%s version %s\nBuild Date: %s\n"
			Lang.name Version.version Version.date
	
	let main () =
		let filename = ref "" in
		let toplevel = ref true in
		let version = ref false in
		let no_typechecking =
			ref (not Lang.Typechecker.typecheck_default_enabled) in
		let no_type_display = ref false in
        let show_exception_stack_trace = ref false in
        let debug = ref false in
		Arg.parse
			[("--version",
				Arg.Set(version),
				"show version information");
            ("--typecheck",
			  Arg.Clear(no_typechecking),
				"enable typechecking");
			("--no-typecheck",
				Arg.Set(no_typechecking),
				"disable typechecking");
			("--hide-types",
			  Arg.Set(no_type_display),
				"disable displaying of types");
            ("--show-backtrace",
				Arg.Set(show_exception_stack_trace),
				"Enable the display of exception stack traces");
            ("--debug",
				Arg.Set(debug),
				"Enable debug (only applies to AFbV currently)")]
			(function fname ->
						filename := fname;
						version := false;
						toplevel := false)
			("Usage: " ^
				Lang.name ^
				" [ options ] [ filename ]\noptions:");
		
        Printexc.record_backtrace (!show_exception_stack_trace) ;
        
        Lang.Options.set_debug !debug ;
        
        if !version then
            print_version ()
        else if !toplevel then
            toplevel_loop (not (!no_typechecking)) (not (!no_type_display))
        else
            run_file !filename
end
