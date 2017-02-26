
type options_type = { mutable debug : bool }

let options = { debug = false }



let get_debug () = options.debug

let set_debug v = options.debug <- v