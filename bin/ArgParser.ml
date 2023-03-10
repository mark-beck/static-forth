type t = { files : string list; verbose : bool; repl : bool }

let usage_msg = Resources.name ^ " " ^ Resources.version ^ " [-v] [-r] <file1> [<file2>] ..."
let verbose = ref false
let repl = ref false
let input_files = ref []


let anon_fun filename =
  input_files := filename::!input_files

let speclist =
  [ ("-v", Arg.Set verbose, "Output debug information");
    ("-r", Arg.Set repl, "Start REPL")]

let parse () =
  Arg.parse speclist anon_fun usage_msg;
  { files = List.rev !input_files; verbose = !verbose; repl = !repl }