let clear_screen_ansi () : unit =
  print_string "\x1b[2J\x1b[H";
  flush stdout

let rec count_width lines =
  match lines with
  | [] -> 0
  | [ line ] -> String.length line
  | line :: rest -> Int.max (String.length line) (count_width rest)

let cubby_hole contents =
  (** The width of the provided contents in bytes? *)
  let width = count_width contents in
  (* BUG: the horizontal line is not a valid character, it must be treated as a string *)
  print_endline ("\x1b[31m┏" ^ (String.make width '━') ^ "┓\x1b[0m");
  print_endline (Int.to_string width);
  print_endline "\x1b[31m┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\x1b[0m"

(* \x1b[0m is the closing tag *)

(* \x1b[1m\x1b[92mHello, Cubby\x1b[0m *)

(* Box drawing characters are a thing: https://en.wikipedia.org/wiki/Box-drawing_characters *)
let render () =
  cubby_hole [
    "Hello, Cubby"
  ]

let () = 
  clear_screen_ansi ();
  render ();
  print_endline "\x1b[37mTest"
