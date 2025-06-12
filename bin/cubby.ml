let clear_screen_ansi () : unit =
  print_string "\x1b[2J\x1b[H";
  flush stdout

(* \x1b[0m is the closing tag *)

(* Box drawing characters are a thing: https://en.wikipedia.org/wiki/Box-drawing_characters *)
let render () =
  print_endline "\x1b[31m┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\x1b[0m";
  print_endline "\x1b[1m \x1b[92mHello, Cubby\x1b[0m";
  print_endline "\x1b[31m┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\x1b[0m"

let () = 
  clear_screen_ansi ();
  render ();
  print_endline "\x1b[37mTest"
