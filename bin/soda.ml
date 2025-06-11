let clear_screen_ansi () : unit =
  print_string "\x1b[2J\x1b[H";
  flush stdout

(* Box drawing characters are a thing: https://en.wikipedia.org/wiki/Box-drawing_characters *)
let render () =
  print_endline "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓";
  print_endline "\x1b[1m\x1b[92mHello, Soda\x1b[92m";
  print_endline "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"

let () = 
  clear_screen_ansi ();
  render ();
  print_endline "\x1b[37mTest"
