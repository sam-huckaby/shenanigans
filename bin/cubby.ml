let clear_screen_ansi () : unit =
  print_string "\x1b[2J\x1b[H";
  flush stdout

let rec count_width lines =
  match lines with
  | [] -> 0
  | [ line ] -> String.length line
  | line :: rest -> Int.max (String.length line) (count_width rest)

let rec make_unicode_string count str =
  match count with
    | 0 -> str ^ str
    | _ -> str ^ (make_unicode_string (count - 1) str)

let print_in_cubby pad str =
  print_endline ("\x1b[31m┃\x1b[0m" ^ (String.make pad ' ') ^ str ^ (String.make pad ' ') ^ "\x1b[31m┃\x1b[0m")

let cubby_hole contents =
  let width = count_width contents in
  print_endline ("\x1b[31m┏" ^ (make_unicode_string width "━") ^ "┓\x1b[0m");
  List.iter (print_in_cubby 1) contents;
  print_endline ("\x1b[31m┗" ^ (make_unicode_string width "━") ^ "┛\x1b[0m")

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
