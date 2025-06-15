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

let _centered_in_cubby pad width str =
  let front_pad = pad + Float.to_int (Float.floor (((Float.of_int width) -. (Float.of_int (String.length str))) /. 2.0)) in
  let end_pad = pad + Float.to_int (Float.ceil (((Float.of_int width) -. (Float.of_int (String.length str))) /. 2.0)) in
  print_endline ("\x1b[31m┃\x1b[0m" ^ (String.make front_pad ' ') ^ str ^ (String.make end_pad ' ') ^ "\x1b[31m┃\x1b[0m")

let left_in_cubby pad width str =
  let front_pad = pad in
  let end_pad = pad + (width - String.length str) in
  print_endline ("\x1b[31m┃\x1b[0m" ^ (String.make front_pad ' ') ^ str ^ (String.make end_pad ' ') ^ "\x1b[31m┃\x1b[0m")

let _right_in_cubby pad width str =
  let front_pad = pad + (width - String.length str) in
  let end_pad = pad in
  print_endline ("\x1b[31m┃\x1b[0m" ^ (String.make front_pad ' ') ^ str ^ (String.make end_pad ' ') ^ "\x1b[31m┃\x1b[0m")

let checkbox_line width selected index str =
  let box = if selected = index then "[X]" else "[ ]" in
  left_in_cubby 1 width (box ^ str)

let checkbox_list options selected =
  let width = count_width options in
(* BUG: Future Sam, the error being thrown when the program is run appears to happen when String.make is given a negative int value *)
  print_endline (Int.to_string width);
  List.iteri (checkbox_line width selected) options

(** Create a new cubby hole with the given contents *)
let cubby_hole contents =
  let width = count_width contents in
  print_endline ("\x1b[31m┏" ^ (make_unicode_string width "━") ^ "┓\x1b[0m");
  (*List.iter (left_in_cubby 1 width) contents;*)
  checkbox_list contents 1;
  print_endline ("\x1b[31m┗" ^ (make_unicode_string width "━") ^ "┛\x1b[0m")

(* \x1b[0m is the closing tag *)

(* Box drawing characters are a thing: https://en.wikipedia.org/wiki/Box-drawing_characters *)
let render () =
  cubby_hole [
    "Hello, Cubby" ;
    "What in the world is this?" ;
    "I think I need more broccoli in my syrup" ;
  ]

let () = 
  clear_screen_ansi ();
  render ();
  print_endline "\x1b[37mTest"
