let clear_screen_ansi () : unit =
  print_string "\x1b[2J\x1b[H";
  flush stdout

let draw_centered_box (content: string list) : unit =
  clear_screen_ansi ();

  (* Define the character for horizontal padding inside the box, around the content. *)
  let horizontal_padding_char = ' ' in
  (* Define the number of padding characters on each side (left/right) of the content block. *)
  let num_horizontal_padding_chars = 3 in (* e.g., "= content =" uses 1 space *)

  (* Handle empty content: draw a small empty box. *)
  if content = [] then (
    let min_box_width = 2 + (2 * num_horizontal_padding_chars) + 2 in (* == for content, plus padding, plus borders *)
    let empty_content_line = String.make (min_box_width - 2) horizontal_padding_char in
    print_endline (String.make min_box_width '=');
    print_endline ("=" ^ empty_content_line ^ "=");
    print_endline (String.make min_box_width '=');
    flush stdout;
    (* exit 0; (* Uncomment if you want to exit after drawing an empty box in a script *) *)
    () (* Return unit *)
  ) else (
    (* Calculate the maximum width of any line in the content. This determines the width of the text block. *)
    let content_max_width = List.fold_left (fun acc next_line -> max acc (String.length next_line)) 0 content in

    (* The width of the area where centered text lines will be placed. *)
    let text_block_width = content_max_width in

    (* The width of the inside of the box, including the text block and surrounding horizontal padding. *)
    let inner_box_width = text_block_width + (2 * num_horizontal_padding_chars) in

    (* The total width of the box, including the border characters ('='). *)
    let box_total_width = inner_box_width + 2 (* for '=' on left and right *) in

    (* Print the top border. *)
    print_endline (String.make box_total_width '=');

    (* Print each line of content, centered and padded. *)
    List.iter (fun line ->
      let line_len = String.length line in

      (* Calculate padding needed to center the current line within the text_block_width. *)
      let centering_padding_total = text_block_width - line_len in
      let centering_padding_left = centering_padding_total / 2 in
      let centering_padding_right = centering_padding_total - centering_padding_left in (* Handles odd differences *)

      (* Construct the centered text segment for the current line. *)
      let centered_line_segment =
        (String.make centering_padding_left ' ') ^ line ^ (String.make centering_padding_right ' ')
      in
      (* centered_line_segment now has length text_block_width *)

      (* Construct the full line to be printed: border, padding, centered text, padding, border. *)
      let output_line =
        Printf.sprintf "=%s%s%s="
          (String.make num_horizontal_padding_chars horizontal_padding_char)
          centered_line_segment
          (String.make num_horizontal_padding_chars horizontal_padding_char)
      in
      print_endline output_line
    ) content;

    (* Print the bottom border. *)
    print_endline (String.make box_total_width '=');

    flush stdout
  )

let rec game_loop action =
        draw_centered_box [
          "My todo list" ;
          "" ;
          "[ ] Build a Castle" ;
          "[ ] Dig a moat" ;
          "[ ] Build a bridge" ;
          "[ ] Make friends along the way" ;
        ];
        print_endline ("(a) add an item");
        print_endline ("(c) complete an item");
        print_endline ("(r) remove an item");
        if action = "end" then 
                print_endline "goodbye!"
        else begin
                Printf.printf "What would you like to do? ";
                game_loop (read_line())
        end

let () = 
        try
                game_loop ""
        with
        | End_of_file -> print_endline "\n Goodbye!"


(*let () = Coffee_lib.Program.start ()*)
