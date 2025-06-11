(*
  OCaml TUI Library - Inspired by Bubble Tea
*)

(* This is an interface of a TUI app. All apps that use this library must at least provide these details *)
module type App = sig
  (* This value is essentially the shape of the data that will be tracked as the app is running *)
  (** The type of the application's state. *)
  type model

  (* This is an action within the app. Everything takes place using "messages" *)
  (** The type of messages that can update the model. *)
  type msg

  (* This is a command that the user can execute, which could produce one or more messages which could update the model *)
  (** Represents a command that can be executed, potentially producing a new message. *)
  module Cmd : sig
    type 'a t = unit -> 'a option
    val none : 'a t
    val batch : 'a t list -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  (** The initial state of the application and any initial commands to run. *)
  val init : unit -> model * msg Cmd.t

  (** Handles incoming messages and updates the model.
      Returns the new model and any commands to execute. *)
  val update : model -> msg -> model * msg Cmd.t

  (** Renders the current model as a Notty image. *)
  val view : model -> Notty.image

  (** Maps a raw terminal event to an application-specific message.
      The type is the full polymorphic variant returned by Notty_unix.Term.event. *)
  val map_term_event :
    ([ `End
     | `Key of Notty.Unescape.key
     | `Mouse of Notty.Unescape.mouse
     | `Paste of Notty.Unescape.paste
     | `Resize of int * int
     ]) -> msg option

  (** Determines if the application should quit based on the current model. *)
  val should_quit : model -> bool
end

module TuiProgram (App : App) = struct
  open Notty_unix

  module Cmd = App.Cmd

  let run (initial_model : App.model) (initial_cmd : App.msg Cmd.t) =
    let term = Term.create () in

    let rec loop model current_cmd =
      (* 1. Render the current view *)
      Term.image term (App.view model);

      (* 2. Check if the application should quit based on the current model *)
      if App.should_quit model then
        Term.release term (* Exit if model says so *)
      else begin
        (* 3. Execute the current command, if any, to get a potential message *)
        let msg_from_cmd : App.msg option = current_cmd () in

        (* 4. Determine the next application message:
              - If there's a message from a command, use that.
              - Otherwise, wait for a terminal event and map it using App.map_term_event.
        *)
        let next_app_msg_opt : App.msg option =
          match msg_from_cmd with
          | Some cmd_msg -> Some cmd_msg (* Message from command takes precedence *)
          | None ->
              (* No message from command, wait for terminal event *)
              App.map_term_event (Notty_unix.Term.event term) (* App maps terminal event *)
        in

        (* 5. Process the obtained message (if any) *)
        match next_app_msg_opt with
        | None ->
            (* No message from command and no message from terminal event (e.g., unhandled or `End` mapped to None by app).
               This means we continue the loop, effectively waiting for the next command/event.
               If Term.event is blocking and App.map_term_event returns None for a specific event,
               it means the app chose to ignore it and we simply carry on.
            *)
            loop model Cmd.none (* Continue with no new command *)
        | Some app_msg ->
            (* Process the application message *)
            let updated_model, new_cmd_from_update = App.update model app_msg in

            (* 6. Check quit condition again *after* the update *)
            if App.should_quit updated_model then
              begin
                Term.image term (App.view updated_model); (* Render final "quitting" state *)
                Term.release term
              end
            else
              loop updated_model new_cmd_from_update (* Continue with the new model and command *)
      end
    in

    (* Initial setup before starting the loop *)
    if App.should_quit initial_model then
      Term.release term (* App might want to quit immediately from init state *)
    else
      try
        loop initial_model initial_cmd
      with
      | e ->
          (* Ensure terminal is released on any exception during the loop *)
          let _ = Term.release term in (* Ignore result, just ensure it's called *)
          raise e
          
  let start () =
    let initial_model, initial_cmd = App.init () in
    run initial_model initial_cmd
end

(* Default Command implementation (can be part of tui_lib.ml or a separate utils file) *)
module DefaultCmd = struct
  type 'a t = unit -> 'a option
  let none : 'a t = fun () -> None
  let batch (cmds: 'a t list) : 'a t = fun () ->
    List.fold_left
      (fun acc cmd -> match acc with Some _ -> acc | None -> cmd ())
      None
      cmds
  let map (f : 'a -> 'b) (cmd : 'a t) : 'b t =
    fun () -> match cmd () with
              | Some x -> Some (f x)
              | None -> None
end


(* This would be in a separate file, e.g., example.ml or main.ml *)
module CounterApp : App = struct
  module Cmd = DefaultCmd (* Use the DefaultCmd implementation *)

  type model = {
    count: int;
    quitting: bool; (* Flag to indicate if the app should quit *)
    last_key_debug: string; (* For displaying last mapped key *)
  }

  type msg =
    | Increment
    | Decrement
    | KeyPress of char (* For generic character key presses *)
    | EnterPressed
    | ArrowLeft
    | ArrowRight
    | WindowResized
    | Quit (* A dedicated message to signal quit intent *)
    | NoOp (* For events that are acknowledged but require no state change *)

  let init () : model * msg Cmd.t =
    ({ count = 0; quitting = false; last_key_debug = "None" }, Cmd.none)

  let update (model : model) (msg : msg) : model * msg Cmd.t =
    if model.quitting && msg <> Quit then (* If already quitting, mostly ignore other messages *)
      (model, Cmd.none)
    else
      match msg with
      | Increment -> ({ model with count = model.count + 1; last_key_debug = "Increment" }, Cmd.none)
      | Decrement -> ({ model with count = model.count - 1; last_key_debug = "Decrement" }, Cmd.none)
      | KeyPress c -> ({ model with last_key_debug = Printf.sprintf "KeyPress: %c" c }, Cmd.none)
      | EnterPressed -> ({ model with last_key_debug = "EnterPressed" }, Cmd.none)
      | ArrowLeft -> ({ model with last_key_debug = "ArrowLeft" }, Cmd.none)
      | ArrowRight -> ({ model with last_key_debug = "ArrowRight" }, Cmd.none)
      | WindowResized -> ({ model with last_key_debug = "WindowResized" }, Cmd.none)
      | Quit -> ({ model with quitting = true; last_key_debug = "Quit" }, Cmd.none) (* Set the quitting flag *)
      | NoOp -> ({model with last_key_debug = "NoOp"}, Cmd.none) (* No state change *)

  let view (model : model) : Notty.image =
    let open Notty in
    let open Notty.Infix in (* for <-> operator *)

    if model.quitting then
      I.string A.(fg red ++ st bold) "Quitting application..."
    else
      let count_str = I.string A.(fg lightblue ++ st bold) (Printf.sprintf "Count: %d" model.count) in
      let last_key_str = I.string A.(fg green) (Printf.sprintf "Last Event Mapped: %s" model.last_key_debug) in
      let instructions =
        I.string A.empty "Arrows or k/j to increment/decrement count. 'q' to quit."
      in
      count_str <-> last_key_str <-> instructions

  let map_term_event (event : (* Explicitly use the wider polymorphic variant type *)
    [ `End
    | `Key of Notty.Unescape.key
    | `Mouse of Notty.Unescape.mouse
    | `Paste of Notty.Unescape.paste
    | `Resize of int * int
    ]) : msg option =
    match event with
    | `Key (`ASCII 'q', _) -> Some Quit
    | `Key (`ASCII 'k', _) | `Key (`Arrow `Up, _) -> Some Increment
    | `Key (`ASCII 'j', _) | `Key (`Arrow `Down, _) -> Some Decrement
    | `Key (`Enter, _) -> Some EnterPressed
    | `Key (`Arrow `Left, _) -> Some ArrowLeft
    | `Key (`Arrow `Right, _) -> Some ArrowRight
    | `Key (`ASCII c, _mods) -> Some (KeyPress c) (* Other ASCII keys *)
    | `Key (_other_key, _mods) -> Some NoOp (* Catch-all for other unhandled key events (e.g., function keys) *)
    | `Resize (_w, _h) -> Some WindowResized (* Use _w, _h if you need dimensions *)
    | `Mouse _ -> Some NoOp
    | `Paste _ -> Some NoOp
    | `End -> Some Quit (* Treat EOF or interrupt as a quit signal *)
    (* Note: `Unsupported of string` is not listed in the error message's type for Term.event,
       so it's not included as a case here. If it can occur, the type definition would need
       to be wider and this match would need to handle it. *)


  let should_quit (model : model) : bool =
    model.quitting
end

(* Create the program from our App module *)
module Program = TuiProgram(CounterApp)

(* Entry point: usually in your main executable file *)
(* To run, uncomment this line IF this whole block is compiled as a single .ml file: *)
(* let () = Program.start () *)

let () = Program.start ()
