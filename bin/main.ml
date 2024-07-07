open Unogame.Menu
open Unogame.Common
open Printf

let main_menu = { title = "Main menu"; items = menu_items }
let options_menu = { title = "Options menu"; items = options_menu_items }

let player_count_prompt =
  { title = "Add player count"; items = get_player_count_prompt }

let clear_screen () = ignore (Sys.command "clear")

let get_selection (items : selection_item list) =
  let selectionText = read_line () in
  List.find_opt
    (fun x ->
      match x with
      | MenuItem n -> n.shortcut = selectionText
      | PromptItem _ -> true)
    items
  |> Option.map (function
       | MenuItem m -> MenuItem m
       | PromptItem p -> PromptItem { p with selection = Some selectionText })

let rec loop model : unit =
  List.iter draw_selection_item model.items;
  match get_selection model.items with
  | None ->
      clear_screen ();
      print_endline "Invalid choice!";
      loop model
  | Some selected -> (
      clear_screen ();
      let execute_action action title =
        printf "You chose: %s\n" title;
        message_handler (action ())
      in
      match selected with
      | MenuItem m -> execute_action m.action m.title
      | PromptItem p -> execute_action p.action p.title)

and message_handler (message : message) : unit =
  match message with
  | Navigation n -> (
      match n with
      | Start -> loop options_menu
      | PrevGames -> loop main_menu
      | Exit -> exit 0)
  | Prompt p -> (
      match p with
      | SelectPlayerCount -> loop player_count_prompt
      | TogglePlayerTypes -> loop main_menu)

let () = loop main_menu
