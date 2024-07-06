open Unogame.Menu
open Unogame.Common
open Printf

let main_menu = { title = "Main menu"; items = menu_items }

let options_menu =
  { title = "Options menu"; items = options_menu_items }

let player_count_prompt =
  { title = "Add player count"; items = get_player_count_prompt }

let clear_screen () = ignore (Sys.command "clear")

type selected_item = SelectedItems of menu_item list | Selection of string | Nil

let get_selection (items: selection_item list): selected_item =
  let selectionText = read_line () in
    match items with 
    | [] -> Nil
    | MenuItem _ :: _ -> SelectedItems (List.filter (fun x -> x.shortcut = selectionText) items
    | PromptItem _ :: _ -> Selection selectionText

let rec loop model : unit =
  List.iter draw_selection_item model.items;
  match get_selection model.items with
  | [] ->
      clear_screen ();
      print_endline "Invalid choice!";
      loop model
  | selected :: _ ->
      clear_screen ();
      printf "You chose: %s\n" selected.title;
      message_handler (selected.action ())

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
