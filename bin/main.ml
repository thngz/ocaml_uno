open Unogame.Menu
open Unogame.Common
open Printf

let main_menu = { title = "Main menu"; items = menu_items }
let options_menu = { title = "Options menu"; items = options_menu_items }

let player_count_prompt =
  { title = "Add player count"; items = get_player_count_prompt }

let clear_screen () = ignore (Sys.command "clear")

type game_config = { player_count : int }

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

let rec loop model game_config : unit =
  clear_screen ();
  printf "Current player count is: %d\n" game_config.player_count;
  List.iter draw_selection_item model.items;

  match get_selection model.items with
  | None ->
      print_endline "Invalid choice!";
      loop model game_config
  | Some selected -> (
      match selected with
      | MenuItem m ->
          printf "You chose: %s\n" m.title;
          message_handler (m.action ()) game_config
      | PromptItem p -> (
          match p.selection with
          | Some s -> message_handler (p.action (Some s)) game_config
          | None -> printf "Prompt returned nothing"))

and message_handler message config =
  match message with
  | Navigation n -> (
      match n with
      | Start -> loop options_menu config
      | PrevGames -> loop main_menu config
      | Exit -> exit 0)
  | Prompt p -> (
      match p with
      | SelectPlayerCount pc -> handle_player_count pc config loop
      | TogglePlayerTypes _ -> loop main_menu config)

and handle_player_count player_count_opt config loop =
  match player_count_opt with
  | Some input ->
      if String.lowercase_ascii input = "b" then loop options_menu config
      else
        let player_count =
          safe_str_to_int input config.player_count "player count"
        in
        loop player_count_prompt { player_count }
  | None -> loop player_count_prompt config

let () = loop main_menu { player_count = 2 }
