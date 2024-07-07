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

let handle_player_count pc loop config =
  match pc with
  | Some p ->
      if String.lowercase_ascii p = "b" then loop options_menu config
      else
        loop player_count_prompt
          {
            player_count =
              (match int_of_string_opt p with
              | Some s -> s
              | None ->
                  printf "Please enter correct player count\n";
                  2);
          }
  | None -> loop player_count_prompt config

let rec loop model game_config : unit =
  List.iter draw_selection_item model.items;
  printf "conf is %d\n" game_config.player_count;

  match get_selection model.items with
  | None ->
      clear_screen ();
      print_endline "Invalid choice!";
      loop model game_config
  | Some selected -> (
      clear_screen ();
      match selected with
      | MenuItem m ->
          printf "You chose: %s\n" m.title;
          message_handler (m.action ()) game_config
      | PromptItem p -> (
          match p.selection with
          | Some s -> message_handler (p.action (Some s)) game_config
          | None -> printf "Prompt returned nothing"))

and message_handler (message : message) (config : game_config) =
  match message with
  | Navigation n -> (
      match n with
      | Start -> loop options_menu config
      | PrevGames -> loop main_menu config
      | Exit -> exit 0)
  | Prompt p -> (
      match p with
      | SelectPlayerCount pc -> handle_player_count pc loop config
      | TogglePlayerTypes _ -> loop main_menu config)

let () = loop main_menu { player_count = 2 }
