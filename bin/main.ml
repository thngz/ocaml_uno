open Unogame.Menu
open Unogame.Common
open Unogame.Player
open Unogame.Utils
open Printf

type game_config = { player_count : int; players : player list }

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

let rec loop model game_config : unit =
  clear_screen ();
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
      | SelectPlayerCount pc -> handle_player_count pc config
      | TogglePlayerTypes _ -> handle_toggle_player_types config)

and handle_player_count player_count_opt config =
  match player_count_opt with
  | Some input ->
      if String.lowercase_ascii input = "b" then loop options_menu config
      else
        let player_count =
          safe_str_to_int input config.player_count "player count"
        in
        message_handler (Navigation Start) { config with player_count }
  | None -> loop player_count_prompt config

and handle_toggle_player_types config =
  clear_screen ();
  let players_as_menu_items =
    map_with_index
      (fun i x ->
        MenuItem
          {
            title = x.nickname;
            shortcut = string_of_int (i + 1);
            action = (fun () -> Navigation Start);
          })
      config.players
  in

  loop
    { title = "Currently added players"; items = players_as_menu_items }
    config

let () =
  loop main_menu
    {
      player_count = 2;
      players =
        [
          { nickname = "Player 1"; p_type = Human };
          { nickname = "Ai 1"; p_type = Computer };
        ];
    }
