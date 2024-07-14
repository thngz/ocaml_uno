open Unogame.Menu
open Unogame.Common
open Unogame.Player
open Unogame.Utils
open Printf

let main_menu = { title = "Main menu"; items = menu_items }
let options_menu = { title = "Options menu"; items = options_menu_items }
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

let set_player_count input config =
  let player_count = safe_str_to_int input config.player_count "player count" in
  { config with player_count }

let set_selected_player_type input config =
  let player_index = safe_str_to_int input 0 "player index" in
  let updated_players =
    List.mapi
      (fun i player ->
        if i = player_index then
          { player with p_type = toggle_type player.p_type }
        else player)
      config.players
  in

  { config with players = updated_players }

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
          | Some s -> message_handler (p.action s) game_config
          | None -> printf "Prompt returned nothing"))

and message_handler message config =
  match message with
  | Navigate n -> (
      match n with
      | Start -> loop options_menu config
      | PrevGames -> loop main_menu config
      | Exit -> exit 0)
  | Prompt p -> (
      match p with
      | SelectPlayerCount selection ->
          printf "Selection is: %s\n" selection;
          if selection = String.empty then
            loop
              { title = "Get player count"; items = get_player_count_prompt }
              config
          else
            loop
              { title = "Get player count"; items = get_player_count_prompt }
              (set_player_count selection config)
      | TogglePlayerTypes selection ->
          printf "Selection is: %s\n" selection;
          List.iteri
            (fun i p ->
              printf "%d) " i;
              draw_player p)
            config.players;
          if selection = String.empty then
            loop
              { title = "Toggle player types"; items = toggle_player_at_prompt }
              config
          else
            loop
              { title = "Toggle player types"; items = toggle_player_at_prompt }
              (set_selected_player_type selection config))

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
