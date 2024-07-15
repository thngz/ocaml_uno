open Unogame.Menu
open Unogame.Common
open Unogame.Config
open Unogame.Player
open Printf

type loop_params = { menu : menu; config : config; should_draw_config : bool }

let clear_screen () = ignore (Sys.command "clear")

let get_selection (items : ui_item list) =
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

let rec loop (params : loop_params) : unit =
  clear_screen ();
  if params.should_draw_config then draw_config params.config else ();
  draw_menu params.menu;
  match get_selection params.menu.items with
  | None ->
      print_endline "Invalid choice!";
      loop params
  | Some selected -> (
      match selected with
      | MenuItem m ->
          printf "You chose: %s\n" m.title;
          message_handler (m.action ()) params
      | PromptItem p -> (
          match p.selection with
          | Some s -> message_handler (p.action s) params
          | None -> printf "Prompt returned nothing"))

and message_handler (message : message) (params : loop_params) =
  match message with
  | Navigate n -> (
      match n with
        | Start -> loop { params with menu = options_menu; should_draw_config = false }
        | PrevGames -> loop { params with menu = options_menu; should_draw_config = false }
      | Exit -> exit 0)
  | Prompt p -> (
      match p with
      | SelectPlayerCount selection ->
          printf "Selection is: %s\n" selection;
          if selection = String.empty then
            loop
              {
                params with
                menu = player_count_prompt;
                should_draw_config = true;
              }
          else
            loop
              {
                params with
                menu = player_count_prompt;
                config = set_player_count selection params.config;
              }
      | TogglePlayerTypes selection ->
          printf "Selection is: %s\n" selection;
          if selection = String.empty then
            loop
              {
                params with
                menu = playertype_prompt;
                should_draw_config = true;
              }
          else
            loop
              {
                params with
                menu = playertype_prompt;
                config = set_selected_player_type selection params.config;
              })

let () =
  let default_config =
    {
      player_count = 2;
      players =
        [
          { nickname = "Player 1"; p_type = Human };
          { nickname = "Ai 1"; p_type = Computer };
        ];
    }
  in
  loop { menu = main_menu; config = default_config; should_draw_config = false }
