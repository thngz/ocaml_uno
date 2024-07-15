open Unogame.Menu
open Unogame.Common
open Unogame.Config
open Unogame.Player
open Printf

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

let rec loop (model : menu) (config : config) : unit =
  clear_screen ();
  draw_menu model;

  match get_selection model.items with
  | None ->
      print_endline "Invalid choice!";
      loop model config
  | Some selected -> (
      match selected with
      | MenuItem m ->
          printf "You chose: %s\n" m.title;
          message_handler (m.action ()) config
      | PromptItem p -> (
          match p.selection with
          | Some s -> message_handler (p.action s) config
          | None -> printf "Prompt returned nothing"))

and message_handler (message : message) (config : config) =
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
          if selection = String.empty then loop player_count_prompt config
          else loop player_count_prompt (set_player_count selection config)
      | TogglePlayerTypes selection ->
          printf "Selection is: %s\n" selection;
          if selection = String.empty then loop playertype_prompt config
          else
            loop playertype_prompt (set_selected_player_type selection config))

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
  loop main_menu default_config
