open Config
open Common
open Card
open Player
(* open Printf *)

type direction = Clockwise | CounterClockwise
type game_state = Running | Uno | End

type game = {
  current_player : player;
  current_player_index : int;
  game_config : config;
  players : player list;
  deck : cards;
  cards_on_field : cards;
  direction : direction;
  game_state : game_state;
}

let distribute_cards deck players hand_size : cards * player list =
  let rec aux deck players player_index deal_amount =
    let player_opt = List.nth_opt players player_index in
    match player_opt with
    | None when deal_amount > 0 -> aux deck players 0 deal_amount
    | None -> (deck, players)
    | Some _ -> (
        match deck with
        | [] ->
            print_endline "Deck is empty";
            (deck, players)
        | x :: xs ->
            aux xs
              (List.mapi
                 (fun i p ->
                   if i = player_index then { p with hand = x :: p.hand } else p)
                 players)
              (player_index + 1) (deal_amount - 1))
  in
  aux deck players 0 (List.length players * hand_size)

let get_next_player current_index players : player * int =
  let next_index = current_index + 1 in
  match List.nth_opt players next_index with
  | None -> (List.nth players 0, 0)
  | Some p -> (p, next_index)

let render_current_game_state game =
  print_endline "*******************************";
  print_endline "Current game field";
  if List.length game.cards_on_field = 0 then
    print_endline "No cards have been played yet"
  else draw_cards game.cards_on_field;
  print_endline "*******************************";
  print_endline "";
  print_endline "";
  render_player_information game.current_player

let rec game_loop (game : game) =
  clear_screen ();
  match game.game_state with
  | Running -> (
      let next_player, next_index =
        get_next_player game.current_player_index game.players
      in
      render_current_game_state game;
      match game.current_player.p_type with
      | Human ->
          let _ = read_line () in
          game_loop
            {
              game with
              current_player = next_player;
              current_player_index = next_index;
            }
      | Computer ->
          let _ = read_line () in
          game_loop
            {
              game with
              current_player = next_player;
              current_player_index = next_index;
            })
  | Uno -> ()
  | End -> ()

and start_game (config : config) =
  let deck, players =
    distribute_cards create_deck config.players config.default_hand_size
  in
  let game =
    {
      current_player = List.nth players 0;
      current_player_index = 0;
      game_config = config;
      players;
      deck;
      cards_on_field = [];
      direction = Clockwise;
      game_state = Running;
    }
  in
  game_loop game
