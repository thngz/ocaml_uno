open Config
open Common
open Printf
open Card
open Player

type direction = Clockwise | CounterClockwise
type game_state = Starting | Running | Uno | End

type game = {
  current_player : player;
  current_player_index : int;
  game_config : config;
  deck : cards;
  cards_on_field : cards;
  direction : direction;
  game_state : game_state;
}

let distribute_cards (deck, players) : cards * player list =
  let rec aux deck players player_index hand_size =
    let player_opt = List.nth_opt players player_index in
    match player_opt with
    | None when hand_size > 0 -> aux deck players 0 hand_size
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
              (player_index + 1) (hand_size - 1))
  in

  aux deck players 0 7

let get_next_player (current_index : int) (players : player list) : player * int
    =
  match List.nth_opt players current_index with
  | None -> (List.nth players 0, 0)
  | Some p -> (p, current_index + 1)

let rec game_loop (game : game) =
  clear_screen ();

  printf "Current player %s" game.current_player.nickname;
  match game.game_state with
  | Starting -> handle_starting_state game
  | Running -> (
      let next_player, next_index =
        get_next_player game.current_player_index game.game_config.players
      in

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

and handle_starting_state game =
  let players = game.game_config.players in

  let updated_deck, updated_players = distribute_cards (game.deck, players) in
  game_loop
    {
      game with
      game_config = { game.game_config with players = updated_players };
      deck = updated_deck;
      game_state = Running;
    }

and start_game (config : config) =
  let game =
    {
      current_player = List.nth config.players 0;
      current_player_index = 0;
      game_config = config;
      deck = create_deck;
      cards_on_field = [];
      direction = Clockwise;
      game_state = Starting;
    }
  in
  game_loop game
