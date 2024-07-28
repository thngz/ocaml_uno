open Config
open Common
open Card
open Player
open Printf
open Colors

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
  notification : string;
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
                   if i = player_index then
                     {
                       p with
                       hand = x :: p.hand;
                       hand_length = p.hand_length + 1;
                     }
                   else p)
                 players)
              (player_index + 1) (deal_amount - 1))
  in
  aux deck players 0 (List.length players * hand_size)

let render_current_game_state (game : game) : game =
  print_endline "Current game field";
  print_endline "*******************************";
  (match game.cards_on_field with
  | [] -> print_endline "No cards have been played yet"
  | x :: _ ->
      printf "Currently at the top of the table:\n%s\n" (card_to_string x);
      print_endline "";
      print_endline "";
      printf "Log: \t\t\n";
      draw_cards game.cards_on_field);

  print_endline "*******************************";
  print_endline "";
  print_endline "";
  render_player_information game.current_player;
  game

let set_next_player (game : game) : game =
  let get_next_player current_index players : player * int =
    let next_index = current_index + 1 in
    match List.nth_opt players next_index with
    | None -> (List.nth players 0, 0)
    | Some p -> (p, next_index)
  in
  let next_player, next_index =
    get_next_player game.current_player_index game.players
  in
  { game with current_player = next_player; current_player_index = next_index }

let place_card (game : game) card index : game =
  let updated_hand =
    List.filteri (fun i _ -> i != index) game.current_player.hand
  in
  let updated_players =
    List.mapi
      (fun i p ->
        if i = game.current_player_index then { p with hand = updated_hand }
        else p)
      game.players
  in
  {
    game with
    cards_on_field = card :: game.cards_on_field;
    players = updated_players;
  }

let handle_player_move (game : game) : game =
  match read_line () with
  | "s" ->
      let notification =
        sprintf "%s skipped their turn!" game.current_player.nickname
      in
      { game with notification }
  | str -> (
      match int_of_string_opt str with
      | Some index when index < game.current_player.hand_length && index >= 0 ->
          let card = List.nth game.current_player.hand index in
          if List.length game.cards_on_field > 0 then
            let top_card = List.hd game.cards_on_field in
            if
              card.card_value = top_card.card_value
              || card.color = top_card.color
            then game
            else
              let notification =
                "Cannot play this card, ensure it matches the color or  number \
                 of the top card!"
              in
              { game with notification }
          else game
      | Some _ -> { game with notification = "Invalid index provided!" }
      | None ->
          { game with notification = "Invalid type of argument provided!" })

let rec game_loop (game : game) : unit =
  clear_screen ();
  print_endline (paint_red game.notification);
  match game.game_state with
  | Running -> game |> render_current_game_state |> handle_move
  | Uno -> game |> game_loop (*Does nothing yet..*)
  | End -> game |> game_loop (*Likewise*)

and handle_move (game : game) : unit =
  match game.current_player.p_type with
  | Human ->
      printf "Select a card to play (s to skip) >>> ";
      game |> handle_player_move |> set_next_player |> game_loop
  | Computer ->
      let _ = read_line () in
      game |> set_next_player |> game_loop

let start_game (config : config) : unit =
  let deck, players =
    distribute_cards
      (create_deck |> shuffle_cards)
      config.players config.default_hand_size
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
      notification = String.empty;
    }
  in
  game_loop game
