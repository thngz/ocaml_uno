open Config
open Common
open Printf
open Card
open Player

let start_game config =
  clear_screen ();
  printf "Starting game with %d players\n" config.player_count;

  let players_with_cards =
    let deck = create_deck in
    let rec deal_cards cards (player : player) n =
      if n = 0 then player
      else
        match cards with
        | [] -> player
        | x :: xs -> deal_cards xs { player with hand = x :: player.hand } (n - 1)
    in
    List.map (fun player -> deal_cards deck player 7) config.players
  in

  List.iter
    (fun player ->
      printf "Player %s hand:" player.nickname;
      draw_cards player.hand)
    players_with_cards
