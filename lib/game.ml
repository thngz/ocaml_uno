open Config
open Common
open Printf
open Card
open Player

let start_game config =
  clear_screen ();
  printf "Starting game with %d players" config.player_count;
    
  let deal_cards players =
    List.map (fun p -> 
        {p with cards = Some List.iter}) players
