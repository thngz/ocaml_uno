open Printf
open Card

type player_type = Human | Computer
type player = { nickname : string; p_type : player_type; hand: cards}

let render_player player =
  match player.p_type with
  | Human -> printf "Human player with name %s \n" player.nickname
  | Computer -> printf "Computer player with name %s \n" player.nickname

let toggle_type p_type =
  match p_type with Human -> Computer | Computer -> Human

let render_players players =
  List.iteri (fun i p ->
      printf "%d) " i;
      render_player p) players

let render_player_information player =
    print_endline "===============================";
    render_player player;
    
    print_endline "===============================";
    printf "Current hand has %d cards\n" (List.length player.hand);
    draw_cards player.hand;
    print_endline "===============================";

