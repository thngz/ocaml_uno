open Printf

type player_type = Human | Computer
type player = { nickname : string; p_type : player_type }

let draw_player player =
  match player.p_type with
  | Human -> printf "Human player with name %s \n" player.nickname
  | Computer -> printf "Computer player with name %s \n" player.nickname

let toggle_type p_type =
  match p_type with Human -> Computer | Computer -> Human

let draw_players players =
  List.iteri (fun i p ->
      printf "%d) " i;
      draw_player p) players
