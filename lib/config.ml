open Player
open Common
open Printf

type config = {
  player_count : int;
  players : player list;
  default_hand_size : int;
}

let set_player_count (input : string) (config : config) =
  let player_count = safe_str_to_int input config.player_count "player count" in
  let updated_players =
    match player_count with
    | x when x > config.player_count ->
        config.players
        @ [
            {
              nickname = sprintf "Ai %d" (List.length config.players);
              p_type = Computer;
              hand = [];
            };
          ]
    | x when x < config.player_count ->
        List.filteri (fun i _ -> i < config.player_count - 1) config.players
    | _ -> config.players
  in
  { config with player_count; players = updated_players }

let set_selected_player_type (input : string) (config : config) =
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

let render_config config =
  printf "Currently there are %d players \n" config.player_count;
  render_players config.players
