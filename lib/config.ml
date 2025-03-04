open Player
open Printf

type config = {
  player_count : int;
  players : player list;
  default_hand_size : int;
}

let set_player_count (input : string) (config : config) =
  match int_of_string_opt input with
  | Some player_count ->
      let updated_players =
        match player_count with
        | x when x > config.player_count ->
            config.players
            @ [
                {
                  nickname = sprintf "Ai %d" (List.length config.players);
                  p_type = Computer;
                  hand = [];
                  hand_length = 0;
                };
              ]
        | x when x < config.player_count ->
            List.filteri (fun i _ -> i < config.player_count - 1) config.players
        | _ -> config.players
      in
      { config with player_count; players = updated_players }
  | None ->
      print_endline "Invalid player count!";
      config

let set_selected_player_type (input : string) (config : config) =
  match int_of_string_opt input with
  | Some index ->
      let updated_players =
        List.mapi
          (fun i player ->
            if i = index then { player with p_type = toggle_type player.p_type }
            else player)
          config.players
      in
      { config with players = updated_players }
  | None ->
      print_endline "Invalid input provided";
      config

let render_config config =
  printf "Currently there are %d players \n" config.player_count;
  render_players config.players
