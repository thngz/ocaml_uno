open Player
open Common

type config = { player_count : int; players : player list }

let set_player_count (input : string) (config : config) =
  let player_count = safe_str_to_int input config.player_count "player count" in
  { config with player_count }

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
