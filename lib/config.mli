open Player

type config = {
  player_count : int;
  players : player list;
  default_hand_size : int;
}

val set_player_count : string -> config -> config
val set_selected_player_type : string -> config -> config
val render_config : config -> unit
