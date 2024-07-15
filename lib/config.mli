open Player

type config = { player_count : int; players : player list }

val set_player_count : string -> config -> config
val set_selected_player_type : string -> config -> config

val draw_config : config -> unit

