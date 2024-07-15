type player_type = Human | Computer
type player = { nickname : string; p_type : player_type }

val toggle_type : player_type -> player_type
val draw_players : player list -> unit
