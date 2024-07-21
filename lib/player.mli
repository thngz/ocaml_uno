open Card

type player_type = Human | Computer
type player = { nickname : string; p_type : player_type; hand : cards}

val toggle_type : player_type -> player_type
val render_players : player list -> unit
