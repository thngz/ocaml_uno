open Card

type player_type = Human | Computer
type player = { nickname : string; p_type : player_type; hand : cards; hand_length: int}

val toggle_type : player_type -> player_type
val render_players : player list -> unit
val render_player_information: player -> unit
