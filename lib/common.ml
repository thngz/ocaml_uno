open Player

type navigation_message = Start | PrevGames | Exit

type prompt_message =
  | SelectPlayerCount of string 
  | TogglePlayerTypes of string

type game_config = { player_count : int; players : player list }

type message =
  | Navigate of navigation_message
  | Prompt of prompt_message
