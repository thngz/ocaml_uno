type navigation_message = Start | PrevGames | StartGame | Exit

type prompt_message =
  | SelectPlayerCount of string
  | TogglePlayerTypes of string

type message = Navigate of navigation_message | Prompt of prompt_message


let clear_screen () = ignore (Sys.command "clear")
