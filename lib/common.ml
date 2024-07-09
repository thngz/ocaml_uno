
type navigation_message = Start | PrevGames | Exit 
type prompt_message = SelectPlayerCount of string option | TogglePlayerTypes of string option

type message = Navigation of navigation_message | Prompt of prompt_message


