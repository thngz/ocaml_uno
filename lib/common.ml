type navigation_message = Start | PrevGames | Exit 
type prompt_message = SelectPlayerCount | TogglePlayerTypes 

type message = Navigation of navigation_message | Prompt of prompt_message
