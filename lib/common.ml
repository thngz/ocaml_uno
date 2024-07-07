open Printf

type navigation_message = Start | PrevGames | Exit 
type prompt_message = SelectPlayerCount of string option | TogglePlayerTypes of string option

type message = Navigation of navigation_message | Prompt of prompt_message


let safe_str_to_int convertable fallback convertable_name =
  match int_of_string_opt convertable with
  | Some p -> p
  | None ->
      printf "Please enter correct %s\n" convertable_name;
      fallback
