open Printf

type navigation_message = Start | PrevGames | StartGame | Exit

type prompt_message =
  | SelectPlayerCount of string
  | TogglePlayerTypes of string

type message = Navigate of navigation_message | Prompt of prompt_message 

let safe_str_to_int (convertable : string) (fallback : int)
    (convertable_name : string) =
  match int_of_string_opt convertable with
  | Some p -> p
  | None ->
      printf "Please enter correct %s\n" convertable_name;
      fallback

let clear_screen () = ignore (Sys.command "clear")
