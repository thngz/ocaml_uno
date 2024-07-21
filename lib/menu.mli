open Common

type menu_item = { title : string; shortcut : string; action : unit -> message }

type prompt_item = {
  title : string;
  action : string -> message;
  selection : string option;
}

type ui_item = MenuItem of menu_item | PromptItem of prompt_item
type menu = { items : ui_item list; title : string }

val render_menu : menu -> unit

val main_menu : menu
val options_menu : menu
val player_count_prompt : menu
val playertype_prompt: menu
