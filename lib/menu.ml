open Common

type menu_item = { title : string; shortcut : string; action : unit -> message }

type prompt_item = {
  title : string;
  action : string -> message;
  selection : string option;
}

type selection_item = MenuItem of menu_item | PromptItem of prompt_item
type menu = { items : selection_item list; title : string }

let draw_selection_item (item : selection_item) =
  match item with
  | MenuItem mi -> Printf.printf "%s) %s\n" mi.shortcut mi.title
  | PromptItem pi -> Printf.printf "%s >>> " pi.title

let menu_items =
  [
    MenuItem
      { title = "Start"; shortcut = "1"; action = (fun () -> Navigate Start) };
    MenuItem
      {
        title = "View previous games";
        shortcut = "2";
        action = (fun () -> Navigate PrevGames);
      };
    MenuItem
      { title = "Exit"; shortcut = "e"; action = (fun () -> Navigate Exit) };
  ]

let options_menu_items =
  [
    MenuItem
      {
        title = "Select player amount";
        shortcut = "1";
        action = (fun () -> Prompt (SelectPlayerCount String.empty));
      };
    MenuItem
      {
        title = "Toggle player types";
        shortcut = "2";
        action = (fun () -> Prompt (TogglePlayerTypes String.empty));
      };
    MenuItem
      { title = "Exit"; shortcut = "e"; action = (fun () -> Navigate Exit) };
  ]

let get_player_count_prompt =
  [
    MenuItem
      { title = "Back"; shortcut = "b"; action = (fun () -> Navigate Start) };
    PromptItem
      {
        title = "Set player count";
        action = (fun arg -> Prompt (SelectPlayerCount arg));
        selection = None;
      };
  ]

let toggle_player_at_prompt =
  [
    MenuItem
      { title = "Back"; shortcut = "b"; action = (fun () -> Navigate Start) };
    PromptItem
      {
        title = "Toggle player at: ";
        action = (fun arg -> Prompt (TogglePlayerTypes arg));
        selection = None;
      };
  ]
