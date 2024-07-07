open Common

type menu_item = { title : string; shortcut : string; action : unit -> message }

type prompt_item = {
  title : string;
  action : string option -> message;
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
      { title = "Start"; shortcut = "1"; action = (fun () -> Navigation Start) };
    MenuItem
      {
        title = "View previous games";
        shortcut = "3";
        action = (fun () -> Navigation PrevGames);
      };
    MenuItem
      { title = "Exit"; shortcut = "e"; action = (fun () -> Navigation Exit) };
  ]

let options_menu_items =
  [
    MenuItem
      {
        title = "Select player amount";
        shortcut = "1";
        action = (fun () -> Prompt (SelectPlayerCount None));
      };
    MenuItem
      {
        title = "Toggle player types";
        shortcut = "2";
        action = (fun () -> Prompt (TogglePlayerTypes None));
      };
    MenuItem
      { title = "Exit"; shortcut = "e"; action = (fun () -> Navigation Exit) };
  ]

let get_player_count_prompt =
  [
    PromptItem
      {
        title = "Set player count";
        action = (fun arg -> Prompt (SelectPlayerCount arg));
        selection = None;
      };
  ]
