open Common

type menu_item = { title : string; shortcut : string; action : unit -> message }
type prompt_item = { title : string; action : unit -> message }
type selection_item = MenuItem of menu_item | PromptItem of prompt_item
type menu = { items : selection_item list; title : string }

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
    PromptItem
      {
        title = "Select player amount";
        action = (fun () -> Prompt SelectPlayerCount);
      };
    PromptItem
      {
        title = "Toggle player types";
        action = (fun () -> Prompt TogglePlayerTypes);
      };
    MenuItem
      { title = "Exit"; shortcut = "e"; action = (fun () -> Navigation Exit) };
  ]

let get_player_count_prompt =
  [
    PromptItem {
      title = "Get player count";
      action = (fun () -> Prompt SelectPlayerCount);
    };
  ]

let draw_selection_item (item : selection_item) =
  match item with
  | MenuItem mi -> Printf.printf "%s) %s\n" mi.shortcut mi.title
  | PromptItem pi -> Printf.printf ">>> %s\n" pi.title
