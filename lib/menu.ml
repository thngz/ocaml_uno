open Common

type menu_item = { title : string; shortcut : string; action : unit -> message }

type prompt_item = {
  title : string;
  action : string -> message;
  selection : string option;
}

type ui_item = MenuItem of menu_item | PromptItem of prompt_item
type menu = { items : ui_item list; title : string }

let render_menu (menu : menu) =
  let render_menu_item (item : ui_item) =
    match item with
    | MenuItem mi -> Printf.printf "%s) %s\n" mi.shortcut mi.title
    | PromptItem pi -> Printf.printf "%s >>> " pi.title
  in
  List.iter render_menu_item menu.items

let main_menu =
  let main_menu_items =
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
  in
  { title = "Main menu"; items = main_menu_items }

let options_menu =
  let options_menu_items =
    [
      MenuItem
        {
          title = "Start game!";
          shortcut = "1";
          action = (fun () -> Navigate StartGame);
        };
      MenuItem
        {
          title = "Select player amount";
          shortcut = "2";
          action = (fun () -> Prompt (SelectPlayerCount String.empty));
        };
      MenuItem
        {
          title = "Toggle player types";
          shortcut = "3";
          action = (fun () -> Prompt (TogglePlayerTypes String.empty));
        };
      MenuItem
        { title = "Exit"; shortcut = "e"; action = (fun () -> Navigate Exit) };
    ]
  in
  { title = "Options menu"; items = options_menu_items }

let player_count_prompt =
  let player_count_prompt_items =
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
  in
  { title = "Insert player count"; items = player_count_prompt_items }

let playertype_prompt =
  let playertype_toggle_items =
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
  in
  { title = "Toggle player types"; items = playertype_toggle_items }

