open Unogame.Menu
open Unogame.Common
open Printf


let main_menu = create_menu menu_items
let options_menu = create_menu options_menu_items

let rec game_loop (model: model): unit =
    model.view ();
    
    let selectionText = read_line () in
    let filtered = List.filter (fun x -> x.shortcut = selectionText) model.items in 
    match filtered with
            | [] -> ignore (Sys.command "clear"); print_endline "Invalid choice!"; game_loop model;
            | selected :: _ -> 
                    ignore (Sys.command "clear");
                    printf "You chose: ";
                    List.iter draw_item filtered;
                    update (selected.action ())
and update (message: message): unit =
    match message with
        | Start -> game_loop main_menu
        | Options -> game_loop options_menu
        | ViewGames -> game_loop main_menu
        | Exit -> exit 0


let () = game_loop main_menu
