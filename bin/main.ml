open Unogame.Menu
open Printf
let menu_items: menu_item list = [ 
    {title = "Start"; shortcut= "1"};
    {title = "Options"; shortcut= "2"};
    {title = "View previous games"; shortcut= "3"};
    {title = "Exit"; shortcut= "e"};
];;

let rec game_loop () =
    draw menu_items;
    let selectionText = read_line () in
    let filtered = List.filter (fun x -> x.shortcut = selectionText) menu_items
    in 
    match selectionText with
        | "e" -> print_endline "Exiting"; exit 0
        | _ -> match filtered with
            | [] -> print_endline "Invalid choice!"
            | _ ->  printf "You chose: ";
                    List.iter draw_item filtered;
    game_loop ()


let () = game_loop ()
