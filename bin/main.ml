open Unogame.Menu
open Printf
let menu_items: menu_item list = [ 
    {title = "Start"; shortcut= "1"; action=(fun () -> print_endline "start")};
    {title = "Options"; shortcut= "2"; action=(fun () -> print_endline "options")};
    {title = "View previous games"; shortcut= "3"; action=(fun () -> print_endline "prev")};
    {title = "Exit"; shortcut= "e"; action=(fun () -> print_endline "Exiting"; exit 0)};
];;

let rec game_loop () =
    draw menu_items;
    let selectionText = read_line () in
    let filtered = List.filter (fun x -> x.shortcut = selectionText) menu_items in 
    match filtered with
            | [] -> print_endline "Invalid choice!"
            | x :: _ ->  printf "You chose: ";
                    List.iter draw_item filtered;
                    x.action ();
                
    game_loop ()


let () = game_loop ()
