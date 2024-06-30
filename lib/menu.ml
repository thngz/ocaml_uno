open Common

module Menu = Menu
type menu_item = {title: string; shortcut: string; action: unit -> message};;

type model = {
    items: menu_item list;
    view: unit -> unit;
}

let draw_item (item: menu_item) =
    Printf.printf "%s) %s\n" item.shortcut item.title

let draw (items: menu_item list) = List.iter draw_item items

let menu_items: menu_item list = [ 
    {title = "Start"; shortcut= "1"; action=(fun () -> Start)};
    {title = "Options"; shortcut= "2"; action=(fun () -> Options)};
    {title = "View previous games"; shortcut= "3"; action=(fun () -> ViewGames)};
    {title = "Exit"; shortcut= "e"; action=(fun () -> Exit)};
];;

let options_menu_items: menu_item list = [ 
    {title = "Foo"; shortcut= "1"; action=(fun () -> Start)};
    {title = "Bar"; shortcut= "2"; action=(fun () -> Options)};
    {title = "Baz"; shortcut= "3"; action=(fun () -> ViewGames)};
    {title = "Back"; shortcut= "b"; action=(fun () -> Start)};
    {title = "Exit"; shortcut= "e"; action=(fun () -> Exit)};
];;

let create_menu items = {
    items = items;
    view = (fun () -> draw items) 
}
