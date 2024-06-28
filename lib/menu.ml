type menu_item = {title: string; shortcut: string; action: unit -> unit}

let draw_item item =
    Printf.printf "%s) %s\n" item.shortcut item.title

let draw items = List.iter draw_item items
