open Printf

let safe_str_to_int convertable fallback convertable_name =
  match int_of_string_opt convertable with
  | Some p -> p
  | None ->
      printf "Please enter correct %s\n" convertable_name;
      fallback

let map_with_index f lst =
  let rec aux i = function [] -> [] | x :: xs -> f i x :: aux (i + 1) xs in
  aux 0 lst
