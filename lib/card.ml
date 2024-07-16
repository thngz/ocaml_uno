open Printf

type card_color = Red | Blue | Green | Yellow

type card_value =
  | Value0
  | Value1
  | Value2
  | Value3
  | Value4
  | Value5
  | Value6
  | Value7
  | Value8
  | Value9
  | ValueSkip
  | ValueReverse
  | ValueDrawTwo
  | ValueDrawFour
  | ValueWild

type card = { color : card_color; card_value : card_value }
type cards = card list

let create_deck =
  let all_colors = [ Red; Blue; Green; Yellow ] in
  let all_values =
    [
      Value0;
      Value1;
      Value2;
      Value3;
      Value4;
      Value5;
      Value6;
      Value7;
      Value8;
      Value9;
      ValueSkip;
      ValueReverse;
      ValueDrawTwo;
      ValueDrawFour;
      ValueWild;
    ]
  in
  List.concat_map
    (fun color ->
      List.map (fun value -> { color; card_value = value }) all_values)
    all_colors

let card_to_string card =
  let color_to_string = function
    | Red -> "Red"
    | Blue -> "Blue"
    | Green -> "Green"
    | Yellow -> "Yellow"
  in
  let value_to_string = function
    | Value0 -> "0"
    | Value1 -> "1"
    | Value2 -> "2"
    | Value3 -> "3"
    | Value4 -> "4"
    | Value5 -> "5"
    | Value6 -> "6"
    | Value7 -> "7"
    | Value8 -> "8"
    | Value9 -> "9"
    | ValueSkip -> "Skip"
    | ValueReverse -> "Reverse"
    | ValueDrawTwo -> "Draw Two"
    | ValueDrawFour -> "Draw Four"
    | ValueWild -> "Wild"
  in
  sprintf "%s of %s"
    (value_to_string card.card_value)
    (color_to_string card.color)

let draw_cards cards =
   List.iter (fun card -> print_endline (card_to_string card)) cards 
