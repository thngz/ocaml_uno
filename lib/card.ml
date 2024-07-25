open Printf
open Colors

type card_color = Red | Blue | Green | Yellow

type special_card =
  | ValueSkip
  | ValueReverse
  | ValueDrawTwo
  | ValueDrawFour
  | ValueWild

type common_card =
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

type card_value = SpecialCard of special_card | CommonCard of common_card
type card = { color : card_color; card_value : card_value }
type cards = card list

let create_deck =
  let all_colors = [ Red; Blue; Green; Yellow ] in
  let special_cards =
    [
      SpecialCard ValueSkip;
      SpecialCard ValueReverse;
      SpecialCard ValueDrawTwo;
      SpecialCard ValueDrawFour;
      SpecialCard ValueWild;
    ]
  in
  let common_cards =
    [
      CommonCard Value0;
      CommonCard Value1;
      CommonCard Value2;
      CommonCard Value3;
      CommonCard Value4;
      CommonCard Value5;
      CommonCard Value6;
      CommonCard Value7;
      CommonCard Value8;
      CommonCard Value9;
    ]
  in
  let all_cards = special_cards @ common_cards in
  List.concat_map
    (fun color ->
      List.map (fun value -> { color; card_value = value }) all_cards)
    all_colors

let card_to_string card =
  let color_to_string = function
    | Red -> paint_red "Red"
    | Blue -> paint_blue "Blue"
    | Green -> paint_green "Green"
    | Yellow -> paint_yellow "Yellow"
  in
  let value_to_string = function
    | SpecialCard ValueSkip -> "Skip"
    | SpecialCard ValueReverse -> "Reverse"
    | SpecialCard ValueDrawTwo -> "Draw Two"
    | SpecialCard ValueDrawFour -> "Draw Four"
    | SpecialCard ValueWild -> "Wild"
    | CommonCard Value0 -> "0"
    | CommonCard Value1 -> "1"
    | CommonCard Value2 -> "2"
    | CommonCard Value3 -> "3"
    | CommonCard Value4 -> "4"
    | CommonCard Value5 -> "5"
    | CommonCard Value6 -> "6"
    | CommonCard Value7 -> "7"
    | CommonCard Value8 -> "8"
    | CommonCard Value9 -> "9"
  in

  sprintf "%s of %s"
    (value_to_string card.card_value)
    (color_to_string card.color)

let draw_cards cards =
  List.iteri (fun i card -> printf "%d) %s \n" i (card_to_string card)) cards

let shuffle_cards (cards : cards) =
  Random.self_init ();
  let nd = List.map (fun c -> (Random.bits (), c)) cards in
  let sond = List.sort compare nd in
  List.map snd sond
