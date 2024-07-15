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
