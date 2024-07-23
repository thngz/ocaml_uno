open Printf

let paint_red str = sprintf "\027[31m%s\027[0m" str
let paint_green str = sprintf "\027[92m%s\027[0m" str
let paint_yellow str = sprintf "\027[93m%s\027[0m" str
let paint_blue str = sprintf "\027[94m%s\027[0m" str
