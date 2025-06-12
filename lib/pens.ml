(* Color codes and stuff https://jakob-bagterp.github.io/colorist-for-python/ansi-escape-codes/standard-16-colors/#foreground-text-and-background-colors *)

type pen_color =
        | Red
        | Orange
        | Yellow
        | Green
        | Blue
        | Purple

let change_pen = function
        | Red -> "red"
        | Orange -> "orange"
        | Yellow -> "yellow"
        | Green -> "green"
        | Blue -> "blue"
        | Purple -> "purple"
