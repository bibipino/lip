open Adder

let%test _ = addlist [] = 0
let%test _ = addlist [3] = 3
let%test _ = addlist [1;2] = 3
let%test _ = addlist [1;2;3] = 6
let%test _ = addlist [2;3;4] = 9
let%test _ = addlist [3;4;5] = 12