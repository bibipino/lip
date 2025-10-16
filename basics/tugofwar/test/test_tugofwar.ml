open Tugofwar

let%test _ = toklist_of_string "AAAA=BBB" = [A;A;A;A;X;B;B;B]
let%test _ = try toklist_of_string "ABC" |> fun _ -> false with _ -> true
let%test _ = valid [A;A;A;A;X;B;B;B] = true
let%test _ = valid [A;A;X;A;X;B;B;B] = false
let%test _ = valid [A;A;X;B;X;B;B;B] = false
let%test _ = valid [A;A;B;A;X;B;A;B] = false
let%test _ = valid [] = true
let%test _ = valid [X] = true
let%test _ = win [A;A;X;B] = A
let%test _ = win [A;X;B] = X
let%test _ = win [A;X;B;B] = B



