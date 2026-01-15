let s = "Hello World"
let i = ref 0

let next () : char option =
    if !i < String.length s then (
        i := !i + 1;
        Some (s.[!i - 1])
    ) else (
        None
    )
