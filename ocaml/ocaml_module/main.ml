
let () =
    let v = ref (Mod.next ()) in
    while !v <> None do
        match !v with
        | None -> ()
        | Some c -> print_char c;
        v := Mod.next ()
    done
