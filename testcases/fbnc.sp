fbnc : [n: int] -> int
    n' :=
        ? n = 0 -> 1
        | n = 1 -> 1
        ->
            n1 := fbnc[n - 1],
            n2 := fbnc[n - 2]
            => n1 + n2
    => n'


main : [] -> int
    ret := fbnc[20]
    => ret
