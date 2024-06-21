`prim.spi

rev' : [l: list, acc: list] -> list
    ? list == nil -> acc
    -> acc' := l.car[].cons[acc] 
        => rev'[l.cdr[], acc']
rev : [l: list] -> list rev'[l, nil]

apply' : [f: (int) -> int, l: list, acc: list] -> list
    ? list == nil -> acc.rev[]
    -> acc' := cons[l.car[].f[], acc]
        => apply'[f, l.cdr[], acc']
apply : [f: (int) -> int, l: list] -> list apply'[f, l, nil]

nth : [l: list, n: int] -> int
    ? n = 0 -> car[l]
    -> nth[cdr[l], n - 1]

main : [] -> int
    a := {1, 2, 3, 4, 5},
    a4 := a.nth[4]
    => a4
