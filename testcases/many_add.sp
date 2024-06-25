`prim.spi

add5: [a1: int, a2: int, a3: int, a4: int, a5: int] -> int
    a123 := a1 + a2 + a3,
    a45 := a4 + a5,
    a_all := a123 + a45
    => a123 + a45

add_if_even: [acc: int, n: int] -> int
    acc := 
        ? n mod 2 = 0 -> acc + n
        -> acc
    => acc

main: [] -> int
    prints["hello world"];
    0
