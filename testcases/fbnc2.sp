fib0 : [int] -> int
fib1 : [int] -> int
fib2 : [int] -> int

main: [] -> int =
    fib0[4]

fib0 : [n: int] -> int =
    ? n >= 2 -> fib1[n]
    -> 1

fib1 : [n: int] -> int =
    ? n >= 4 -> fib2[n]
    -> fib0[n-1] + fib0[n-2]

fib2 : [n: int] -> int =
    fib1[n-1] + fib1[n-2]
