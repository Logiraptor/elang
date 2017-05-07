
let i32 fact(x i32) =
    if x = 0 then
        1
    else
        x * fact(x - 1)

let i32 foo(a i32) = 4 + a

let i32 fib(x i32, y i32) = foo(x + 2 + y + 4)

let str main() = "hello"

let bool equal(a i32, b i32) = a = b