

# let int fact(x int) =
#     if x = 0 then
#         1
#     else
#         x * fact(x - 1)

let int foo(a int) = 4 + a

let int fib(x int, y int) = foo(x + 2 + y + 4)

let int main() = fib(4, 5)

let bool equal(a int, b int) = a = b
