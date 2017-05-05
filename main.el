

let int foo(a int) = 4 + a

let int fib(x int, y int) = foo(x + 2 + y + 4)

let int main() = fib(4, 5)

let bool equal(a int, b int) = a = b
