
extern i32 strlen(s str)
extern str malloc(size i32)
extern str strcpy(dst str, src str)
extern i32 puts(s str)
extern str gets(s str)
# we can define a limited version of printf that only works for one i32
extern i32 printf(s str, d i32)

let i32 print_int(x i32) =
    printf("%d\n", x)

let i32 fizz_buzz(n i32) =
    if n = 0 then n else
    let _ = fizz_buzz(n - 1) in
    if n % 3 = 0 & n % 5 = 0 then
        puts("FizzBuzz") 
    else if n % 3 = 0 then
        puts("Fizz")
    else if n % 5 = 0 then
        puts("Buzz")
    else
        print_int(n)

let i32 main() = fizz_buzz(100)
