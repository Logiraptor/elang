
type bool = i1
type char = i8
type int = i32

type managed_string = struct(data str, len i32)

extern int strlen(s str)
extern str malloc(size int)
extern str strcpy(dst str, src str)
extern int puts(s str)
extern str gets(s str)
# we can define a limited version of printf that only works for one int
extern int printf(s str, d int)

let int print_str(s managed_string) =
    let _ = puts(s.data) in
    let _ = print_int(s.len) in
    0

let int print_int(x int) =
    printf("%d\n", x)

let int fizz_buzz(n int) =
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

let int main() =
    let msg = "structures are working!" in
    let ms = {data=msg, len=strlen(msg)} in
    print_str(ms)
