
extern i32 strlen(s str)
extern str malloc(size i32)
extern str strcpy(dst str, src str)
extern i32 puts(s str)
extern i32 gets(s str)
# we can define a limited version of printf that only works for one i32
extern i32 printf(s str, d i32)

let i32 echo_inner(buf str) =
    let err = gets(buf) in
    if err = 0 then 0 else
        let _ = puts(buf) in
        echo_inner(buf)

let i32 echo(maxSize i32) =
    let buf = malloc(maxSize) in
    echo_inner(buf)

let i32 main() = echo(100)
