
# Fib Recursive

```go
func fact(n int) int {
    if n <= 1 {
        return 1
    }
    return fact(n-1) * n
}
```

```bc

# fact(n int) int
# local 0 : n
fact_cb123c:

# if n <= 1
local 0
const 1
cmp
jle alt_e2b4e3
# return 1
const 1
ret

alt_e2b4e3:
local 0
const 1
sub
call fact_cb123c
local 0
mul
ret

```


```regalloc

# fact(n int) int
# %arg0 : n
fact_cb123c:

# if n <= 1
cmp %arg0 1
jle alt_e2b4e3
# return 1
mov %retval %arg0
ret

alt_e2b4e3:
sub %temp0 %arg0 1
push %arg0
call fact_cb123c
pop %arg0
mul %retval %retval %arg0
ret

```

