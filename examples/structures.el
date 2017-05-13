
type person = struct(
    name str,
    age i32,
    addr struct (
        street str,
        state str,
        country str
    )
)

extern i32 puts(s str)

let i32 print_person(p person) =
    let _ = puts(p.addr.street) in 0

let i32 main() =
    print_person({
        name="Structure Name",
        age=30,
        addr={
            street="Street Address",
            state="Louisiana",
            country="US"
        }
    })
