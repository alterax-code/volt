// â”€â”€ VOLT Etape 10: Pattern Matching â”€â”€

// 1. Basic match with int literals
x := 2
match x
    1 -> print("one")
    2 -> print("two")
    3 -> print("three")
    _ -> print("other")
// â†’ two

// 2. Match with strings
name := "bob"
match name
    "alice" -> print("hi alice")
    "bob" -> print("hi bob")
    _ -> print("hi stranger")
// â†’ hi bob

// 3. Wildcard binding
y := 42
match y
    0 -> print("zero")
    n -> print(f"got: {n}")
// â†’ got: 42

// 4. Or patterns
z := 3
match z
    1 | 2 -> print("small")
    3 | 4 | 5 -> print("medium")
    _ -> print("big")
// â†’ medium

// 5. Match in function
fn classify(n)
    match n
        0 -> print("zero")
        1 -> print("one")
        _ -> print("many")

classify(0)
classify(1)
classify(7)
// â†’ zero, one, many

// 6. Match with booleans
flag := true
match flag
    true -> print("yes")
    false -> print("no")
// â†’ yes

// 7. Match in loop
for i in 0..5
    match i % 3
        0 -> print(f"{i}:fizz")
        1 -> print(f"{i}:one")
        _ -> print(f"{i}:other")
// â†’ 0:fizz 1:one 2:other 3:fizz 4:one

// 8. Match with block body
val := 10
match val
    0 -> print("zero")
    _
        doubled := val * 2
        print(f"doubled: {doubled}")
// â†’ doubled: 20

// 9. Match with negative
n := -1
match n
    -1 -> print("neg one")
    0 -> print("zero")
    1 -> print("pos one")
// â†’ neg one

// 10. Or pattern with strings
color := "blue"
match color
    "red" | "orange" | "yellow" -> print("warm")
    "blue" | "green" -> print("cool")
    _ -> print("unknown")
// â†’ cool