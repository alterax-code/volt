// â”€â”€ VOLT Etape 12: Types + Maps â”€â”€

// 1. Typed function params
fn add(a: int, b: int) -> int
    return a + b

print(add(3, 4))
// â†’ 7

// 2. Type error caught
try
    add("hello", 3)
catch e
    print(e)
// â†’ type error: expected int, got str

// 3. Typed let
x: int := 42
print(x)
// â†’ 42

// 4. Typed let error
try
    y: str := 123
catch e
    print(e)
// â†’ type error: expected str, got int

// 5. Float type
fn half(x: float) -> float
    return x / 2.0

print(half(10.0))
// â†’ 5

// 6. String type
fn greet(name: str) -> str
    return f"hello {name}"

print(greet("world"))
// â†’ hello world

// 7. Bool type
fn negate(b: bool) -> bool
    if b
        return false
    return true

print(negate(true))
print(negate(false))
// â†’ false, true

// 8. Array type
fn first(arr: array)
    print(arr[0])

first([10, 20, 30])
// â†’ 10

// 9. Map literal
m := {name: "VOLT", version: 42}
print(m)
// â†’ {name: VOLT, version: 42}

// 10. Map field access
print(m.name)
print(m.version)
// â†’ VOLT, 42

// 11. Map type check
fn show_map(data: map)
    print(data.title)

show_map({title: "hello"})
// â†’ hello

// 12. type() on map
print(type(m))
// â†’ map

// 13. Multiple typed params
fn range_check(val: int, lo: int, hi: int) -> bool
    return val >= lo and val <= hi

print(range_check(5, 1, 10))
print(range_check(15, 1, 10))
// â†’ true, false

// 14. Mixed typed/untyped params
fn repeat(s: str, n)
    result := ""
    for i in 0..n
        result = result + s
    print(result)

repeat("ab", 3)
// â†’ ababab

// 15. Int rejects float
try
    add(3.14, 2)
catch e
    print(e)
// â†’ type error: expected int, got float

// 16. Typed mut
mut counter: int := 0
counter = counter + 1
print(counter)
// â†’ 1