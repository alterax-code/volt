// â”€â”€ VOLT Etape 9: Error Handling â”€â”€

// 1. Basic try/catch with throw
try
    throw "something went wrong"
catch e
    print(f"Caught: {e}")
// â†’ Caught: something went wrong

// 2. No error â†’ catch skipped
try
    x := 42
    print(x)
catch e
    print("should not print")
// â†’ 42

// 3. Division by zero (catchable runtime error)
try
    result := 10 / 0
    print("should not print")
catch e
    print(f"Math error: {e}")
// â†’ Math error: division by zero

// 4. Throw from function
fn risky(n)
    if n < 0
        throw f"negative: {n}"
    return n * 2

try
    print(risky(5))
    print(risky(-3))
catch e
    print(f"Error: {e}")
// â†’ 10
// â†’ Error: negative: -3

// 5. Nested try/catch
try
    try
        throw "inner error"
    catch e
        print(f"Inner caught: {e}")
    print("after inner")
catch e
    print("should not print")
// â†’ Inner caught: inner error
// â†’ after inner

// 6. Rethrow from catch
try
    try
        throw "original"
    catch e
        throw f"wrapped: {e}"
catch e
    print(e)
// â†’ wrapped: original

// 7. Error in loop with break
mut count := 0
while count < 10
    if count == 5
        break
    count = count + 1
print(f"count: {count}")
// â†’ count: 5

// 8. Continue in loop
mut sum := 0
for i in 0..10
    if i % 2 == 0
        continue
    sum = sum + i
print(f"odd sum: {sum}")
// â†’ odd sum: 25

// 9. Try/catch in loop
mut errors := 0
for i in 0..5
    try
        if i == 2
            throw "boom"
        if i == 4
            throw "bang"
    catch e
        errors = errors + 1
print(f"errors: {errors}")
// â†’ errors: 2

// 10. Error value is a string
try
    throw 42
catch e
    print(f"type test: {e}")
// â†’ type test: 42