// â”€â”€ VOLT Etape 11: Modules + Builtins â”€â”€

use "mylib/math"
use "mylib/greet"

// 1. Imported math functions
print(square(5))
// â†’ 25

// 2. Cube
print(cube(3))
// â†’ 27

// 3. Boolean return
print(is_even(4))
print(is_even(7))
// â†’ true, false

// 4. Recursive imported function
print(factorial(5))
// â†’ 120

// 5. Multi-param imported function
print(clamp(15, 0, 10))
print(clamp(-5, 0, 10))
print(clamp(5, 0, 10))
// â†’ 10, 0, 5

// 6. Imported greet functions
hello("VOLT")
goodbye("bugs")
// â†’ Hello, VOLT!
// â†’ Goodbye, bugs!

// 7. type() builtin
print(type(42))
print(type(3.14))
print(type("hello"))
print(type(true))
print(type([1, 2, 3]))
// â†’ int, float, str, bool, array

// 8. int() builtin - truncation
print(int(3.7))
print(int(9.9))
print(int(-2.8))
// â†’ 3, 9, -2

// 9. int() from string
print(int("42"))
// â†’ 42

// 10. float() from string
print(float("3.14"))
// â†’ 3.14

// 11. Combine imports with existing features
nums := [1, 2, 3, 4, 5]
squares := nums.map(|x| square(x))
print(squares)
// â†’ [1, 4, 9, 16, 25]

// 12. Filter with imported function
evens := nums.filter(|x| is_even(x))
print(evens)
// â†’ [2, 4]

// 13. type() in match
fn describe(val)
    match type(val)
        "int" -> print(f"{val} is an integer")
        "str" -> print(f"{val} is a string")
        _ -> print(f"{val} is something else")

describe(42)
describe("hello")
describe(3.14)
// â†’ 42 is an integer, hello is a string, 3.14 is something else

// 14. Duplicate import ignored
use "mylib/math"
print(square(6))
// â†’ 36