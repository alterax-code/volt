// â”€â”€ VOLT Etape 7: Closures & Lambdas â”€â”€

// 1. Basic lambda (inline expression)
add := |a, b| a + b
print(add(3, 4))
// â†’ 7

// 2. Lambda stored in variable
double := |x| x * 2
print(double(5))
// â†’ 10

// 3. Higher-order function: pass function as argument
fn apply(f, x)
    return f(x)

print(apply(double, 21))
// â†’ 42

// 4. Closure captures outer variable
multiplier := 10
scale := |x| x * multiplier
print(scale(7))
// â†’ 70

// 5. Function returning a closure (factory)
fn make_adder(n)
    return |x| x + n

add5 := make_adder(5)
print(add5(10))
// â†’ 15

// 6. Named function passed as value
fn square(x)
    return x * x

print(apply(square, 6))
// â†’ 36

// 7. Closure with multiple captures
base := 100
bonus := 50
calc := |x| x + base + bonus
print(calc(5))
// â†’ 155

// 8. Lambda in for loop (build array of results)
nums := [1, 2, 3, 4, 5]
transform := |x| x * x + 1
mut results := ""
for n in nums
    mut r := transform(n)
    if results == ""
        results = str(r)
    else
        results = results + " " + str(r)
print(results)
// â†’ 2 5 10 17 26

// 9. Zero-param lambda
greet := || "hello"
print(greet())
// â†’ hello

// 10. Nested closure
fn outer(x)
    fn inner(y)
        return x + y
    return inner(10)

print(outer(5))
// â†’ 15