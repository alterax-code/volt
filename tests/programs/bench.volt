// â”€â”€ VOLT Benchmark Suite â”€â”€

// 1. Fibonacci (recursive)
fn fib(n)
    if n < 2
        return n
    return fib(n - 1) + fib(n - 2)

result := fib(30)
print(f"fib(30) = {result}")

// 2. Loop performance
sum := 0
for i in 0..100000
    sum = sum + i
print(f"sum(0..100k) = {sum}")

// 3. String operations
s := ""
for i in 0..1000
    s = s + "a"
print(f"string concat 1000x len = {s.len()}")

// 4. Array operations
arr := []
for i in 0..10000
    arr.push(i)
total := arr.reduce(|a, b| a + b, 0)
print(f"array reduce 10k = {total}")

// 5. Table operations
try
    t := load_csv("tests/programs/users.csv")
    last := t
    for i in 0..1000
        last = t.where(|row| row.age > 28).select("name", "salary")
    print(f"table pipeline 1000x done, last count = {last.count()}")
catch e
    print(f"table bench skipped: {e}")

print("bench done")