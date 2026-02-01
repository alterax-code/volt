// Fibonacci benchmark
fn fibonacci(n: int) -> int
    if n <= 1
        return n
    fibonacci(n - 1) + fibonacci(n - 2)

fn main()
    result := fibonacci(30)
    print(result)