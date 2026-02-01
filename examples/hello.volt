// Hello World in VOLT
print("Hello, World!")

name := "VOLT"
version := 1.0
print(f"{name} v{version} is running!")

// Quick demo
fn fizzbuzz(n)
    for i in 1..n
        if i % 15 == 0
            print("FizzBuzz")
        elif i % 3 == 0
            print("Fizz")
        elif i % 5 == 0
            print("Buzz")
        else
            print(i)

fizzbuzz(20)