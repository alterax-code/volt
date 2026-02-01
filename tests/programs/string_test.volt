fn main()
    // String concat with +
    greeting := "Hello" + " " + "World"
    print(greeting)

    // Auto-conversion with str()
    answer := "The answer is " + str(42)
    print(answer)

    // F-string interpolation
    name := "Lucas"
    age := 28
    msg := f"Hello {name}, you are {age} years old!"
    print(msg)

    // F-string with expressions
    x := 10
    y := 20
    print(f"{x} + {y} = {x + y}")

    // String methods
    text := "Hello World"
    print(text.upper())
    print(text.lower())
    print(text.len())
    print(text.contains("World"))
    print(text.replace("World", "VOLT"))

    // Trim
    padded := "  spaces  "
    print(padded.trim())

    // Split
    csv := "a,b,c,d"
    parts := csv.split(",")
    print(parts)
    print(len(parts))

    // String equality (value-based)
    a := "hello"
    b := "hel" + "lo"
    print(a == b)

    // Starts/ends with
    path := "/home/lucas/volt"
    print(path.starts_with("/home"))
    print(path.ends_with(".volt"))