// â”€â”€ VOLT Data A: File I/O + Pipe Operator â”€â”€

// 1. Write file
write_file("test_output.txt", "Hello from VOLT!")
print("written")

// 2. Read file
content := read_file("test_output.txt")
print(content)

// 3. Append file
append_file("test_output.txt", "\nSecond line")
content2 := read_file("test_output.txt")
print(content2)

// 4. Read error caught
try
    bad := read_file("nonexistent.txt")
catch e
    print("caught file error")

// 5. Basic pipe
fn double(x)
    return x * 2

result := 5 |> double()
print(result)

// 6. Pipe chain
fn triple(x)
    return x * 3

result2 := 2 |> double() |> triple()
print(result2)

// 7. Pipe with extra args
fn add(a, b)
    return a + b

result3 := 10 |> add(5)
print(result3)

// 8. Pipe with lambdas
fn apply(val, f)
    return f(val)

result4 := 7 |> apply(|x| x * x)
print(result4)

// 9. Pipe into print
"VOLT pipes work!" |> print()

// 10. Write and read back data
data := [10, 20, 30]
write_file("data.txt", str(data))
loaded := read_file("data.txt")
print(f"loaded: {loaded}")

// 11. String pipe chain
fn upper(s)
    return s.upper()

"hello volt" |> upper() |> print()

// 12. Pipe with array methods (via wrapper functions)
fn do_filter(arr, f)
    return arr.filter(f)

fn do_map(arr, f)
    return arr.map(f)

fn do_reduce(arr, f, init)
    return arr.reduce(f, init)

nums := [1, 2, 3, 4, 5]
total := nums |> do_filter(|x| x > 2) |> do_map(|x| x * 10) |> do_reduce(|a, b| a + b, 0)
print(total)

// Cleanup
write_file("test_output.txt", "")
write_file("data.txt", "")
print("done")