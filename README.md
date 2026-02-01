# âš¡ VOLT

**A fast, expressive scripting language built in Rust.**

VOLT combines Python-like syntax with a high-performance bytecode VM. It compiles to NaN-boxed bytecode and runs at ~150x the speed of a tree-walking interpreter â€” all in ~3,000 lines of zero-dependency Rust.

```volt
t := load_csv("data.csv")

top := t.where(|r| r.salary > 50000).select("name", "salary").sort_by("salary", "desc").limit(5)

print(top)
```

```
name    | salary
--------+-------
Frank   | 72000
Charlie | 68000
Hugo    | 59000
Alice   | 55000
[4 rows x 2 cols]
```

---

## Features

**Language**
- Indentation-based syntax â€” no braces, no semicolons
- First-class functions, closures, lambdas (`|x| x * 2`)
- Pattern matching with or-patterns and bindings
- F-strings (`f"Hello, {name}!"`)
- Structs with methods
- Error handling (`try` / `catch` / `throw`)
- Module system (`use`)
- Runtime type checking (`is`, `as`)
- Maps, arrays with `filter`, `map`, `reduce`, `sort`, `find`

**Data Pipeline**
- `Table` type with SQL-like query methods
- CSV load/save (`load_csv`, `save_csv`)
- `where`, `select`, `sort_by`, `group_by`, `limit`
- Aggregations: `sum`, `avg`, `min`, `max`, `count`
- Pipe operator (`|>`) for chaining transforms
- File I/O (`read_file`, `write_file`, `append_file`)

**Performance**
- NaN-boxed bytecode VM â€” no heap allocation for numbers/bools
- String interning â€” identical strings share one allocation
- `fib(30)` in **~8ms** (release), full 5-benchmark suite in **~51ms**
- Zero dependencies â€” just `rustc`

---

## Quick Start

### Install

```bash
git clone https://github.com/YOUR_USER/volt.git
cd volt
cargo build --release
```

The binary is at `target/release/volt` (or `volt.exe` on Windows).

### Run a Program

```bash
volt run examples/hello.volt
```

### Interactive REPL

```bash
volt repl
```

```
  VOLT 1.0 REPL
  :help for commands, :quit to exit

>> 2 + 2
4
>> fn fib(n)
..     if n < 2
..         return n
..     return fib(n-1) + fib(n-2)
..
>> fib(10)
55
```

---

## Language Tour

### Variables & Types

```volt
name := "Lucas"
age := 28
pi := 3.14
active := true

mut count := 0
count = count + 1
```

### Functions

```volt
fn greet(name)
    print(f"Hello, {name}!")

fn add(a, b)
    return a + b

// Lambdas
double := |x| x * 2
nums := [1, 2, 3].map(|x| x * 10)
```

### Control Flow

```volt
if score > 90
    print("A")
elif score > 80
    print("B")
else
    print("C")

for i in 0..10
    print(i)

while x > 0
    x = x - 1
```

### Pattern Matching

```volt
match status
    200 -> print("OK")
    404 -> print("Not Found")
    500 | 502 | 503 -> print("Server Error")
    code -> print(f"Other: {code}")
```

### Structs

```volt
struct Point
    x: float
    y: float

    fn distance(self, other: Point) -> float
        dx := self.x - other.x
        dy := self.y - other.y
        return sqrt(dx * dx + dy * dy)

a := Point(0.0, 0.0)
b := Point(3.0, 4.0)
print(a.distance(b))  // 5.0
```

### Error Handling

```volt
try
    data := read_file("config.txt")
    print(data)
catch e
    print(f"Error: {e}")
```

### Arrays

```volt
nums := [3, 1, 4, 1, 5, 9]

nums.push(2)
nums.sort()
print(nums.len())

evens := nums.filter(|x| x % 2 == 0)
total := nums.reduce(|a, b| a + b, 0)
```

### Maps

```volt
user := {name: "Lucas", age: 28}
print(user.name)
user.email = "lucas@example.com"
```

### Pipe Operator

```volt
result := data
    |> parse()
    |> validate()
    |> transform()
    |> save()
```

### Tables & CSV

```volt
t := load_csv("users.csv")

// SQL-like queries
seniors := t.where(|r| r.age > 30).select("name", "salary").sort_by("salary", "desc")

print(seniors)
print(f"Average salary: {t.avg('salary')}")

// Group by
t.group_by("city")

// Save results
save_csv("output.csv", seniors)
```

### Modules

```volt
// math_utils.volt
pub fn square(x)
    return x * x

// main.volt
use "math_utils"
print(square(5))  // 25
```

---

## Embed in Rust

VOLT is also a Rust library. Add it to your project:

```rust
fn main() {
    // One-liner
    volt::run(r#"print("Hello from VOLT!")"#).unwrap();

    // Compile once, run many times
    let compiled = volt::compile("print(40 + 2)").unwrap();
    let mut vm = volt::VM::new(compiled);
    vm.run().unwrap();

    // Run a file
    volt::run_file("script.volt").unwrap();
}
```

---

## Architecture

```
Source â†’ Lexer â†’ Parser â†’ Compiler â†’ VM
 .volt    tokens   AST    bytecode   execute
```

| Component | File | Lines | Role |
|-----------|------|-------|------|
| Lexer | `src/lexer/` | 280 | Tokenization with indentation tracking |
| Parser | `src/parser/` | 495 | Pratt parser â†’ AST |
| Compiler | `src/vm/compiler.rs` | 706 | AST â†’ bytecode with 90+ opcodes |
| VM | `src/vm/mod.rs` | 1134 | NaN-boxed stack machine |
| Values | `src/vm/value.rs` | 141 | Heap objects, string interning |
| Library | `src/lib.rs` | 108 | Public embedding API |
| CLI | `src/main.rs` | 321 | REPL + file runner + AST printer |
| **Total** | | **~3,200** | **Zero dependencies** |

### Bytecode VM

VOLT uses NaN-boxing to pack all values into 64-bit floats:

- Numbers â†’ IEEE 754 doubles (no boxing overhead)
- Booleans, nil â†’ special NaN bit patterns
- Objects (strings, arrays, closures, tables) â†’ NaN + heap index

This means zero allocations for arithmetic â€” the VM operates entirely on a flat `[f64]` stack.

---

## Benchmarks

Release build on AMD Ryzen / Windows 11:

| Benchmark | Time |
|-----------|------|
| `fib(30)` recursive | ~8ms |
| `sum(0..100k)` loop | <1ms |
| String concat Ã—1000 | <1ms |
| Array reduce 10k | <1ms |
| Table pipeline Ã—1000 | ~40ms |
| **All 5 combined** | **~51ms** |

---

## Development

```bash
# Build
cargo build

# Run tests
cargo run -- run tests/programs/table_test.volt
cargo run -- run tests/programs/pipe_test.volt
cargo run -- run tests/programs/bench.volt

# Release build + benchmark
cargo build --release
./target/release/volt run tests/programs/bench.volt

# Run embedding example
cargo run --example embed
```

---

## License

MIT Â© 2025