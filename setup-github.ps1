## VOLT - Phase E: GitHub README + CI - v2
Write-Host "`n  VOLT - Setting up GitHub files...`n" -ForegroundColor Cyan

New-Item -ItemType Directory -Force -Path ".github/workflows" | Out-Null
New-Item -ItemType Directory -Force -Path "examples" | Out-Null

$f0 = @'
# ⚡ VOLT

**A fast, expressive scripting language built in Rust.**

VOLT combines Python-like syntax with a high-performance bytecode VM. It compiles to NaN-boxed bytecode and runs at ~150x the speed of a tree-walking interpreter — all in ~3,000 lines of zero-dependency Rust.

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
- Indentation-based syntax — no braces, no semicolons
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
- NaN-boxed bytecode VM — no heap allocation for numbers/bools
- String interning — identical strings share one allocation
- `fib(30)` in **~8ms** (release), full 5-benchmark suite in **~51ms**
- Zero dependencies — just `rustc`

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
Source → Lexer → Parser → Compiler → VM
 .volt    tokens   AST    bytecode   execute
```

| Component | File | Lines | Role |
|-----------|------|-------|------|
| Lexer | `src/lexer/` | 280 | Tokenization with indentation tracking |
| Parser | `src/parser/` | 495 | Pratt parser → AST |
| Compiler | `src/vm/compiler.rs` | 706 | AST → bytecode with 90+ opcodes |
| VM | `src/vm/mod.rs` | 1134 | NaN-boxed stack machine |
| Values | `src/vm/value.rs` | 141 | Heap objects, string interning |
| Library | `src/lib.rs` | 108 | Public embedding API |
| CLI | `src/main.rs` | 321 | REPL + file runner + AST printer |
| **Total** | | **~3,200** | **Zero dependencies** |

### Bytecode VM

VOLT uses NaN-boxing to pack all values into 64-bit floats:

- Numbers → IEEE 754 doubles (no boxing overhead)
- Booleans, nil → special NaN bit patterns
- Objects (strings, arrays, closures, tables) → NaN + heap index

This means zero allocations for arithmetic — the VM operates entirely on a flat `[f64]` stack.

---

## Benchmarks

Release build on AMD Ryzen / Windows 11:

| Benchmark | Time |
|-----------|------|
| `fib(30)` recursive | ~8ms |
| `sum(0..100k)` loop | <1ms |
| String concat ×1000 | <1ms |
| Array reduce 10k | <1ms |
| Table pipeline ×1000 | ~40ms |
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

MIT © 2025
'@
[System.IO.File]::WriteAllText("$PWD/README.md", $f0)
Write-Host "  [OK] README.md" -ForegroundColor Green

$f1 = @'
/target
Cargo.lock
**/*.rs.bk
*.pdb
*.exe
*.dll
*.so
*.dylib

# Test artifacts
tests/programs/output.csv
tests/programs/test_*.txt
tests/programs/data.txt
'@
[System.IO.File]::WriteAllText("$PWD/.gitignore", $f1)
Write-Host "  [OK] .gitignore" -ForegroundColor Green

$f2 = @'
MIT License

Copyright (c) 2025 Lucas Jacob

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
'@
[System.IO.File]::WriteAllText("$PWD/LICENSE", $f2)
Write-Host "  [OK] LICENSE" -ForegroundColor Green

$f3 = @'
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

env:
  CARGO_TERM_COLOR: always

jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - name: Install Rust
        uses: dtolnay/rust-toolchain@stable

      - name: Cache cargo
        uses: actions/cache@v4
        with:
          path: |
            ~/.cargo/registry
            ~/.cargo/git
            target
          key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}

      - name: Build
        run: cargo build --verbose

      - name: Build release
        run: cargo build --release

      - name: Check warnings
        run: cargo build 2>&1 | grep -c "warning" | xargs test 0 -eq
        shell: bash

      - name: Run pipe tests
        run: cargo run -- run tests/programs/pipe_test.volt

      - name: Run table tests
        run: cargo run -- run tests/programs/table_test.volt

      - name: Run benchmarks
        run: cargo run --release -- run tests/programs/bench.volt

      - name: Run embedding example
        run: cargo run --example embed

  clippy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          components: clippy
      - run: cargo clippy -- -D warnings
'@
[System.IO.File]::WriteAllText("$PWD/.github/workflows/ci.yml", $f3)
Write-Host "  [OK] .github/workflows/ci.yml" -ForegroundColor Green

$f4 = @'
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
'@
[System.IO.File]::WriteAllText("$PWD/examples/hello.volt", $f4)
Write-Host "  [OK] examples/hello.volt" -ForegroundColor Green

$f5 = @'
// Data Pipeline Example in VOLT
// Demonstrates Table, CSV, and pipe operator

t := load_csv("tests/programs/users.csv")
print("=== Employee Database ===")
print(t)

// Who earns above average?
avg_salary := t.avg("salary")
print(f"\nAverage salary: {avg_salary}")

above_avg := t.where(|r| r.salary > avg_salary).select("name", "city", "salary").sort_by("salary", "desc")

print("\nAbove average earners:")
print(above_avg)

// City breakdown
print("\nEmployees per city:")
print(t.group_by("city"))

// Youngest in each city
print("\nYoungest employee:")
youngest := t.sort_by("age").limit(1)
print(youngest)

// Pipeline with pipe operator
fn double(x)
    return x * 2

result := 5 |> double()
print(f"\n5 |> double() = {result}")

// Save filtered results
save_csv("tests/programs/output.csv", above_avg)
print("\nSaved above_avg to output.csv")
'@
[System.IO.File]::WriteAllText("$PWD/examples/pipeline.volt", $f5)
Write-Host "  [OK] examples/pipeline.volt" -ForegroundColor Green

$f6 = @'
/// Example: Embedding VOLT in a Rust application
///
/// Run with: cargo run --example embed
fn main() {
    // One-liner: run VOLT source directly
    println!("--- Run inline code ---");
    volt::run(r#"
fn greet(name)
    print(f"Hello, {name}!")
greet("World")
"#).unwrap();

    // Step-by-step: compile, then run
    println!("\n--- Compile + Run ---");
    let compiled = volt::compile(r#"
data := [1, 2, 3, 4, 5]
total := data.reduce(|a, b| a + b, 0)
print(f"Sum = {total}")
"#).unwrap();
    let mut vm = volt::VM::new(compiled);
    vm.run().unwrap();

    // Run a file
    println!("\n--- Run file ---");
    match volt::run_file("tests/programs/table_test.volt") {
        Ok(()) => println!("File executed successfully"),
        Err(e) => println!("Error: {}", e),
    }
}
'@
[System.IO.File]::WriteAllText("$PWD/examples/embed.rs", $f6)
Write-Host "  [OK] examples/embed.rs" -ForegroundColor Green


Write-Host "`n  GitHub files ready!" -ForegroundColor Cyan
Write-Host "  Test: cargo run -- run examples/pipeline.volt"
