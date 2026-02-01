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