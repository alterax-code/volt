// Hello World in VOLT
fn add(a: int, b: int) -> int
    a + b

fn main()
    x := 42
    name := "Lucas"
    result := add(x, 8)

    if result > 40
        print("Big number!")
    else
        print("Small number")

    for i in 0..10
        print(i)