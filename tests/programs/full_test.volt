// VOLT Full Test

struct Point
    x: f64
    y: f64

    fn distance(self, other: Point) -> f64
        dx := self.x - other.x
        dy := self.y - other.y
        sqrt((dx * dx) + (dy * dy))

fn fibonacci(n: int) -> int
    if n <= 1
        return n
    fibonacci(n - 1) + fibonacci(n - 2)

fn main()
    x := 42
    name := "Lucas"
    mut counter := 0
    items := [1, 2, 3, 4, 5]

    result := fibonacci(10)

    if result > 40 and x == 42
        print("Big!")
    elif result > 20
        print("Medium")
    else
        print("Small")

    for i in 0..10
        counter = counter + 1
        print(i)

    a := Point(0.0, 0.0)
    b := Point(3.0, 4.0)
    dist := a.distance(b)
    print(dist)

    while counter > 0
        counter = counter - 1