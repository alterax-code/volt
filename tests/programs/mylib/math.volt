fn square(x)
    return x * x

fn cube(x)
    return x * x * x

fn is_even(n)
    return n % 2 == 0

fn factorial(n)
    if n <= 1
        return 1
    return n * factorial(n - 1)

fn clamp(x, lo, hi)
    if x < lo
        return lo
    if x > hi
        return hi
    return x