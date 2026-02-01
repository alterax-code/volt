// â”€â”€ VOLT Etape 8: Array Methods â”€â”€

// 1. push (mutates)
mut arr := [1, 2, 3]
arr.push(4)
print(arr)
// â†’ [1, 2, 3, 4]

// 2. pop (mutates, returns removed)
val := arr.pop()
print(val, arr)
// â†’ 4 [1, 2, 3]

// 3. len (as method)
print(arr.len())
// â†’ 3

// 4. map
doubled := [1, 2, 3, 4].map(|x| x * 2)
print(doubled)
// â†’ [2, 4, 6, 8]

// 5. filter
evens := [1, 2, 3, 4, 5, 6].filter(|x| x % 2 == 0)
print(evens)
// â†’ [2, 4, 6]

// 6. reduce
sum := [1, 2, 3, 4, 5].reduce(|acc, x| acc + x, 0)
print(sum)
// â†’ 15

// 7. forEach
[10, 20, 30].for_each(|x| print(x))
// â†’ 10
// â†’ 20
// â†’ 30

// 8. find
found := [1, 2, 3, 4, 5].find(|x| x > 3)
print(found)
// â†’ 4

// 9. sort
sorted := [3, 1, 4, 1, 5, 9].sort()
print(sorted)
// â†’ [1, 1, 3, 4, 5, 9]

// 10. reverse
rev := [1, 2, 3].reverse()
print(rev)
// â†’ [3, 2, 1]

// 11. join
str_val := ["hello", "world"].join(" ")
print(str_val)
// â†’ hello world

// 12. chaining: filter + map + join
result := [1, 2, 3, 4, 5].filter(|x| x > 2).map(|x| x * 10).join(", ")
print(result)
// â†’ 30, 40, 50

// 13. Index assignment
mut data := [10, 20, 30]
data[1] = 99
print(data)
// â†’ [10, 99, 30]

// 14. map with named function
fn square(x)
    return x * x

print([1, 2, 3].map(square))
// â†’ [1, 4, 9]