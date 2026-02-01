// â”€â”€ VOLT Data B: Table + CSV â”€â”€

// 1. Load CSV
t := load_csv("tests/programs/users.csv")
print(type(t))
// â†’ table

// 2. Row count
print(t.count())
// â†’ 8

// 3. Columns
print(t.columns())
// â†’ [name, age, city, salary]

// 4. Head
print(t.head(3))

// 5. Select columns
t2 := t.select("name", "salary")
print(t2.head(2))

// 6. Sort by age
t3 := t.sort_by("age")
print(t3.head(3))

// 7. Sort descending
t4 := t.sort_by("salary", "desc")
print(t4.head(3))

// 8. Where (filter)
paris := t.where(|r| r.city == "Paris")
print(paris.count())
// â†’ 3

// 9. Aggregations
print(t.sum("salary"))
print(t.avg("salary"))
print(t.min("age"))
print(t.max("age"))

// 10. Column extraction
ages := t.col("age")
print(ages)

// 11. Group by
groups := t.group_by("city")
print(groups)

// 12. Chained pipeline
result := t.where(|r| r.age > 28).select("name", "salary").sort_by("salary", "desc").limit(3)
print(result)

// 13. Save CSV
save_csv("tests/programs/output.csv", result)
check := load_csv("tests/programs/output.csv")
print(check.count())

// 14. Pipe with table
fn show_count(tbl)
    return tbl.count()

c := t |> show_count()
print(f"piped count: {c}")

print("done")