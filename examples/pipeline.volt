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