/-
  Totem Tests
  Comprehensive test suite for TOML parsing
-/
import Totem
import Crucible

open Crucible
open Totem

-- Helper to check parse success
def shouldParse (input : String) : IO Unit := do
  match Totem.parse input with
  | .ok _ => pure ()
  | .error e => throw (IO.userError s!"Parse failed: {e}")

-- Helper to check parse failure
def shouldFailToParse (input : String) : IO Unit := do
  match Totem.parse input with
  | .ok _ => throw (IO.userError "Expected parse to fail, but it succeeded")
  | .error _ => pure ()

namespace Tests.Strings

testSuite "String Parsing"

test "basic string" := do
  match Totem.parse "key = \"hello world\"" with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "hello world"
  | .error e => throw (IO.userError s!"{e}")

test "escape sequences" := do
  match Totem.parse "key = \"tab:\\there\"" with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "tab:\there"
  | .error e => throw (IO.userError s!"{e}")

test "unicode escape 4-digit" := do
  match Totem.parse "key = \"\\u0048\\u0065\\u006C\\u006C\\u006F\"" with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "Hello"
  | .error e => throw (IO.userError s!"{e}")

test "literal string" := do
  match Totem.parse "key = 'no \\escapes here'" with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "no \\escapes here"
  | .error e => throw (IO.userError s!"{e}")

test "multi-line basic string" := do
  let input := "key = \"\"\"\nline1\nline2\"\"\""
  match Totem.parse input with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "line1\nline2"
  | .error e => throw (IO.userError s!"{e}")

test "multi-line literal string" := do
  let input := "key = '''\nno \\escape\nhere'''"
  match Totem.parse input with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "no \\escape\nhere"
  | .error e => throw (IO.userError s!"{e}")

end Tests.Strings

namespace Tests.Numbers

testSuite "Number Parsing"

test "positive integer" := do
  match Totem.parse "key = 42" with
  | .ok t =>
    let v ← t.getAs (α := Int) "key" |>.toIO
    v ≡ 42
  | .error e => throw (IO.userError s!"{e}")

test "negative integer" := do
  match Totem.parse "key = -17" with
  | .ok t =>
    let v ← t.getAs (α := Int) "key" |>.toIO
    v ≡ (-17)
  | .error e => throw (IO.userError s!"{e}")

test "hex integer" := do
  match Totem.parse "key = 0xDEAD" with
  | .ok t =>
    let v ← t.getAs (α := Int) "key" |>.toIO
    v ≡ 0xDEAD
  | .error e => throw (IO.userError s!"{e}")

test "octal integer" := do
  match Totem.parse "key = 0o755" with
  | .ok t =>
    let v ← t.getAs (α := Int) "key" |>.toIO
    v ≡ 0o755
  | .error e => throw (IO.userError s!"{e}")

test "binary integer" := do
  match Totem.parse "key = 0b1010" with
  | .ok t =>
    let v ← t.getAs (α := Int) "key" |>.toIO
    v ≡ 10
  | .error e => throw (IO.userError s!"{e}")

test "integer with underscores" := do
  match Totem.parse "key = 1_000_000" with
  | .ok t =>
    let v ← t.getAs (α := Int) "key" |>.toIO
    v ≡ 1000000
  | .error e => throw (IO.userError s!"{e}")

test "float" := do
  match Totem.parse "key = 3.14" with
  | .ok t =>
    let v ← t.getAs (α := Float) "key" |>.toIO
    if (v - 3.14).abs < 0.001 then pure ()
    else throw (IO.userError s!"expected ~3.14, got {v}")
  | .error e => throw (IO.userError s!"{e}")

test "float with exponent" := do
  match Totem.parse "key = 1e10" with
  | .ok t =>
    let v ← t.getAs (α := Float) "key" |>.toIO
    if (v - 1e10).abs < 1e5 then pure ()
    else throw (IO.userError s!"expected ~1e10, got {v}")
  | .error e => throw (IO.userError s!"{e}")

test "negative float" := do
  match Totem.parse "key = -0.5" with
  | .ok t =>
    let v ← t.getAs (α := Float) "key" |>.toIO
    if (v - (-0.5)).abs < 0.001 then pure ()
    else throw (IO.userError s!"expected ~-0.5, got {v}")
  | .error e => throw (IO.userError s!"{e}")

end Tests.Numbers

namespace Tests.Booleans

testSuite "Boolean Parsing"

test "true" := do
  match Totem.parse "key = true" with
  | .ok t =>
    let v ← t.getAs (α := Bool) "key" |>.toIO
    v ≡ true
  | .error e => throw (IO.userError s!"{e}")

test "false" := do
  match Totem.parse "key = false" with
  | .ok t =>
    let v ← t.getAs (α := Bool) "key" |>.toIO
    v ≡ false
  | .error e => throw (IO.userError s!"{e}")

end Tests.Booleans

namespace Tests.DateTime

testSuite "DateTime Parsing"

test "full datetime with Z" := do
  match Totem.parse "key = 1979-05-27T07:32:00Z" with
  | .ok t =>
    match t.get? "key" with
    | some (.dateTime dt) =>
      dt.date.year ≡ 1979
      dt.date.month ≡ 5
      dt.date.day ≡ 27
      dt.time.hour ≡ 7
      dt.time.minute ≡ 32
    | _ => throw (IO.userError "expected datetime")
  | .error e => throw (IO.userError s!"{e}")

test "local date" := do
  match Totem.parse "key = 2024-12-25" with
  | .ok t =>
    match t.get? "key" with
    | some (.localDate d) =>
      d.year ≡ 2024
      d.month ≡ 12
      d.day ≡ 25
    | _ => throw (IO.userError "expected local date")
  | .error e => throw (IO.userError s!"{e}")

end Tests.DateTime

namespace Tests.Arrays

testSuite "Array Parsing"

test "integer array" := do
  match Totem.parse "key = [1, 2, 3]" with
  | .ok t =>
    let v ← t.getAs (α := Array Int) "key" |>.toIO
    v ≡ #[1, 2, 3]
  | .error e => throw (IO.userError s!"{e}")

test "string array" := do
  match Totem.parse "key = [\"a\", \"b\", \"c\"]" with
  | .ok t =>
    let v ← t.getAs (α := Array String) "key" |>.toIO
    v ≡ #["a", "b", "c"]
  | .error e => throw (IO.userError s!"{e}")

test "nested array" := do
  match Totem.parse "key = [[1, 2], [3, 4]]" with
  | .ok t =>
    match t.get? "key" with
    | some (.array outer) =>
      outer.size ≡ 2
    | _ => throw (IO.userError "expected array")
  | .error e => throw (IO.userError s!"{e}")

test "multiline array" := do
  let input := "key = [\n  1,\n  2,\n  3\n]"
  match Totem.parse input with
  | .ok t =>
    let v ← t.getAs (α := Array Int) "key" |>.toIO
    v ≡ #[1, 2, 3]
  | .error e => throw (IO.userError s!"{e}")

end Tests.Arrays

namespace Tests.Tables

testSuite "Table Parsing"

test "simple table" := do
  let input := "[server]\nhost = \"localhost\"\nport = 8080"
  match Totem.parse input with
  | .ok t =>
    let host ← t.getAs (α := String) "server.host" |>.toIO
    let port ← t.getAs (α := Int) "server.port" |>.toIO
    host ≡ "localhost"
    port ≡ 8080
  | .error e => throw (IO.userError s!"{e}")

test "nested tables" := do
  let input := "[a.b.c]\nkey = \"value\""
  match Totem.parse input with
  | .ok t =>
    let v ← t.getAs (α := String) "a.b.c.key" |>.toIO
    v ≡ "value"
  | .error e => throw (IO.userError s!"{e}")

test "inline table" := do
  match Totem.parse "key = { name = \"test\", value = 42 }" with
  | .ok t =>
    match t.get? "key" with
    | some (.inlineTable inner) =>
      let name ← inner.getAs (α := String) "name" |>.toIO
      let value ← inner.getAs (α := Int) "value" |>.toIO
      name ≡ "test"
      value ≡ 42
    | _ => throw (IO.userError "expected inline table")
  | .error e => throw (IO.userError s!"{e}")

test "dotted keys" := do
  match Totem.parse "a.b.c = \"value\"" with
  | .ok t =>
    let v ← t.getAs (α := String) "a.b.c" |>.toIO
    v ≡ "value"
  | .error e => throw (IO.userError s!"{e}")

end Tests.Tables

namespace Tests.ArrayOfTables

testSuite "Array of Tables"

test "basic array of tables" := do
  let input := "[[products]]\nname = \"Hammer\"\n\n[[products]]\nname = \"Nail\""
  match Totem.parse input with
  | .ok t =>
    match t.get? "products" with
    | some (.array arr) =>
      arr.size ≡ 2
    | _ => throw (IO.userError "expected array of products")
  | .error e => throw (IO.userError s!"{e}")

end Tests.ArrayOfTables

namespace Tests.Extract

testSuite "Type Extraction"

test "extract int as nat" := do
  match Totem.parse "key = 42" with
  | .ok t =>
    let v ← t.getAs (α := Nat) "key" |>.toIO
    v ≡ 42
  | .error e => throw (IO.userError s!"{e}")

test "extract int as float" := do
  match Totem.parse "key = 42" with
  | .ok t =>
    let v ← t.getAs (α := Float) "key" |>.toIO
    if (v - 42.0).abs < 0.001 then pure ()
    else throw (IO.userError s!"expected ~42.0, got {v}")
  | .error e => throw (IO.userError s!"{e}")

test "extract with option" := do
  match Totem.parse "key = 42" with
  | .ok t =>
    let v ← t.getAsOption (α := Int) "key" |>.toIO
    v ≡ some 42
    let missing ← t.getAsOption (α := Int) "missing" |>.toIO
    missing ≡ none
  | .error e => throw (IO.userError s!"{e}")

test "key not found error" := do
  match Totem.parse "key = 42" with
  | .ok t =>
    match t.getAs (α := Int) "missing" with
    | .ok _ => throw (IO.userError "expected error")
    | .error (.keyNotFound _) => pure ()
    | .error e => throw (IO.userError s!"wrong error: {e}")
  | .error e => throw (IO.userError s!"{e}")

test "type conversion error" := do
  match Totem.parse "key = \"not a number\"" with
  | .ok t =>
    match t.getAs (α := Int) "key" with
    | .ok _ => throw (IO.userError "expected error")
    | .error (.typeConversion _ _ _) => pure ()
    | .error e => throw (IO.userError s!"wrong error: {e}")
  | .error e => throw (IO.userError s!"{e}")

end Tests.Extract

namespace Tests.Env

testSuite "Environment Interpolation"

test "simple interpolation" := do
  let env : String → Option String := fun s =>
    if s == "MY_VAR" then some "hello" else none
  match Totem.parseWithEnvResolver "key = \"${MY_VAR}\"" env with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "hello"
  | .error e => throw (IO.userError s!"{e}")

test "interpolation with default" := do
  let env : String → Option String := fun _ => none
  match Totem.parseWithEnvResolver "key = \"${MISSING:-default}\"" env with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "default"
  | .error e => throw (IO.userError s!"{e}")

test "mixed literal and interpolation" := do
  let env : String → Option String := fun s =>
    if s == "NAME" then some "World" else none
  match Totem.parseWithEnvResolver "key = \"Hello, ${NAME}!\"" env with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "Hello, World!"
  | .error e => throw (IO.userError s!"{e}")

test "missing env var without default errors" := do
  let env : String → Option String := fun _ => none
  match Totem.parseWithEnvResolver "key = \"${MISSING}\"" env with
  | .ok _ => throw (IO.userError "expected error")
  | .error _ => pure ()

end Tests.Env

namespace Tests.Comments

testSuite "Comments"

test "line comment" := do
  let input := "# This is a comment\nkey = \"value\""
  match Totem.parse input with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "value"
  | .error e => throw (IO.userError s!"{e}")

test "inline comment" := do
  let input := "key = \"value\" # inline comment"
  match Totem.parse input with
  | .ok t =>
    let v ← t.getAs (α := String) "key" |>.toIO
    v ≡ "value"
  | .error e => throw (IO.userError s!"{e}")

end Tests.Comments

def main : IO UInt32 := do
  IO.println "Totem - TOML Parser Tests"
  IO.println "========================="
  let result ← runAllSuites
  return if result != 0 then 1 else 0
