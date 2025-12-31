# Totem

A TOML configuration parser library for Lean 4 with typed extraction and environment variable interpolation.

## Features

- **Full TOML 1.0 support** - Strings, integers, floats, booleans, datetime, arrays, tables
- **Position-aware errors** - Line and column numbers for debugging
- **Typed extraction** - `FromConfig` typeclass for String, Int, Nat, Float, Bool, Option, Array
- **Environment interpolation** - `${VAR}` and `${VAR:-default}` syntax
- **Dotted key access** - Navigate nested tables with `"a.b.c"` paths

## Installation

Add to your `lakefile.lean`:

```lean
require totem from git "https://github.com/nathanial/totem" @ "v0.0.1"
```

## Quick Start

```lean
import Totem

def main : IO Unit := do
  let toml := "
[database]
host = \"localhost\"
port = 5432
enabled = true
"

  match Totem.parse toml with
  | .ok table =>
    let host ← table.getAs (α := String) "database.host" |>.toIO
    let port ← table.getAs (α := Int) "database.port" |>.toIO
    let enabled ← table.getAs (α := Bool) "database.enabled" |>.toIO
    IO.println s!"Connecting to {host}:{port} (enabled: {enabled})"
  | .error e =>
    IO.eprintln s!"Parse error: {e}"
```

Output:
```
Connecting to localhost:5432 (enabled: true)
```

## Environment Variable Interpolation

```lean
import Totem

def main : IO Unit := do
  let toml := "
api_key = \"${API_KEY}\"
timeout = \"${TIMEOUT:-30}\"
"

  match ← Totem.parseWithEnv toml with
  | .ok table =>
    let key ← table.getAs (α := String) "api_key" |>.toIO
    let timeout ← table.getAs (α := String) "timeout" |>.toIO
    IO.println s!"Key: {key}, Timeout: {timeout}"
  | .error e =>
    IO.eprintln s!"Error: {e}"
```

## Supported TOML Types

### Strings

```toml
# Basic strings (with escapes)
str1 = "Hello\nWorld"

# Literal strings (no escapes)
str2 = 'C:\path\to\file'

# Multi-line basic
str3 = """
Line 1
Line 2"""

# Multi-line literal
str4 = '''
No \escapes\ here
'''
```

### Numbers

```toml
# Integers
int1 = 42
int2 = -17
int3 = 1_000_000

# Different bases
hex = 0xDEADBEEF
oct = 0o755
bin = 0b11010110

# Floats
flt1 = 3.14
flt2 = -0.01
flt3 = 1e10
flt4 = 6.022e23

# Special floats
inf1 = inf
inf2 = -inf
nan = nan
```

### DateTime

```toml
# Full datetime with timezone
dt1 = 1979-05-27T07:32:00Z
dt2 = 1979-05-27T00:32:00-07:00

# Local datetime (no timezone)
dt3 = 1979-05-27T07:32:00

# Local date
date = 1979-05-27

# Local time
time = 07:32:00
```

### Arrays

```toml
integers = [1, 2, 3]
strings = ["a", "b", "c"]
nested = [[1, 2], [3, 4]]

# Multi-line
hosts = [
  "alpha",
  "beta",
  "gamma",
]
```

### Tables

```toml
# Standard table
[server]
host = "localhost"
port = 8080

# Nested tables
[server.database]
name = "mydb"

# Inline tables
point = { x = 1, y = 2 }

# Dotted keys
fruit.apple.color = "red"
```

### Array of Tables

```toml
[[products]]
name = "Hammer"
price = 9.99

[[products]]
name = "Nail"
price = 0.05
```

## API Reference

### Parsing

```lean
-- Parse TOML string
def parse (input : String) : ParseResult Table

-- Parse with environment variable interpolation
def parseWithEnv (input : String) : IO (Except String Table)

-- Parse with custom environment resolver
def parseWithEnvResolver (input : String) (getEnv : String → Option String) : ParseResult Table

-- Load from file
def loadFile (path : System.FilePath) : IO (Except String Table)

-- Load from file with env interpolation
def loadFileWithEnv (path : System.FilePath) : IO (Except String Table)
```

### Table Access

```lean
-- Get value by key
table.get? (key : String) : Option Value

-- Get value by dotted path
table.getPath? (path : List String) : Option Value

-- Check if key exists
table.contains (key : String) : Bool

-- Get all keys
table.keys : Array String

-- Get table size
table.size : Nat
```

### Typed Extraction

```lean
-- Extract with type conversion
table.getAs [FromConfig α] (path : String) : ExtractResult α

-- Extract with Option for missing keys
table.getAsOption [FromConfig α] (path : String) : ExtractResult (Option α)
```

### Supported Types

The `FromConfig` typeclass has instances for:
- `String` - String values
- `Int` - Integer values
- `Nat` - Non-negative integers
- `Float` - Float values (also converts from Int)
- `Bool` - Boolean values
- `Option α` - Missing keys become `none`
- `Array α` - Array values with element conversion
- `Table` - Inline table values
- `Value` - Raw value passthrough

## Error Handling

### Parse Errors

```lean
inductive ParseError where
  | unexpectedChar (pos : Position) (char : Char) (expected : String)
  | unexpectedEnd (context : String)
  | invalidKey (pos : Position) (msg : String)
  | invalidString (pos : Position) (msg : String)
  | invalidNumber (pos : Position) (msg : String)
  | invalidDateTime (pos : Position) (msg : String)
  | invalidValue (pos : Position) (msg : String)
  | duplicateKey (pos : Position) (key : String)
  | invalidTablePath (pos : Position) (path : String)
  | mixedArrayTypes (pos : Position)
  | invalidInlineTable (pos : Position) (msg : String)
```

### Extract Errors

```lean
inductive ExtractError where
  | typeConversion (path : String) (expected : String) (actual : String)
  | keyNotFound (path : String)
  | indexOutOfBounds (path : String) (index : Nat) (size : Nat)
  | envVarNotFound (varName : String)
  | envVarInterpolationFailed (path : String) (msg : String)
```

### Converting to IO

```lean
-- Convert ParseResult to IO
let table ← Totem.parse toml |> ParseResult.toIO

-- Convert ExtractResult to IO
let port ← table.getAs (α := Int) "server.port" |>.toIO
```

## Building

```bash
# Build the library
lake build

# Run tests
lake test
```

## License

MIT
