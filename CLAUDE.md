# CLAUDE.md

TOML configuration parser for Lean 4 with typed extraction and environment variable interpolation.

## Build

```bash
lake build
lake test
```

## Architecture

```
Totem.lean           # Main API: parse, parseWithEnv, loadFile, loadFileWithEnv
Totem/
  Core.lean          # Re-exports core types
  Core/
    Value.lean       # Value type (string, int, float, bool, datetime, array, table)
    Position.lean    # Source position tracking
    Error.lean       # ParseError, ExtractError types
  Parser.lean        # Main parser entry point
  Parser/
    Primitives.lean  # Basic parsing utilities
    Key.lean         # Key parsing (bare, quoted, dotted)
    String.lean      # String literals (basic, literal, multiline)
    Number.lean      # Int/Float with hex, octal, binary, scientific
    DateTime.lean    # RFC 3339 datetime, local date/time
    Value.lean       # Value parsing
    Document.lean    # Tables, arrays of tables, full document
  Extract.lean       # FromConfig typeclass for typed extraction
  Env.lean           # Environment variable interpolation (${VAR}, ${VAR:-default})
```

## Key Types

- `Table` - Ordered key-value pairs (preserves insertion order)
- `Value` - Tagged union: string, int, float, bool, datetime, array, inlineTable
- `FromConfig α` - Typeclass for extracting typed values (String, Int, Nat, Float, Bool, Option, Array)

## Dependencies

- `sift` - Parser combinator library
- `crucible` - Test framework
