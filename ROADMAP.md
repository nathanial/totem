# Totem Roadmap

This document tracks potential improvements, new features, and code cleanup opportunities for the Totem TOML configuration parser.

## Feature Proposals

### [Priority: High] TOML Serialization (Write Support)

**Description:** Add the ability to serialize `Table` and `Value` types back to valid TOML strings. Currently, Totem can only parse TOML, not generate it.

**Rationale:** Many configuration use cases require round-trip support - reading a config, modifying it, and writing it back. This is essential for configuration editors, migration tools, and programmatic config generation.

**Affected Files:**
- New file: `Totem/Serialize.lean`
- `Totem/Core/Value.lean` (add `ToString` instances or serialization methods)
- `Totem.lean` (export serialization API)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: High] Schema Validation

**Description:** Add optional schema validation to verify that parsed TOML documents conform to expected structures - required keys, type constraints, value ranges, etc.

**Rationale:** Configuration errors are often caught at runtime deep in application code. Schema validation would catch these at parse time with clear error messages indicating which keys are missing or have wrong types.

**Affected Files:**
- New file: `Totem/Schema.lean`
- `Totem/Core/Error.lean` (add schema validation errors)
- `Totem.lean` (export validation API)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Derive Macro for FromConfig

**Description:** Add a `deriving FromConfig` capability to automatically generate `FromConfig` instances for user-defined structures, similar to `deriving Repr` or `deriving BEq`.

**Rationale:** Currently users must manually implement `FromConfig` for custom types. A derive macro would dramatically reduce boilerplate and improve developer experience.

**Affected Files:**
- New file: `Totem/Derive.lean`
- `Totem/Extract.lean` (potentially integrate macro support)

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] Compile-Time TOML Embedding

**Description:** Add a macro similar to `include_str%` (from staple) that parses and validates TOML at compile time, producing a typed configuration value.

**Rationale:** Would allow embedding configuration directly in code with compile-time validation, eliminating runtime parse errors for static configs.

**Affected Files:**
- New file: `Totem/Macros.lean`
- `Totem.lean` (export macro)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] JSON Conversion

**Description:** Add bidirectional conversion between TOML `Value`/`Table` types and JSON representations.

**Rationale:** Interoperability with JSON-based systems is common. This would enable migration paths, API integrations, and debugging workflows.

**Affected Files:**
- New file: `Totem/Json.lean`
- `Totem.lean` (export conversion functions)

**Estimated Effort:** Small

**Dependencies:** None (use Lean's built-in JSON support)

---

### [Priority: Medium] Extended Environment Variable Features

**Description:** Enhance environment variable interpolation with additional features:
- Nested interpolation: `${${VAR_NAME}}`
- Escape sequences: `\$` to include literal `$`
- Alternative operators: `${VAR:+alternative}` (use alternative if VAR is set)
- Error on undefined: `${VAR:?error message}`

**Rationale:** These patterns are common in shell scripting and 12-factor app configurations. Current implementation only supports `${VAR}` and `${VAR:-default}`.

**Affected Files:**
- `Totem/Env.lean`
- `Totem/Core/Error.lean` (add new error variants)

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Low] TOML Path Query Language

**Description:** Add a JSONPath-like query language for extracting values from TOML documents with wildcards and array indexing.

**Rationale:** Would simplify extracting values from complex nested structures without manual traversal code.

**Affected Files:**
- New file: `Totem/Query.lean`
- `Totem/Extract.lean` (integrate with existing extraction)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Merge Multiple TOML Files

**Description:** Add utilities to merge multiple TOML tables with configurable conflict resolution strategies (first wins, last wins, deep merge, error on conflict).

**Rationale:** Common pattern for layered configuration (defaults + environment-specific overrides).

**Affected Files:**
- `Totem/Core/Value.lean` (enhance `Table.merge`)
- Possibly new file: `Totem/Merge.lean`

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Low] Configuration Hot Reload Support

**Description:** Add file watching capability and callback infrastructure to detect and reload configuration changes at runtime.

**Rationale:** Long-running services often need to update configuration without restart.

**Affected Files:**
- New file: `Totem/Watch.lean`
- `Totem.lean` (export watching API)

**Estimated Effort:** Medium

**Dependencies:** None (would use FFI for file watching)

---

## Code Improvements

### [Priority: High] Better Date/Time Validation

**Current State:** Date validation in `Totem/Parser/DateTime.lean` (lines 26-29) only checks month 1-12 and day 1-31, without considering month-specific day limits or leap years.

**Proposed Change:** Implement proper calendar validation including:
- Month-specific day limits (28/29/30/31)
- Leap year detection for February
- Consistent validation approach

**Benefits:** Prevents accepting invalid dates like 2024-02-30 or 2023-02-29.

**Affected Files:**
- `Totem/Parser/DateTime.lean`
- Possibly `Totem/Core/Value.lean` (add validation helpers to LocalDate)

**Estimated Effort:** Small

---

### [Priority: High] Error Recovery and Multiple Errors

**Current State:** Parser stops at the first error encountered. Users must fix errors one at a time.

**Proposed Change:** Implement error recovery to continue parsing after errors and collect multiple diagnostics.

**Benefits:** Better developer experience, especially for large configuration files.

**Affected Files:**
- `Totem/Parser/State.lean` (add error collection)
- `Totem/Core/Error.lean` (add multi-error type)
- `Totem/Parser/Document.lean` (implement recovery logic)

**Estimated Effort:** Large

---

### [Priority: Medium] Reduce Code Duplication in Number Parsing

**Current State:** `Totem/Parser/Number.lean` has nearly identical code patterns for `parseHexInt`, `parseOctalInt`, and `parseBinaryInt` (lines 61-148).

**Proposed Change:** Refactor to a single generic function parameterized by digit validation and base:
```lean
def parseIntBase (base : Nat) (isValidDigit : Char → Bool) (digitValue : Char → Nat) : Parser Int
```

**Benefits:** Reduced code, easier maintenance, fewer places for bugs.

**Affected Files:**
- `Totem/Parser/Number.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] String Building Performance

**Current State:** String parsing uses repeated `String.push` in loops (e.g., `Totem/Parser/String.lean` lines 85-99), which may have O(n^2) performance for long strings due to immutable string concatenation.

**Proposed Change:** Use `Array Char` or `ByteArray` accumulator and convert to String once at the end.

**Benefits:** Better performance for large strings.

**Affected Files:**
- `Totem/Parser/String.lean`
- `Totem/Parser/Primitives.lean` (`readWhile` function)

**Estimated Effort:** Small

---

### [Priority: Medium] Unify peekN Pattern

**Current State:** `peekN` returns `Option String` and is used with pattern matching against `some "..."` throughout the codebase.

**Proposed Change:** Add a `peekString` or `expectLookahead` helper that directly checks for a specific string, returning Bool.

**Benefits:** Cleaner code at call sites, slightly more efficient.

**Affected Files:**
- `Totem/Parser/State.lean`
- Multiple parser files that use `peekN`

**Estimated Effort:** Small

---

### [Priority: Medium] Add BEq Instance for Table and Value

**Current State:** `Value` and `Table` only derive `Repr` and `Inhabited`, not `BEq`.

**Proposed Change:** Implement `BEq` instances for both types.

**Benefits:** Enables equality testing in tests and application code without manual comparison.

**Affected Files:**
- `Totem/Core/Value.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Optimize Table Lookups

**Current State:** `Table` uses linear search for key lookup (`entries.find?`), which is O(n) per lookup.

**Proposed Change:** For large tables, consider using `HashMap` or `RBMap` internally, or at least provide an `optimize` function that converts to a more efficient representation.

**Benefits:** Better performance for configs with many keys.

**Affected Files:**
- `Totem/Core/Value.lean`

**Estimated Effort:** Medium

**Dependencies:** Consider whether to add external dependencies (batteries)

---

### [Priority: Low] Parser Monad Transformer Stack

**Current State:** Parser is defined as `ExceptT ParseError (StateM ParserState)`.

**Proposed Change:** Consider using a custom monad with ReaderT for configuration options (e.g., strict mode, custom error messages).

**Benefits:** More extensible parser with configurable behavior.

**Affected Files:**
- `Totem/Parser/State.lean`
- All parser modules

**Estimated Effort:** Medium

---

## Code Cleanup

### [Priority: High] Add Docstrings to All Public Functions

**Issue:** Many public functions lack documentation. For example:
- `Totem/Parser/Primitives.lean`: `skipWs`, `skipNewline`, `readWhile` have minimal or no docs
- `Totem/Core/Value.lean`: Table methods like `insertPath`, `getPath?` could use examples

**Location:** Multiple files throughout the codebase

**Action Required:**
1. Add docstrings to all public functions
2. Include usage examples where helpful
3. Document edge cases and error conditions

**Estimated Effort:** Medium

---

### [Priority: High] Add Property-Based Tests

**Issue:** Test suite uses example-based tests only. No property-based testing for parser correctness guarantees.

**Location:** `Tests/Main.lean`

**Action Required:**
1. Add plausible dependency
2. Write generators for TOML values
3. Test round-trip (serialize-parse) invariants
4. Test parser edge cases with random inputs

**Estimated Effort:** Medium

---

### [Priority: Medium] Consistent Error Message Format

**Issue:** Error messages have inconsistent formatting. Some use colons, some use parentheses, capitalization varies.

**Location:** `Totem/Core/Error.lean` (lines 27-40)

**Action Required:** Standardize error message format across all error types for better user experience.

**Estimated Effort:** Small

---

### [Priority: Medium] Add Test Cases for Edge Cases

**Issue:** Some TOML edge cases are not tested:
- Unicode strings with surrogate pairs
- Very long strings
- Deeply nested tables
- Array of tables with nested regular tables
- Empty values (empty strings, empty arrays, empty tables)
- Maximum integer values
- Inf and NaN floats
- All datetime variants (offset datetime, local datetime, local date, local time)

**Location:** `Tests/Main.lean`

**Action Required:** Add comprehensive edge case tests for parser robustness.

**Estimated Effort:** Medium

---

### [Priority: Medium] Remove Partial Functions Where Possible

**Issue:** Several functions are marked `partial` when they could potentially be proven terminating:
- `Totem/Parser/String.lean`: `toHexString` (line 17)
- `Totem/Env.lean`: `interpolateString` and related functions

**Location:** Multiple files

**Action Required:** Analyze each partial function and either prove termination or document why it's necessary.

**Estimated Effort:** Small

---

### [Priority: Medium] Unused Variable Warning

**Issue:** In `Totem/Parser/Value.lean` line 127, `fullKey` is computed but not used (the check uses `keyParts` directly).

**Location:** `Totem/Parser/Value.lean:127`

**Action Required:** Remove unused variable or use it in the error message for better debugging.

**Estimated Effort:** Small

---

### [Priority: Low] Consolidate Private Helper Functions

**Issue:** Several private helper functions are duplicated or could be shared:
- `padLeft` in `Totem/Core/Value.lean` could be useful elsewhere
- `findCharIdx` in `Totem/Env.lean` is similar to standard library functions
- `hexChar` and `toHexString` in `Totem/Parser/String.lean`

**Location:** Multiple files

**Action Required:** Consider creating a `Totem/Internal/Util.lean` for shared private helpers.

**Estimated Effort:** Small

---

### [Priority: Low] Add README with Examples

**Issue:** No README.md file exists in the project root. The module docstring in `Totem.lean` provides basic usage, but a proper README would help discoverability.

**Location:** Project root

**Action Required:** Create README.md with:
- Installation instructions
- Quick start examples
- API overview
- Link to full documentation

**Estimated Effort:** Small

---

### [Priority: Low] Consider Adding Hashable Instance

**Issue:** `Value` and `Table` do not implement `Hashable`, limiting their use as keys in hash-based collections.

**Location:** `Totem/Core/Value.lean`

**Action Required:** Implement `Hashable` for both types.

**Estimated Effort:** Small

---

## Test Coverage Gaps

The following areas could benefit from additional test coverage:

1. **Parser error conditions** - Tests for specific error messages and positions
2. **TOML 1.0 spec conformance** - Test against official TOML test suite
3. **Environment interpolation edge cases** - Nested variables, escape sequences
4. **Very large documents** - Performance and memory usage
5. **Malformed input resilience** - Fuzzing for crash/hang detection

---

## Notes

- Effort estimates: Small (~1-2 hours), Medium (~1-2 days), Large (~1 week+)
- Priority based on: user value, frequency of use case, implementation complexity
- This roadmap should be updated as items are completed or new opportunities identified
