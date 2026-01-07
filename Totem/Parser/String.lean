/-
  Totem.Parser.String
  TOML string parsing (basic, literal, multi-line) using Sift
-/
import Sift
import Totem.Parser.Primitives

namespace Totem.Parser

open Sift

/-- Get hex digit character -/
private def hexChar (n : Nat) : Char :=
  if n < 10 then Char.ofNat ('0'.toNat + n)
  else Char.ofNat ('A'.toNat + n - 10)

/-- Convert a Nat to hex string -/
private partial def toHexString (n : Nat) : String :=
  if n == 0 then "0"
  else go n []
where
  go (n : Nat) (acc : List Char) : String :=
    if n == 0 then String.mk acc
    else go (n / 16) (hexChar (n % 16) :: acc)

/-- Parse a 4-digit unicode escape (\uXXXX) -/
def parseUnicodeEscape4 : Sift.Parser Unit Char := do
  let code ← hexDigitsN 4
  if h : code.toUInt32 < 0x110000 then
    pure (Char.ofNat code)
  else
    Parser.fail s!"invalid unicode code point: {code}"

/-- Parse an 8-digit unicode escape (\UXXXXXXXX) -/
def parseUnicodeEscape8 : Sift.Parser Unit Char := do
  let code ← hexDigitsN 8
  if h : code.toUInt32 < 0x110000 then
    pure (Char.ofNat code)
  else
    Parser.fail s!"invalid unicode code point: {code}"

/-- Parse escape sequence in basic string -/
def parseEscape : Sift.Parser Unit Char := do
  let escaped ← anyChar
  match escaped with
  | 'b' => return '\x08'  -- backspace
  | 't' => return '\t'    -- tab
  | 'n' => return '\n'    -- newline
  | 'f' => return '\x0C'  -- form feed
  | 'r' => return '\r'    -- carriage return
  | '"' => return '"'
  | '\\' => return '\\'
  | 'u' => parseUnicodeEscape4
  | 'U' => parseUnicodeEscape8
  | c => Parser.fail s!"invalid escape sequence: \\{c}"

/-- Parse basic string (double-quoted with escapes) -/
partial def parseBasicString : Sift.Parser Unit String := do
  let _ ← char '"'
  let mut result := ""
  while (← peek) != some '"' do
    if ← atEnd then
      Parser.fail "unclosed string"
    let c ← anyChar
    if c == '\\' then
      let unescaped ← parseEscape
      result := result.push unescaped
    else if c == '\n' then
      Parser.fail "newline not allowed in basic string"
    else if c.toNat < 0x20 && c != '\t' then
      Parser.fail s!"control character not allowed: U+{toHexString c.toNat}"
    else
      result := result.push c
  let _ ← char '"'
  return result

/-- Parse multi-line basic string (triple double-quoted) -/
partial def parseMultiLineBasicString : Sift.Parser Unit String := do
  let _ ← string "\"\"\""
  -- Skip immediate newline after opening quotes
  if (← peek) == some '\n' then
    let _ ← anyChar
  else if (← peek) == some '\r' then
    let _ ← anyChar
    if (← peek) == some '\n' then
      let _ ← anyChar
  let mut result := ""
  while true do
    if ← atEnd then
      Parser.fail "unclosed multi-line string"
    -- Check for closing quotes
    if (← peekString 3) == some "\"\"\"" then
      let _ ← string "\"\"\""
      break
    let c ← anyChar
    if c == '\\' then
      -- Check for line-ending backslash
      match ← peek with
      | some '\n' =>
        let _ ← anyChar
        -- Skip whitespace on next line
        while (← peek).any (fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r') do
          let _ ← anyChar
      | some '\r' =>
        let _ ← anyChar
        if (← peek) == some '\n' then
          let _ ← anyChar
        while (← peek).any (fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r') do
          let _ ← anyChar
      | _ =>
        let unescaped ← parseEscape
        result := result.push unescaped
    else if c.toNat < 0x20 && c != '\t' && c != '\n' && c != '\r' then
      Parser.fail s!"control character not allowed: U+{toHexString c.toNat}"
    else
      result := result.push c
  return result

/-- Parse literal string (single-quoted, no escapes) -/
partial def parseLiteralString : Sift.Parser Unit String := do
  let _ ← char '\''
  let mut result := ""
  while (← peek) != some '\'' do
    if ← atEnd then
      Parser.fail "unclosed literal string"
    let c ← anyChar
    if c == '\n' then
      Parser.fail "newline not allowed in literal string"
    else if c.toNat < 0x20 && c != '\t' then
      Parser.fail s!"control character not allowed: U+{toHexString c.toNat}"
    else
      result := result.push c
  let _ ← char '\''
  return result

/-- Parse multi-line literal string (triple single-quoted) -/
partial def parseMultiLineLiteralString : Sift.Parser Unit String := do
  let _ ← string "'''"
  -- Skip immediate newline after opening quotes
  if (← peek) == some '\n' then
    let _ ← anyChar
  else if (← peek) == some '\r' then
    let _ ← anyChar
    if (← peek) == some '\n' then
      let _ ← anyChar
  let mut result := ""
  while true do
    if ← atEnd then
      Parser.fail "unclosed multi-line literal string"
    -- Check for closing quotes
    if (← peekString 3) == some "'''" then
      let _ ← string "'''"
      break
    let c ← anyChar
    if c.toNat < 0x20 && c != '\t' && c != '\n' && c != '\r' then
      Parser.fail s!"control character not allowed: U+{toHexString c.toNat}"
    else
      result := result.push c
  return result

/-- Parse any TOML string (basic, literal, or multi-line variants) -/
def parseString : Sift.Parser Unit String := do
  -- Check for triple quotes first (order matters)
  if (← peekString 3) == some "\"\"\"" then
    parseMultiLineBasicString
  else if (← peekString 3) == some "'''" then
    parseMultiLineLiteralString
  else if (← peek) == some '"' then
    parseBasicString
  else if (← peek) == some '\'' then
    parseLiteralString
  else
    Parser.fail "expected string"

end Totem.Parser
