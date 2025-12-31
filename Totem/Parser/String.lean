/-
  Totem.Parser.String
  TOML string parsing (basic, literal, multi-line)
-/
import Totem.Parser.Primitives

namespace Totem.Parser

open Totem Parser

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
def parseUnicodeEscape4 : Parser Char := do
  let pos ← getPosition
  let mut codePoint : Nat := 0
  let mut count := 0
  while count < 4 do
    match ← peek? with
    | some c =>
      if isHexDigit c then
        let _ ← next
        codePoint := codePoint * 16 + hexDigitValue c
        count := count + 1
      else
        throw (.invalidString pos s!"invalid unicode escape: expected hex digit, got '{c}'")
    | none => throw (.invalidString pos "incomplete unicode escape")
  if h : codePoint < 0x110000 then
    return Char.ofNat codePoint
  else
    throw (.invalidString pos s!"invalid unicode code point: {codePoint}")

/-- Parse an 8-digit unicode escape (\UXXXXXXXX) -/
def parseUnicodeEscape8 : Parser Char := do
  let pos ← getPosition
  let mut codePoint : Nat := 0
  let mut count := 0
  while count < 8 do
    match ← peek? with
    | some c =>
      if isHexDigit c then
        let _ ← next
        codePoint := codePoint * 16 + hexDigitValue c
        count := count + 1
      else
        throw (.invalidString pos s!"invalid unicode escape: expected hex digit, got '{c}'")
    | none => throw (.invalidString pos "incomplete unicode escape")
  if h : codePoint < 0x110000 then
    return Char.ofNat codePoint
  else
    throw (.invalidString pos s!"invalid unicode code point: {codePoint}")

/-- Parse escape sequence in basic string -/
def parseEscape : Parser Char := do
  let pos ← getPosition
  let escaped ← next
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
  | c => throw (.invalidString pos s!"invalid escape sequence: \\{c}")

/-- Parse basic string (double-quoted with escapes) -/
def parseBasicString : Parser String := do
  let pos ← getPosition
  expect '"'
  let mut result := ""
  while (← peek?) != some '"' do
    if ← atEnd then
      throw (.invalidString pos "unclosed string")
    let c ← next
    if c == '\\' then
      let unescaped ← parseEscape
      result := result.push unescaped
    else if c == '\n' then
      throw (.invalidString pos "newline not allowed in basic string")
    else if c.toNat < 0x20 && c != '\t' then
      throw (.invalidString pos s!"control character not allowed: U+{toHexString c.toNat}")
    else
      result := result.push c
  expect '"'
  return result

/-- Parse multi-line basic string (triple double-quoted) -/
def parseMultiLineBasicString : Parser String := do
  let pos ← getPosition
  expectString "\"\"\""
  -- Skip immediate newline after opening quotes
  if (← peek?) == some '\n' then
    let _ ← next
  else if (← peek?) == some '\r' then
    let _ ← next
    if (← peek?) == some '\n' then
      let _ ← next
  let mut result := ""
  while true do
    if ← atEnd then
      throw (.invalidString pos "unclosed multi-line string")
    -- Check for closing quotes
    if (← peekN 3) == some "\"\"\"" then
      expectString "\"\"\""
      break
    let c ← next
    if c == '\\' then
      -- Check for line-ending backslash
      match ← peek? with
      | some '\n' =>
        let _ ← next
        -- Skip whitespace on next line
        while (← peek?).any (fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r') do
          let _ ← next
      | some '\r' =>
        let _ ← next
        if (← peek?) == some '\n' then
          let _ ← next
        while (← peek?).any (fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r') do
          let _ ← next
      | _ =>
        let unescaped ← parseEscape
        result := result.push unescaped
    else if c.toNat < 0x20 && c != '\t' && c != '\n' && c != '\r' then
      throw (.invalidString pos s!"control character not allowed: U+{toHexString c.toNat}")
    else
      result := result.push c
  return result

/-- Parse literal string (single-quoted, no escapes) -/
def parseLiteralString : Parser String := do
  let pos ← getPosition
  expect '\''
  let mut result := ""
  while (← peek?) != some '\'' do
    if ← atEnd then
      throw (.invalidString pos "unclosed literal string")
    let c ← next
    if c == '\n' then
      throw (.invalidString pos "newline not allowed in literal string")
    else if c.toNat < 0x20 && c != '\t' then
      throw (.invalidString pos s!"control character not allowed: U+{toHexString c.toNat}")
    else
      result := result.push c
  expect '\''
  return result

/-- Parse multi-line literal string (triple single-quoted) -/
def parseMultiLineLiteralString : Parser String := do
  let pos ← getPosition
  expectString "'''"
  -- Skip immediate newline after opening quotes
  if (← peek?) == some '\n' then
    let _ ← next
  else if (← peek?) == some '\r' then
    let _ ← next
    if (← peek?) == some '\n' then
      let _ ← next
  let mut result := ""
  while true do
    if ← atEnd then
      throw (.invalidString pos "unclosed multi-line literal string")
    -- Check for closing quotes
    if (← peekN 3) == some "'''" then
      expectString "'''"
      break
    let c ← next
    if c.toNat < 0x20 && c != '\t' && c != '\n' && c != '\r' then
      throw (.invalidString pos s!"control character not allowed: U+{toHexString c.toNat}")
    else
      result := result.push c
  return result

/-- Parse any TOML string (basic, literal, or multi-line variants) -/
def parseString : Parser String := do
  -- Check for triple quotes first (order matters)
  if (← peekN 3) == some "\"\"\"" then
    parseMultiLineBasicString
  else if (← peekN 3) == some "'''" then
    parseMultiLineLiteralString
  else if (← peek?) == some '"' then
    parseBasicString
  else if (← peek?) == some '\'' then
    parseLiteralString
  else
    let pos ← getPosition
    throw (.invalidString pos "expected string")

end Totem.Parser
