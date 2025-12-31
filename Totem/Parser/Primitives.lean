/-
  Totem.Parser.Primitives
  Low-level parsing helpers
-/
import Totem.Parser.State

namespace Totem.Parser

open Totem Parser

/-- Check if character is whitespace (space or tab only, not newline) -/
def isWs (c : Char) : Bool := c == ' ' || c == '\t'

/-- Check if character is a newline -/
def isNewline (c : Char) : Bool := c == '\n' || c == '\r'

/-- Check if character is valid in a bare key (A-Za-z0-9_-) -/
def isBareKeyChar (c : Char) : Bool :=
  c.isAlphanum || c == '-' || c == '_'

/-- Check if character is a decimal digit -/
def isDigit (c : Char) : Bool := c >= '0' && c <= '9'

/-- Check if character is a hex digit -/
def isHexDigit (c : Char) : Bool :=
  isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

/-- Check if character is an octal digit -/
def isOctalDigit (c : Char) : Bool := c >= '0' && c <= '7'

/-- Check if character is a binary digit -/
def isBinaryDigit (c : Char) : Bool := c == '0' || c == '1'

-- TODO: Replace with Staple.Hex.hexCharToNat after staple release
/-- Convert hex digit to value -/
def hexDigitValue (c : Char) : Nat :=
  if c >= '0' && c <= '9' then c.toNat - '0'.toNat
  else if c >= 'a' && c <= 'f' then c.toNat - 'a'.toNat + 10
  else if c >= 'A' && c <= 'F' then c.toNat - 'A'.toNat + 10
  else 0

/-- Skip whitespace (space and tab only) -/
def skipWs : Parser Unit := do
  while (← peek?).any isWs do
    let _ ← next

/-- Skip a single newline (LF or CRLF) -/
def skipNewline : Parser Bool := do
  match ← peek? with
  | some '\n' =>
    let _ ← next
    return true
  | some '\r' =>
    let _ ← next
    if (← peek?) == some '\n' then
      let _ ← next
    return true
  | _ => return false

/-- Skip comment (# to end of line) -/
def skipComment : Parser Unit := do
  if (← peek?) == some '#' then
    let _ ← next
    let mut done := false
    while !done do
      match ← peek? with
      | some c =>
        if isNewline c then
          done := true
        else
          let _ ← next
      | none => done := true

/-- Skip whitespace and comments on same line -/
def skipLineTrivia : Parser Unit := do
  skipWs
  skipComment

/-- Skip whitespace, comments, and newlines -/
def skipTrivia : Parser Unit := do
  let mut done := false
  while !done do
    skipWs
    if (← peek?) == some '#' then
      skipComment
    else if (← peek?).any isNewline then
      let _ ← skipNewline
    else
      done := true

/-- Read characters while predicate holds -/
def readWhile (pred : Char → Bool) : Parser String := do
  let mut result := ""
  let mut going := true
  while going do
    match ← peek? with
    | some c =>
      if pred c then
        let _ ← next
        result := result.push c
      else
        going := false
    | none => going := false
  return result

/-- Read at least one character while predicate holds -/
def readWhile1 (pred : Char → Bool) (context : String) : Parser String := do
  let pos ← getPosition
  let result ← readWhile pred
  if result.isEmpty then
    match ← peek? with
    | some c => throw (.unexpectedChar pos c context)
    | none => throw (.unexpectedEnd context)
  return result

/-- Parse a sequence of digits, allowing underscores between digits -/
def readDigits (isValidDigit : Char → Bool) : Parser String := do
  let mut result := ""
  let mut lastWasUnderscore := false
  let mut first := true
  let mut going := true
  while going do
    match ← peek? with
    | some c =>
      if isValidDigit c then
        let _ ← next
        result := result.push c
        lastWasUnderscore := false
        first := false
      else if c == '_' && !first && !lastWasUnderscore then
        let _ ← next
        lastWasUnderscore := true
      else
        going := false
    | none => going := false
  return result

/-- Parse exactly N digits -/
def readExactDigits (n : Nat) : Parser String := do
  let pos ← getPosition
  let mut result := ""
  let mut count := 0
  while count < n do
    match ← peek? with
    | some c =>
      if isDigit c then
        let _ ← next
        result := result.push c
        count := count + 1
      else
        throw (.unexpectedChar pos c s!"{n} digits")
    | none => throw (.unexpectedEnd s!"{n} digits")
  return result

end Totem.Parser
