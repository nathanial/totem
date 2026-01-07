/-
  Totem.Parser.Primitives
  Low-level parsing helpers using Sift
-/
import Sift

namespace Totem.Parser

open Sift

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

/-- Convert hex digit to value -/
def hexDigitValue (c : Char) : Nat :=
  if c >= '0' && c <= '9' then c.toNat - '0'.toNat
  else if c >= 'a' && c <= 'f' then c.toNat - 'a'.toNat + 10
  else if c >= 'A' && c <= 'F' then c.toNat - 'A'.toNat + 10
  else 0

/-- Skip whitespace (space and tab only) -/
def skipWs : Sift.Parser Unit Unit := hspaces

/-- Skip a single newline (LF or CRLF) -/
def skipNewline : Sift.Parser Unit Bool := do
  match ← peek with
  | some '\n' =>
    let _ ← anyChar
    return true
  | some '\r' =>
    let _ ← anyChar
    if (← peek) == some '\n' then
      let _ ← anyChar
    return true
  | _ => return false

/-- Skip comment (# to end of line) -/
def skipComment : Sift.Parser Unit Unit := do
  if (← peek) == some '#' then
    let _ ← anyChar
    skipWhile (fun c => !isNewline c)

/-- Skip whitespace and comments on same line -/
def skipLineTrivia : Sift.Parser Unit Unit := do
  skipWs
  skipComment

/-- Skip whitespace, comments, and newlines -/
partial def skipTrivia : Sift.Parser Unit Unit := do
  skipWs
  match ← peek with
  | some '#' =>
    skipComment
    let _ ← skipNewline
    skipTrivia
  | some c =>
    if isNewline c then
      let _ ← skipNewline
      skipTrivia
  | none => pure ()

/-- Parse digits with underscore separators (re-exported from Sift) -/
def digitsWithUnderscores (isValidDigit : Char → Bool) : Sift.Parser Unit String :=
  Sift.digitsWithUnderscores isValidDigit

/-- Parse exactly N decimal digits -/
def readExactDigits (n : Nat) : Sift.Parser Unit String := do
  let digits ← count n digit
  pure (String.mk digits.toList)

/-- TOML newline parser (LF or CRLF) -/
def tomlNewline : Sift.Parser Unit Unit :=
  (char '\r' *> char '\n' *> pure ()) <|> (char '\n' *> pure ())

end Totem.Parser
