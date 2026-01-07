/-
  Totem.Parser.Number
  TOML number parsing (integers and floats) using Sift
-/
import Sift
import Totem.Parser.Primitives
import Totem.Core.Value

namespace Totem.Parser

open Sift Totem

/-- Positive infinity -/
private def posInf : Float := 1.0 / 0.0

/-- NaN (Not a Number) -/
private def nan : Float := 0.0 / 0.0

/-- Parse sign, returning multiplier -/
def parseSign : Sift.Parser Unit Int := do
  match ← peek with
  | some '+' => let _ ← anyChar; return 1
  | some '-' => let _ ← anyChar; return -1
  | _ => return 1

/-- Parse decimal integer (with underscores) -/
partial def parseDecimalInt (leadDigit : Option Char := none) : Sift.Parser Unit Int := do
  let mut result : Nat := 0
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  -- Handle optional leading digit passed in
  if let some d := leadDigit then
    result := d.toNat - '0'.toNat
    hasDigit := true

  while going do
    match ← peek with
    | some c =>
      if isDigit c then
        let _ ← anyChar
        result := result * 10 + (c.toNat - '0'.toNat)
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← anyChar
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    Parser.fail "expected digit"
  if lastWasUnderscore then
    Parser.fail "number cannot end with underscore"

  return Int.ofNat result

/-- Parse hexadecimal integer -/
partial def parseHexInt : Sift.Parser Unit Int := do
  let mut result : Nat := 0
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  while going do
    match ← peek with
    | some c =>
      if isHexDigit c then
        let _ ← anyChar
        result := result * 16 + hexDigitValue c
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← anyChar
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    Parser.fail "expected hex digit"
  if lastWasUnderscore then
    Parser.fail "number cannot end with underscore"

  return Int.ofNat result

/-- Parse octal integer -/
partial def parseOctalInt : Sift.Parser Unit Int := do
  let mut result : Nat := 0
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  while going do
    match ← peek with
    | some c =>
      if isOctalDigit c then
        let _ ← anyChar
        result := result * 8 + (c.toNat - '0'.toNat)
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← anyChar
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    Parser.fail "expected octal digit"
  if lastWasUnderscore then
    Parser.fail "number cannot end with underscore"

  return Int.ofNat result

/-- Parse binary integer -/
partial def parseBinaryInt : Sift.Parser Unit Int := do
  let mut result : Nat := 0
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  while going do
    match ← peek with
    | some c =>
      if isBinaryDigit c then
        let _ ← anyChar
        result := result * 2 + (c.toNat - '0'.toNat)
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← anyChar
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    Parser.fail "expected binary digit"
  if lastWasUnderscore then
    Parser.fail "number cannot end with underscore"

  return Int.ofNat result

/-- Parse TOML integer -/
def parseInteger : Sift.Parser Unit Int := do
  let sign ← parseSign

  -- Check for prefix
  if (← peek) == some '0' then
    let _ ← anyChar
    match ← peek with
    | some 'x' => let _ ← anyChar; return sign * (← parseHexInt)
    | some 'o' => let _ ← anyChar; return sign * (← parseOctalInt)
    | some 'b' => let _ ← anyChar; return sign * (← parseBinaryInt)
    | some c =>
      if isDigit c then
        -- Leading zeros not allowed for decimal
        Parser.fail "leading zeros not allowed"
      else
        -- Just the number 0
        return 0
    | none => return 0
  else
    return sign * (← parseDecimalInt)

/-- Parse fractional part of float (after decimal point) -/
partial def parseFraction : Sift.Parser Unit Float := do
  let mut result : Float := 0
  let mut divisor : Float := 10
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  while going do
    match ← peek with
    | some c =>
      if isDigit c then
        let _ ← anyChar
        result := result + (c.toNat - '0'.toNat).toFloat / divisor
        divisor := divisor * 10
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← anyChar
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    Parser.fail "expected digit after decimal point"
  if lastWasUnderscore then
    Parser.fail "number cannot end with underscore"

  return result

/-- Parse exponent part of float -/
def parseExponent : Sift.Parser Unit Int := do
  let sign ← parseSign
  let value ← parseDecimalInt
  return sign * value

/-- Convert Int to Float -/
private def intToFloat (i : Int) : Float :=
  if i >= 0 then i.toNat.toFloat
  else -((-i).toNat.toFloat)

/-- Parse TOML float -/
partial def parseFloat : Sift.Parser Unit Float := do
  let sign ← parseSign
  let signFloat : Float := if sign < 0 then -1.0 else 1.0

  -- Check for special values
  match ← peekString 3 with
  | some "inf" =>
    let _ ← string "inf"
    return signFloat * posInf
  | some "nan" =>
    let _ ← string "nan"
    return nan
  | _ => pure ()

  -- Parse integer part
  let intPart ← parseDecimalInt

  -- Check for fraction or exponent
  let mut result := intToFloat intPart
  let mut isFloat := false

  if (← peek) == some '.' then
    let _ ← anyChar
    let frac ← parseFraction
    result := result + frac
    isFloat := true

  if (← peek).any (fun c => c == 'e' || c == 'E') then
    let _ ← anyChar
    let exp ← parseExponent
    result := result * Float.pow 10.0 (intToFloat exp)
    isFloat := true

  if !isFloat then
    Parser.fail "expected float (needs decimal point or exponent)"

  return signFloat * result

/-- Check if this looks like a float (has . or e/E after digits) -/
def looksLikeFloat : Sift.Parser Unit Bool := do
  lookAhead do
    -- Skip optional sign
    if (← peek).any (fun c => c == '+' || c == '-') then
      let _ ← anyChar
    -- Check for inf/nan
    match ← peekString 3 with
    | some "inf" | some "nan" => return true
    | _ => pure ()
    -- Skip digits
    let mut going := true
    while going do
      if (← peek).any (fun c => isDigit c || c == '_') then
        let _ ← anyChar
      else
        going := false
    -- Check next char
    return (← peek).any (fun c => c == '.' || c == 'e' || c == 'E')

/-- Parse number (integer or float) -/
def parseNumber : Sift.Parser Unit Value := do
  if ← looksLikeFloat then
    return .float (← parseFloat)
  else
    return .integer (← parseInteger)

end Totem.Parser
