/-
  Totem.Parser.Number
  TOML number parsing (integers and floats)
-/
import Totem.Parser.Primitives
import Totem.Core.Value

namespace Totem.Parser

open Totem Parser

/-- Positive infinity -/
private def posInf : Float := 1.0 / 0.0

/-- NaN (Not a Number) -/
private def nan : Float := 0.0 / 0.0

/-- Parse sign, returning multiplier -/
def parseSign : Parser Int := do
  match ← peek? with
  | some '+' => let _ ← next; return 1
  | some '-' => let _ ← next; return -1
  | _ => return 1

/-- Parse decimal integer (with underscores) -/
def parseDecimalInt (leadDigit : Option Char := none) : Parser Int := do
  let pos ← getPosition
  let mut result : Nat := 0
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  -- Handle optional leading digit passed in
  if let some d := leadDigit then
    result := d.toNat - '0'.toNat
    hasDigit := true

  while going do
    match ← peek? with
    | some c =>
      if isDigit c then
        let _ ← next
        result := result * 10 + (c.toNat - '0'.toNat)
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← next
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    throw (.invalidNumber pos "expected digit")
  if lastWasUnderscore then
    throw (.invalidNumber pos "number cannot end with underscore")

  return Int.ofNat result

/-- Parse hexadecimal integer -/
def parseHexInt : Parser Int := do
  let pos ← getPosition
  let mut result : Nat := 0
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  while going do
    match ← peek? with
    | some c =>
      if isHexDigit c then
        let _ ← next
        result := result * 16 + hexDigitValue c
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← next
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    throw (.invalidNumber pos "expected hex digit")
  if lastWasUnderscore then
    throw (.invalidNumber pos "number cannot end with underscore")

  return Int.ofNat result

/-- Parse octal integer -/
def parseOctalInt : Parser Int := do
  let pos ← getPosition
  let mut result : Nat := 0
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  while going do
    match ← peek? with
    | some c =>
      if isOctalDigit c then
        let _ ← next
        result := result * 8 + (c.toNat - '0'.toNat)
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← next
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    throw (.invalidNumber pos "expected octal digit")
  if lastWasUnderscore then
    throw (.invalidNumber pos "number cannot end with underscore")

  return Int.ofNat result

/-- Parse binary integer -/
def parseBinaryInt : Parser Int := do
  let pos ← getPosition
  let mut result : Nat := 0
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  while going do
    match ← peek? with
    | some c =>
      if isBinaryDigit c then
        let _ ← next
        result := result * 2 + (c.toNat - '0'.toNat)
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← next
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    throw (.invalidNumber pos "expected binary digit")
  if lastWasUnderscore then
    throw (.invalidNumber pos "number cannot end with underscore")

  return Int.ofNat result

/-- Parse TOML integer -/
def parseInteger : Parser Int := do
  let pos ← getPosition
  let sign ← parseSign

  -- Check for prefix
  if (← peek?) == some '0' then
    let _ ← next
    match ← peek? with
    | some 'x' => let _ ← next; return sign * (← parseHexInt)
    | some 'o' => let _ ← next; return sign * (← parseOctalInt)
    | some 'b' => let _ ← next; return sign * (← parseBinaryInt)
    | some c =>
      if isDigit c then
        -- Leading zeros not allowed for decimal
        throw (.invalidNumber pos "leading zeros not allowed")
      else
        -- Just the number 0
        return 0
    | none => return 0
  else
    return sign * (← parseDecimalInt)

/-- Parse fractional part of float (after decimal point) -/
def parseFraction : Parser Float := do
  let pos ← getPosition
  let mut result : Float := 0
  let mut divisor : Float := 10
  let mut hasDigit := false
  let mut lastWasUnderscore := false
  let mut going := true

  while going do
    match ← peek? with
    | some c =>
      if isDigit c then
        let _ ← next
        result := result + (c.toNat - '0'.toNat).toFloat / divisor
        divisor := divisor * 10
        hasDigit := true
        lastWasUnderscore := false
      else if c == '_' && hasDigit && !lastWasUnderscore then
        let _ ← next
        lastWasUnderscore := true
      else
        going := false
    | none => going := false

  if !hasDigit then
    throw (.invalidNumber pos "expected digit after decimal point")
  if lastWasUnderscore then
    throw (.invalidNumber pos "number cannot end with underscore")

  return result

/-- Parse exponent part of float -/
def parseExponent : Parser Int := do
  let sign ← parseSign
  let value ← parseDecimalInt
  return sign * value

/-- Convert Int to Float -/
private def intToFloat (i : Int) : Float :=
  if i >= 0 then i.toNat.toFloat
  else -((-i).toNat.toFloat)

/-- Parse TOML float -/
def parseFloat : Parser Float := do
  let pos ← getPosition
  let sign ← parseSign
  let signFloat : Float := if sign < 0 then -1.0 else 1.0

  -- Check for special values
  match ← peekN 3 with
  | some "inf" =>
    expectString "inf"
    return signFloat * posInf
  | some "nan" =>
    expectString "nan"
    return nan
  | _ => pure ()

  -- Parse integer part
  let intPart ← parseDecimalInt

  -- Check for fraction or exponent
  let mut result := intToFloat intPart
  let mut isFloat := false

  if (← peek?) == some '.' then
    let _ ← next
    let frac ← parseFraction
    result := result + frac
    isFloat := true

  if (← peek?).any (fun c => c == 'e' || c == 'E') then
    let _ ← next
    let exp ← parseExponent
    result := result * Float.pow 10.0 (intToFloat exp)
    isFloat := true

  if !isFloat then
    throw (.invalidNumber pos "expected float (needs decimal point or exponent)")

  return signFloat * result

/-- Check if this looks like a float (has . or e/E after digits) -/
def looksLikeFloat : Parser Bool := do
  let startState ← get
  -- Skip optional sign
  if (← peek?).any (fun c => c == '+' || c == '-') then
    let _ ← next
  -- Check for inf/nan
  match ← peekN 3 with
  | some "inf" | some "nan" =>
    set startState
    return true
  | _ => pure ()
  -- Skip digits
  let mut going := true
  while going do
    if (← peek?).any (fun c => isDigit c || c == '_') then
      let _ ← next
    else
      going := false
  -- Check next char
  let isFloat := (← peek?).any (fun c => c == '.' || c == 'e' || c == 'E')
  set startState
  return isFloat

/-- Parse number (integer or float) -/
def parseNumber : Parser Value := do
  if ← looksLikeFloat then
    return .float (← parseFloat)
  else
    return .integer (← parseInteger)

end Totem.Parser
