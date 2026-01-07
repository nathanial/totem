/-
  Totem.Parser.Value
  TOML value parsing (all value types including arrays and inline tables) using Sift
-/
import Sift
import Totem.Parser.String
import Totem.Parser.Number
import Totem.Parser.Key
import Totem.Parser.DateTime

namespace Totem.Parser

open Sift Totem

/-- Get the type category of a value for homogeneous array checking -/
def valueTypeCategory : Value → String
  | .string _ => "string"
  | .integer _ => "integer"
  | .float _ => "float"
  | .boolean _ => "boolean"
  | .dateTime _ => "datetime"
  | .localDateTime _ => "datetime"
  | .localDate _ => "datetime"
  | .localTime _ => "datetime"
  | .array _ => "array"
  | .inlineTable _ => "table"

mutual
  /-- Parse a TOML value -/
  partial def parseValue : Sift.Parser Unit Value := do
    skipWs
    match ← peek with
    | some '"' =>
      -- Could be basic string or multi-line basic string
      if (← peekString 3) == some "\"\"\"" then
        return .string (← parseMultiLineBasicString)
      else
        return .string (← parseBasicString)
    | some '\'' =>
      -- Could be literal string or multi-line literal string
      if (← peekString 3) == some "'''" then
        return .string (← parseMultiLineLiteralString)
      else
        return .string (← parseLiteralString)
    | some '[' =>
      parseArray
    | some '{' =>
      parseInlineTable
    | some 't' =>
      let _ ← string "true"
      return .boolean true
    | some 'f' =>
      let _ ← string "false"
      return .boolean false
    | some c =>
      -- Could be number or datetime
      -- Datetime starts with digit and has specific pattern
      if isDigit c then
        if ← looksLikeDateTime then
          parseDateTimeOrDate
        else
          parseNumber
      else if c == '+' || c == '-' then
        -- Number with sign, or could be inf/nan
        parseNumber
      else
        Parser.fail s!"unexpected character: '{c}'"
    | none =>
      Parser.fail "unexpected end of input while parsing value"

  /-- Parse a TOML array -/
  partial def parseArray : Sift.Parser Unit Value := do
    let _ ← char '['
    skipTrivia

    let mut elements : Array Value := #[]
    let mut expectedType : Option String := none

    while (← peek) != some ']' do
      if ← atEnd then
        Parser.fail "unclosed array"

      let elem ← parseValue
      let elemType := valueTypeCategory elem

      -- Check homogeneous types
      match expectedType with
      | none => expectedType := some elemType
      | some expected =>
        if elemType != expected then
          Parser.fail "arrays must contain homogeneous types"

      elements := elements.push elem
      skipTrivia

      -- Check for comma or end
      if (← peek) == some ',' then
        let _ ← anyChar
        skipTrivia
      else if (← peek) != some ']' then
        Parser.fail "expected ',' or ']' in array"

    let _ ← char ']'
    return .array elements

  /-- Parse a TOML inline table -/
  partial def parseInlineTable : Sift.Parser Unit Value := do
    let _ ← char '{'
    skipWs

    let mut table := Table.empty

    while (← peek) != some '}' do
      if ← atEnd then
        Parser.fail "unclosed inline table"

      -- Parse key = value
      let keyParts ← parseDottedKey
      skipWs
      let _ ← char '='
      skipWs
      let value ← parseValue

      -- Check for duplicate key
      match keyParts with
      | [k] =>
        if table.contains k then
          Parser.fail s!"duplicate key '{k}'"
        table := table.insert k value
      | _ =>
        -- Handle dotted key in inline table
        table := table.insertPath keyParts value

      skipWs

      -- Check for comma or end
      if (← peek) == some ',' then
        let _ ← anyChar
        skipWs
      else if (← peek) != some '}' then
        Parser.fail "expected ',' or '}' in inline table"

    let _ ← char '}'
    return .inlineTable table
end

end Totem.Parser
