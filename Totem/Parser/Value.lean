/-
  Totem.Parser.Value
  TOML value parsing (all value types including arrays and inline tables)
-/
import Totem.Parser.String
import Totem.Parser.Number
import Totem.Parser.Key
import Totem.Parser.DateTime

namespace Totem.Parser

open Totem Parser

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
  partial def parseValue : Parser Value := do
    let pos ← getPosition
    skipWs
    match ← peek? with
    | some '"' =>
      -- Could be basic string or multi-line basic string
      if (← peekN 3) == some "\"\"\"" then
        return .string (← parseMultiLineBasicString)
      else
        return .string (← parseBasicString)
    | some '\'' =>
      -- Could be literal string or multi-line literal string
      if (← peekN 3) == some "'''" then
        return .string (← parseMultiLineLiteralString)
      else
        return .string (← parseLiteralString)
    | some '[' =>
      parseArray
    | some '{' =>
      parseInlineTable
    | some 't' =>
      expectString "true"
      return .boolean true
    | some 'f' =>
      expectString "false"
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
        throw (.invalidValue pos s!"unexpected character: '{c}'")
    | none =>
      throw (.unexpectedEnd "value")

  /-- Parse a TOML array -/
  partial def parseArray : Parser Value := do
    let pos ← getPosition
    expect '['
    skipTrivia

    let mut elements : Array Value := #[]
    let mut expectedType : Option String := none

    while (← peek?) != some ']' do
      if ← atEnd then
        throw (.invalidValue pos "unclosed array")

      let elem ← parseValue
      let elemType := valueTypeCategory elem

      -- Check homogeneous types
      match expectedType with
      | none => expectedType := some elemType
      | some expected =>
        if elemType != expected then
          throw (.mixedArrayTypes pos)

      elements := elements.push elem
      skipTrivia

      -- Check for comma or end
      if (← peek?) == some ',' then
        let _ ← next
        skipTrivia
      else if (← peek?) != some ']' then
        throw (.invalidValue pos "expected ',' or ']' in array")

    expect ']'
    return .array elements

  /-- Parse a TOML inline table -/
  partial def parseInlineTable : Parser Value := do
    let pos ← getPosition
    expect '{'
    skipWs

    let mut table := Table.empty

    while (← peek?) != some '}' do
      if ← atEnd then
        throw (.invalidInlineTable pos "unclosed inline table")

      -- Parse key = value
      let keyParts ← parseDottedKey
      skipWs
      expect '='
      skipWs
      let value ← parseValue

      -- Check for duplicate key
      let fullKey := String.intercalate "." keyParts
      match keyParts with
      | [k] =>
        if table.contains k then
          throw (.duplicateKey pos k)
        table := table.insert k value
      | _ =>
        -- Handle dotted key in inline table
        table := table.insertPath keyParts value

      skipWs

      -- Check for comma or end
      if (← peek?) == some ',' then
        let _ ← next
        skipWs
      else if (← peek?) != some '}' then
        throw (.invalidInlineTable pos "expected ',' or '}' in inline table")

    expect '}'
    return .inlineTable table
end

end Totem.Parser
