/-
  Totem.Parser.Key
  TOML key parsing (bare and quoted keys)
-/
import Totem.Parser.String

namespace Totem.Parser

open Totem Parser

/-- Parse a bare key (A-Za-z0-9_-) -/
def parseBareKey : Parser String := do
  let pos ← getPosition
  let result ← readWhile isBareKeyChar
  if result.isEmpty then
    match ← peek? with
    | some c => throw (.invalidKey pos s!"invalid key character: '{c}'")
    | none => throw (.invalidKey pos "expected key")
  return result

/-- Parse a single key (bare or quoted) -/
def parseSimpleKey : Parser String := do
  match ← peek? with
  | some '"' => parseBasicString
  | some '\'' => parseLiteralString
  | _ => parseBareKey

/-- Parse a dotted key (key.key.key) returning list of parts -/
def parseDottedKey : Parser (List String) := do
  let mut parts := []
  let first ← parseSimpleKey
  parts := [first]
  while (← peek?) == some '.' do
    let _ ← next
    skipWs
    let part ← parseSimpleKey
    parts := parts ++ [part]
  return parts

end Totem.Parser
