/-
  Totem.Parser.Key
  TOML key parsing (bare and quoted keys) using Sift
-/
import Sift
import Totem.Parser.String

namespace Totem.Parser

open Sift

/-- Parse a bare key (A-Za-z0-9_-) -/
def parseBareKey : Sift.Parser Unit String := do
  let result ← takeWhile isBareKeyChar
  if result.isEmpty then
    match ← peek with
    | some c => Parser.fail s!"invalid key character: '{c}'"
    | none => Parser.fail "expected key"
  return result

/-- Parse a single key (bare or quoted) -/
def parseSimpleKey : Sift.Parser Unit String := do
  match ← peek with
  | some '"' => parseBasicString
  | some '\'' => parseLiteralString
  | _ => parseBareKey

/-- Parse a dotted key (key.key.key) returning list of parts -/
partial def parseDottedKey : Sift.Parser Unit (List String) := do
  let mut parts := []
  let first ← parseSimpleKey
  parts := [first]
  while (← peek) == some '.' do
    let _ ← anyChar
    skipWs
    let part ← parseSimpleKey
    parts := parts ++ [part]
  return parts

end Totem.Parser
