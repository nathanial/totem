/-
  Totem.Env
  Environment variable interpolation for TOML strings
-/
import Totem.Core.Value
import Totem.Core.Error

namespace Totem.Env

open Totem

/-- Pattern for environment variable: ${VAR} or ${VAR:-default} -/
structure EnvPattern where
  varName : String
  defaultValue : Option String
  deriving Repr

/-- Find the first index where a character matches -/
private def findCharIdx (s : String) (c : Char) : Option Nat :=
  let chars := s.toList
  chars.findIdx? (· == c)

/-- Get character at index from list (using Array) -/
private def listGetChar (chars : List Char) (idx : Nat) : Option Char :=
  chars.toArray[idx]?

/-- Parse env pattern from ${...} content (without the ${} wrapper) -/
def parseEnvPatternInner (inner : String) : Option EnvPattern :=
  -- Check for default value separator :-
  match findCharIdx inner ':' with
  | some colonIdx =>
    let chars := inner.toList
    let after := listGetChar chars (colonIdx + 1)
    if after == some '-' then
      let varName := String.mk (chars.take colonIdx)
      let defaultVal := String.mk (chars.drop (colonIdx + 2))
      some { varName, defaultValue := some defaultVal }
    else
      -- Just has a colon but not :-, treat as variable name
      some { varName := inner, defaultValue := none }
  | none =>
    some { varName := inner, defaultValue := none }

/-- State for interpolation -/
private structure InterpolateState where
  result : String
  i : Nat
  chars : Array Char

/-- Interpolate a single string value, resolving ${VAR} and ${VAR:-default} -/
partial def interpolateString (s : String) (getEnv : String → Option String) : ExtractResult String :=
  let chars := s.toList.toArray
  go { result := "", i := 0, chars } getEnv
where
  go (state : InterpolateState) (getEnv : String → Option String) : ExtractResult String :=
    if state.i >= state.chars.size then
      .ok state.result
    else
      let c := state.chars[state.i]!
      if c == '$' && state.i + 1 < state.chars.size && state.chars[state.i + 1]! == '{' then
        -- Find closing brace
        let start := state.i + 2
        let (depth, j) := findClosingBrace state.chars start 1
        if depth != 0 then
          .error (.envVarInterpolationFailed "" "unclosed ${")
        else
          let innerChars := (state.chars.toList.drop start).take (j - start - 1)
          let inner := String.mk innerChars
          match parseEnvPatternInner inner with
          | some ep =>
            match getEnv ep.varName with
            | some val =>
              go { state with result := state.result ++ val, i := j } getEnv
            | none =>
              match ep.defaultValue with
              | some defaultVal =>
                go { state with result := state.result ++ defaultVal, i := j } getEnv
              | none =>
                .error (.envVarNotFound ep.varName)
          | none =>
            .error (.envVarInterpolationFailed "" ("invalid pattern: ${" ++ inner ++ "}"))
      else
        go { state with result := state.result.push c, i := state.i + 1 } getEnv

  findClosingBrace (chars : Array Char) (j : Nat) (depth : Nat) : Nat × Nat :=
    if depth == 0 || j >= chars.size then
      (depth, j)
    else
      let newDepth :=
        if chars[j]! == '{' then depth + 1
        else if chars[j]! == '}' then depth - 1
        else depth
      findClosingBrace chars (j + 1) newDepth

-- Forward declaration for mutual recursion
mutual
  /-- Recursively interpolate all string values in a Value -/
  partial def interpolateValue (v : Value) (getEnv : String → Option String) : ExtractResult Value :=
    match v with
    | .string s =>
      match interpolateString s getEnv with
      | .ok s' => .ok (.string s')
      | .error e => .error e
    | .array arr =>
      match interpolateArray arr getEnv with
      | .ok arr' => .ok (.array arr')
      | .error e => .error e
    | .inlineTable t =>
      match interpolateTable t getEnv with
      | .ok t' => .ok (.inlineTable t')
      | .error e => .error e
    | other => .ok other  -- Non-string values pass through unchanged

  /-- Recursively interpolate all values in an array -/
  partial def interpolateArray (arr : Array Value) (getEnv : String → Option String) : ExtractResult (Array Value) :=
    arr.foldlM (init := #[]) fun acc v =>
      match interpolateValue v getEnv with
      | .ok v' => Except.ok (acc.push v')
      | .error e => Except.error e

  /-- Recursively interpolate all string values in a Table -/
  partial def interpolateTable (t : Table) (getEnv : String → Option String) : ExtractResult Table :=
    let result : ExtractResult (Array (String × Value)) := t.entries.foldlM (init := #[]) fun acc (k, v) =>
      match interpolateValue v getEnv with
      | .ok v' => Except.ok (acc.push (k, v'))
      | .error e => Except.error e
    match result with
    | .ok entries => Except.ok { entries }
    | .error e => Except.error e
end

end Totem.Env
