/-
  Totem - TOML Configuration Parser for Lean 4

  ## Quick Start

  ```lean
  import Totem

  def main : IO Unit := do
    let toml := "
      [database]
      host = \"localhost\"
      port = 5432
    "
    match Totem.parse toml with
    | .ok table =>
      match table.getAs (α := String) "database.host" with
      | .ok host => IO.println s!"Host: {host}"
      | .error e => IO.eprintln s!"Error: {e}"
    | .error e => IO.eprintln s!"Parse error: {e}"
  ```

  ## Environment Variable Interpolation

  ```lean
  let toml := "api_key = \"${API_KEY:-default_key}\""
  let table ← Totem.parseWithEnv toml
  ```

  ## Features

  - Full TOML 1.0 support
  - Position-aware error messages with line/column
  - Typed extraction via FromConfig typeclass
  - Environment variable interpolation: ${VAR} and ${VAR:-default}
-/

import Sift
import Totem.Core
import Totem.Parser
import Totem.Extract
import Totem.Env

namespace Totem

/-- Parse TOML from a string -/
def parse (input : String) : Except Sift.ParseError Table :=
  Parser.parse input

/-- Extract env var names from a string -/
private partial def extractEnvVarsFromString (s : String) : Array String := Id.run do
  let mut result : Array String := #[]
  let mut i := 0
  let chars := s.toList.toArray
  while i < chars.size do
    if chars[i]! == '$' && i + 1 < chars.size && chars[i + 1]! == '{' then
      let start := i + 2
      let mut j := start
      while j < chars.size && chars[j]! != '}' do
        j := j + 1
      if j < chars.size then
        let inner := String.mk ((chars.toList.drop start).take (j - start))
        -- Extract var name (before :- if present)
        let varName := match inner.splitOn ":-" with
          | name :: _ => name
          | _ => inner
        if !result.contains varName then
          result := result.push varName
      i := j + 1
    else
      i := i + 1
  return result

/-- Collect all environment variable names referenced in a table -/
private partial def collectEnvVarNames (v : Value) (names : Array String) : Array String :=
  match v with
  | .string s =>
    let varNames := extractEnvVarsFromString s
    varNames.foldl (fun acc name => if acc.contains name then acc else acc.push name) names
  | .array arr =>
    arr.foldl (fun acc elem => collectEnvVarNames elem acc) names
  | .inlineTable t =>
    t.entries.foldl (fun acc (_, val) => collectEnvVarNames val acc) names
  | _ => names

/-- Parse TOML and interpolate environment variables -/
def parseWithEnv (input : String) : IO (Except String Table) := do
  match parse input with
  | .error e => return .error (toString e)
  | .ok table =>
    -- Collect all referenced env var names
    let envVarNames := table.entries.foldl (fun acc (_, v) => collectEnvVarNames v acc) #[]
    -- Look them all up
    let mut envPairs : Array (String × String) := #[]
    for name in envVarNames do
      match ← IO.getEnv name with
      | some val => envPairs := envPairs.push (name, val)
      | none => pure ()
    -- Build lookup function
    let getEnv := fun name => envPairs.find? (fun (k, _) => k == name) |>.map Prod.snd
    match Env.interpolateTable table getEnv with
    | .error e => return .error (toString e)
    | .ok t => return .ok t

/-- Parse TOML with a custom environment resolver -/
def parseWithEnvResolver (input : String) (getEnv : String → Option String)
    : Except String Table := do
  match Parser.parse input with
  | .error e => .error (toString e)
  | .ok table =>
    match Env.interpolateTable table getEnv with
    | .error e => .error (toString e)
    | .ok t => .ok t

/-- Load TOML from a file -/
def loadFile (path : System.FilePath) : IO (Except String Table) := do
  try
    let content ← IO.FS.readFile path
    match parse content with
    | .ok t => return .ok t
    | .error e => return .error (toString e)
  catch e => return .error s!"Failed to read file: {e}"

/-- Load TOML from a file with environment variable interpolation -/
def loadFileWithEnv (path : System.FilePath) : IO (Except String Table) := do
  try
    let content ← IO.FS.readFile path
    parseWithEnv content
  catch e => return .error s!"Failed to read file: {e}"

end Totem
