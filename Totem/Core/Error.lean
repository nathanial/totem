/-
  Totem.Core.Error
  Parse errors and extraction errors
-/
import Totem.Core.Position

namespace Totem

open Totem

/-- Parse errors with position and context -/
inductive ParseError where
  | unexpectedChar (pos : Position) (char : Char) (expected : String)
  | unexpectedEnd (context : String)
  | invalidKey (pos : Position) (msg : String)
  | invalidString (pos : Position) (msg : String)
  | invalidNumber (pos : Position) (msg : String)
  | invalidDateTime (pos : Position) (msg : String)
  | invalidValue (pos : Position) (msg : String)
  | duplicateKey (pos : Position) (key : String)
  | invalidTablePath (pos : Position) (path : String)
  | mixedArrayTypes (pos : Position)
  | invalidInlineTable (pos : Position) (msg : String)
  | other (pos : Position) (msg : String)
  deriving Repr, BEq

instance : ToString ParseError where
  toString e := match e with
    | .unexpectedChar pos c exp => s!"{pos}: unexpected character '{c}', expected {exp}"
    | .unexpectedEnd ctx => s!"unexpected end of input while parsing {ctx}"
    | .invalidKey pos msg => s!"{pos}: invalid key: {msg}"
    | .invalidString pos msg => s!"{pos}: invalid string: {msg}"
    | .invalidNumber pos msg => s!"{pos}: invalid number: {msg}"
    | .invalidDateTime pos msg => s!"{pos}: invalid datetime: {msg}"
    | .invalidValue pos msg => s!"{pos}: invalid value: {msg}"
    | .duplicateKey pos key => s!"{pos}: duplicate key '{key}'"
    | .invalidTablePath pos path => s!"{pos}: invalid table path '{path}'"
    | .mixedArrayTypes pos => s!"{pos}: arrays must contain homogeneous types"
    | .invalidInlineTable pos msg => s!"{pos}: invalid inline table: {msg}"
    | .other pos msg => s!"{pos}: {msg}"

abbrev ParseResult (α : Type) := Except ParseError α

/-- Extraction errors for type conversion failures -/
inductive ExtractError where
  | typeConversion (path : String) (expected : String) (actual : String)
  | keyNotFound (path : String)
  | indexOutOfBounds (path : String) (index : Nat) (size : Nat)
  | envVarNotFound (varName : String)
  | envVarInterpolationFailed (path : String) (msg : String)
  deriving Repr, BEq

instance : ToString ExtractError where
  toString e := match e with
    | .typeConversion path exp act => s!"at '{path}': expected {exp}, got {act}"
    | .keyNotFound path => s!"key not found: '{path}'"
    | .indexOutOfBounds path idx sz => s!"at '{path}': index {idx} out of bounds (size {sz})"
    | .envVarNotFound v => s!"environment variable not found: '{v}'"
    | .envVarInterpolationFailed path msg => s!"at '{path}': env interpolation failed: {msg}"

abbrev ExtractResult (α : Type) := Except ExtractError α

namespace ParseResult

def toIO (r : ParseResult α) : IO α :=
  match r with
  | .ok a => pure a
  | .error e => throw (IO.userError (toString e))

end ParseResult

namespace ExtractResult

def toIO (r : ExtractResult α) : IO α :=
  match r with
  | .ok a => pure a
  | .error e => throw (IO.userError (toString e))

end ExtractResult

end Totem
