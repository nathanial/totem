/-
  Totem.Core.Error
  Extraction errors (parse errors now use Sift.ParseError)
-/
import Sift

namespace Totem

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

namespace ExtractResult

def toIO (r : ExtractResult α) : IO α :=
  match r with
  | .ok a => pure a
  | .error e => throw (IO.userError (toString e))

end ExtractResult

end Totem
