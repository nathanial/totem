/-
  Totem.Extract
  FromConfig typeclass for typed extraction from TOML values
-/
import Totem.Core.Value
import Totem.Core.Error

namespace Totem

/-- Typeclass for extracting typed values from TOML -/
class FromConfig (α : Type) where
  fromConfig : Value → ExtractResult α

instance : FromConfig String where
  fromConfig v := match v with
    | .string s => .ok s
    | other => .error (.typeConversion "" "String" other.typeName)

instance : FromConfig Int where
  fromConfig v := match v with
    | .integer n => .ok n
    | other => .error (.typeConversion "" "Int" other.typeName)

instance : FromConfig Nat where
  fromConfig v := do
    let n : Int ← FromConfig.fromConfig v
    if n >= 0 then .ok n.toNat
    else .error (.typeConversion "" "Nat (non-negative)" "negative integer")

instance : FromConfig Float where
  fromConfig v := match v with
    | .float f => .ok f
    | .integer n => .ok (Float.ofInt n)  -- Allow int -> float coercion
    | other => .error (.typeConversion "" "Float" other.typeName)

instance : FromConfig Bool where
  fromConfig v := match v with
    | .boolean b => .ok b
    | other => .error (.typeConversion "" "Bool" other.typeName)

instance : FromConfig DateTime where
  fromConfig v := match v with
    | .dateTime dt => .ok dt
    | .localDateTime dt => .ok dt
    | other => .error (.typeConversion "" "DateTime" other.typeName)

instance : FromConfig LocalDate where
  fromConfig v := match v with
    | .localDate d => .ok d
    | .dateTime dt => .ok dt.date
    | .localDateTime dt => .ok dt.date
    | other => .error (.typeConversion "" "LocalDate" other.typeName)

instance : FromConfig LocalTime where
  fromConfig v := match v with
    | .localTime t => .ok t
    | .dateTime dt => .ok dt.time
    | .localDateTime dt => .ok dt.time
    | other => .error (.typeConversion "" "LocalTime" other.typeName)

instance [FromConfig α] : FromConfig (Option α) where
  fromConfig v := match FromConfig.fromConfig v with
    | .ok x => .ok (some x)
    | .error _ => .ok none

instance [FromConfig α] : FromConfig (Array α) where
  fromConfig v := match v with
    | .array arr => arr.mapM FromConfig.fromConfig
    | other => .error (.typeConversion "" "Array" other.typeName)

instance [FromConfig α] : FromConfig (List α) where
  fromConfig v := do
    let arr : Array α ← FromConfig.fromConfig v
    return arr.toList

instance : FromConfig Table where
  fromConfig v := match v with
    | .inlineTable t => .ok t
    | other => .error (.typeConversion "" "Table" other.typeName)

instance : FromConfig Value where
  fromConfig v := .ok v

namespace Table

/-- Extract a value at a dotted path -/
def getAs [FromConfig α] (t : Table) (path : String) : ExtractResult α := do
  let keys := path.splitOn "."
  match t.getPath? keys with
  | some v => FromConfig.fromConfig v |>.mapError fun e =>
      match e with
      | .typeConversion _ exp act => .typeConversion path exp act
      | other => other
  | none => .error (.keyNotFound path)

/-- Try to extract a value, returning Option -/
def getAsOption [FromConfig α] (t : Table) (path : String) : ExtractResult (Option α) := do
  let keys := path.splitOn "."
  match t.getPath? keys with
  | some v =>
    match FromConfig.fromConfig v with
    | .ok x => .ok (some x)
    | .error e => .error e
  | none => .ok none

/-- Extract a value with a default -/
def getAsOrDefault [FromConfig α] (t : Table) (path : String) (default : α) : ExtractResult α := do
  match ← t.getAsOption path with
  | some x => .ok x
  | none => .ok default

/-- Extract a nested table -/
def getTable (t : Table) (path : String) : ExtractResult Table := do
  let keys := path.splitOn "."
  match t.getTablePath? keys with
  | some nested => .ok nested
  | none => .error (.keyNotFound path)

/-- Check if a key exists -/
def hasKey (t : Table) (path : String) : Bool :=
  let keys := path.splitOn "."
  t.getPath? keys |>.isSome

end Table

end Totem
