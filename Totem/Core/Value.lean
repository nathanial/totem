/-
  Totem.Core.Value
  TOML value types
-/

namespace Totem

/-- Pad a string on the left with a character to reach a minimum length -/
private def padLeft (s : String) (width : Nat) (c : Char := ' ') : String :=
  if s.length >= width then s
  else String.mk (List.replicate (width - s.length) c) ++ s

/-- Remove trailing characters matching predicate -/
private def dropRightWhile (s : String) (p : Char → Bool) : String :=
  String.mk (s.data.reverse.dropWhile p).reverse

/-- Local date (YYYY-MM-DD) -/
structure LocalDate where
  year : Int
  month : Nat
  day : Nat
  deriving Repr, BEq, Inhabited

instance : ToString LocalDate where
  toString d :=
    let y := if d.year < 0 then s!"-{padLeft (toString (-d.year).toNat) 4 '0'}"
             else padLeft (toString d.year.toNat) 4 '0'
    let m := padLeft (toString d.month) 2 '0'
    let dd := padLeft (toString d.day) 2 '0'
    s!"{y}-{m}-{dd}"

/-- Local time (HH:MM:SS.nnnnnnnnn) -/
structure LocalTime where
  hour : Nat
  minute : Nat
  second : Nat
  nanosecond : Nat := 0
  deriving Repr, BEq, Inhabited

instance : ToString LocalTime where
  toString t :=
    let h := padLeft (toString t.hour) 2 '0'
    let m := padLeft (toString t.minute) 2 '0'
    let s := padLeft (toString t.second) 2 '0'
    if t.nanosecond == 0 then s!"{h}:{m}:{s}"
    else
      let ns := padLeft (toString t.nanosecond) 9 '0'
      let trimmed := dropRightWhile ns (· == '0')
      s!"{h}:{m}:{s}.{trimmed}"

/-- Timezone offset in minutes from UTC -/
structure TimezoneOffset where
  minutes : Int
  deriving Repr, BEq, Inhabited

instance : ToString TimezoneOffset where
  toString tz :=
    if tz.minutes == 0 then "Z"
    else
      let sign := if tz.minutes >= 0 then "+" else "-"
      let totalMins := tz.minutes.natAbs
      let hours := totalMins / 60
      let mins := totalMins % 60
      s!"{sign}{padLeft (toString hours) 2 '0'}:{padLeft (toString mins) 2 '0'}"

/-- DateTime with optional timezone -/
structure DateTime where
  date : LocalDate
  time : LocalTime
  timezone : Option TimezoneOffset := none
  deriving Repr, BEq, Inhabited

instance : ToString DateTime where
  toString dt :=
    match dt.timezone with
    | none => s!"{dt.date}T{dt.time}"
    | some tz => s!"{dt.date}T{dt.time}{tz}"

-- Forward declaration for mutual recursion
mutual
  /-- TOML value representation -/
  inductive Value where
    | string (val : String)
    | integer (val : Int)
    | float (val : Float)
    | boolean (val : Bool)
    | dateTime (val : DateTime)
    | localDateTime (val : DateTime)  -- DateTime without timezone
    | localDate (val : LocalDate)
    | localTime (val : LocalTime)
    | array (val : Array Value)
    | inlineTable (val : Table)
    deriving Repr, Inhabited

  /-- A TOML table (ordered key-value map) -/
  structure Table where
    entries : Array (String × Value) := #[]
    deriving Repr, Inhabited
end

namespace Value

/-- Get a descriptive type name for error messages -/
def typeName : Value → String
  | .string _ => "String"
  | .integer _ => "Integer"
  | .float _ => "Float"
  | .boolean _ => "Boolean"
  | .dateTime _ => "DateTime"
  | .localDateTime _ => "LocalDateTime"
  | .localDate _ => "LocalDate"
  | .localTime _ => "LocalTime"
  | .array _ => "Array"
  | .inlineTable _ => "Table"

end Value

namespace Table

def empty : Table := { entries := #[] }

def isEmpty (t : Table) : Bool := t.entries.isEmpty

def size (t : Table) : Nat := t.entries.size

/-- Insert or update a key-value pair -/
def insert (t : Table) (key : String) (val : Value) : Table :=
  -- Check if key already exists
  match t.entries.findIdx? (fun (k, _) => k == key) with
  | some idx => { entries := t.entries.set! idx (key, val) }
  | none => { entries := t.entries.push (key, val) }

/-- Get value by key -/
def get? (t : Table) (key : String) : Option Value :=
  t.entries.find? (fun (k, _) => k == key) |>.map Prod.snd

/-- Check if key exists -/
def contains (t : Table) (key : String) : Bool :=
  t.entries.any (fun (k, _) => k == key)

/-- Get all keys -/
def keys (t : Table) : Array String :=
  t.entries.map Prod.fst

/-- Navigate nested tables with dotted path -/
def getPath? (t : Table) (path : List String) : Option Value :=
  match path with
  | [] => none
  | [k] => t.get? k
  | k :: ks =>
    match t.get? k with
    | some (.inlineTable nested) => nested.getPath? ks
    | _ => none

/-- Navigate nested tables and return the table at a path -/
def getTablePath? (t : Table) (path : List String) : Option Table :=
  match path with
  | [] => some t
  | k :: ks =>
    match t.get? k with
    | some (.inlineTable nested) => nested.getTablePath? ks
    | _ => none

/-- Insert at a nested path, creating intermediate tables as needed -/
partial def insertPath (t : Table) (path : List String) (val : Value) : Table :=
  match path with
  | [] => t
  | [k] => t.insert k val
  | k :: ks =>
    let nested := match t.get? k with
      | some (.inlineTable tbl) => tbl
      | _ => Table.empty
    let updated := nested.insertPath ks val
    t.insert k (.inlineTable updated)

/-- Merge another table into this one -/
def merge (t1 t2 : Table) : Table :=
  t2.entries.foldl (fun acc (k, v) => acc.insert k v) t1

end Table

end Totem
