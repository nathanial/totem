/-
  Totem.Core.Position
  Source position tracking for error messages
-/

namespace Totem

/-- Position in TOML source for error messages -/
structure Position where
  offset : Nat
  line : Nat
  column : Nat
  deriving Repr, BEq, Inhabited

instance : ToString Position where
  toString p := s!"line {p.line}, column {p.column}"

namespace Position

def initial : Position := { offset := 0, line := 1, column := 1 }

end Position

end Totem
