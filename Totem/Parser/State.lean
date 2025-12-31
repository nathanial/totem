/-
  Totem.Parser.State
  Parser state and monad definition
-/
import Totem.Core.Error
import Totem.Core.Position

namespace Totem.Parser

open Totem

/-- Parser state tracking position in TOML input -/
structure ParserState where
  input : String
  pos : Nat := 0
  line : Nat := 1
  column : Nat := 1
  deriving Repr

/-- Parser monad combining state and error handling -/
abbrev Parser := ExceptT ParseError (StateM ParserState)

namespace Parser

/-- Get current position as Position struct -/
def getPosition : Parser Position := do
  let s ← get
  return { offset := s.pos, line := s.line, column := s.column }

/-- Check if at end of input -/
def atEnd : Parser Bool := do
  let s ← get
  return s.pos >= s.input.length

/-- Peek at current character without consuming -/
def peek? : Parser (Option Char) := do
  let s ← get
  if s.pos >= s.input.length then
    return none
  else
    return some (s.input.get ⟨s.pos⟩)

/-- Peek at current character, error if at end -/
def peek : Parser Char := do
  match ← peek? with
  | some c => return c
  | none => throw (.unexpectedEnd "input")

/-- Peek at next N characters without consuming -/
def peekN (n : Nat) : Parser (Option String) := do
  let s ← get
  if s.pos + n > s.input.length then
    return none
  else
    return some (s.input.extract ⟨s.pos⟩ ⟨s.pos + n⟩)

/-- Consume and return current character, updating line/column -/
def next : Parser Char := do
  let s ← get
  if s.pos >= s.input.length then
    throw (.unexpectedEnd "input")
  let c := s.input.get ⟨s.pos⟩
  let (newLine, newCol) :=
    if c == '\n' then (s.line + 1, 1)
    else (s.line, s.column + 1)
  set { s with pos := s.pos + 1, line := newLine, column := newCol }
  return c

/-- Try to consume a specific character -/
def tryChar (c : Char) : Parser Bool := do
  match ← peek? with
  | some x =>
    if x == c then
      let _ ← next
      return true
    else
      return false
  | none => return false

/-- Expect a specific character, error if not found -/
def expect (c : Char) : Parser Unit := do
  let pos ← getPosition
  match ← peek? with
  | some x =>
    if x == c then
      let _ ← next
    else
      throw (.unexpectedChar pos x s!"'{c}'")
  | none => throw (.unexpectedEnd s!"expected '{c}'")

/-- Expect a specific string, error if not found -/
def expectString (s : String) : Parser Unit := do
  for c in s.toList do
    expect c

/-- Try a parser, restoring state on failure -/
def tryParse (p : Parser α) : Parser (Option α) := do
  let startState ← get
  try
    let result ← p
    return some result
  catch _ =>
    set startState
    return none

/-- Run parser with default on failure -/
def withDefault (default : α) (p : Parser α) : Parser α := do
  match ← tryParse p with
  | some result => return result
  | none => return default

/-- Run parser on input, returning result -/
def run {α : Type} (p : Parser α) (input : String) : ParseResult α :=
  let initState : ParserState := { input }
  let (result, _) := (ExceptT.run p).run initState
  result

end Parser

end Totem.Parser
