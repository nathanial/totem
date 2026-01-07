/-
  Totem.Parser.Document
  Top-level TOML document parsing using Sift
-/
import Sift
import Totem.Parser.Value

namespace Totem.Parser

open Sift Totem

/-- Parse a table header path [table.path] -/
def parseTableHeader : Sift.Parser Unit (List String) := do
  let _ ← char '['
  skipWs
  let path ← parseDottedKey
  skipWs
  let _ ← char ']'
  return path

/-- Parse an array of tables header [[array.path]] -/
def parseArrayOfTablesHeader : Sift.Parser Unit (List String) := do
  let _ ← string "[["
  skipWs
  let path ← parseDottedKey
  skipWs
  let _ ← string "]]"
  return path

/-- State for tracking implicit vs explicit tables -/
structure DocParserState where
  /-- Set of paths that have been explicitly defined as tables -/
  explicitTables : Array (List String) := #[]
  /-- Set of paths that are arrays of tables -/
  arrayOfTables : Array (List String) := #[]

/-- Check if a table path would conflict with existing definitions -/
def checkTableConflict (state : DocParserState) (path : List String) : Sift.Parser Unit Unit := do
  -- Check if this exact path was already defined as an explicit table
  if state.explicitTables.contains path then
    Parser.fail s!"duplicate table '{String.intercalate "." path}'"
  -- Check if any prefix is an array of tables (can't redefine as regular table)
  let mut i := 0
  while i < path.length do
    let pathPrefix := path.take (i + 1)
    if state.arrayOfTables.contains pathPrefix && pathPrefix != path then
      Parser.fail s!"cannot define table '{String.intercalate "." path}' because '{String.intercalate "." pathPrefix}' is an array of tables"
    i := i + 1

/-- Ensure parent tables exist for a path -/
def ensureParentTables (root : Table) (path : List String) : Table :=
  match path with
  | [] => root
  | [_] => root
  | k :: ks =>
    let parent := match root.get? k with
      | some (.inlineTable t) => t
      | _ => Table.empty
    let updated := ensureParentTables parent ks
    root.insert k (.inlineTable updated)

/-- Get or create the table at a given path -/
def getOrCreateTable (root : Table) (path : List String) : Table × Table :=
  match path with
  | [] => (root, root)
  | _ =>
    let parentPath := path.dropLast
    let key := path.getLast!
    let parent := root.getTablePath? parentPath |>.getD Table.empty
    let target := match parent.get? key with
      | some (.inlineTable t) => t
      | _ => Table.empty
    (root.insertPath parentPath (.inlineTable (parent.insert key (.inlineTable target))), target)

/-- Add a value to an array of tables -/
def appendToArrayOfTables (root : Table) (path : List String) (newTable : Table) : Table :=
  match path with
  | [] => root
  | [key] =>
    -- Single-element path: insert directly into root
    let arr := match root.get? key with
      | some (.array tables) => tables.push (.inlineTable newTable)
      | _ => #[.inlineTable newTable]
    root.insert key (.array arr)
  | _ =>
    let parentPath := path.dropLast
    let key := path.getLast!
    let parent := root.getTablePath? parentPath |>.getD Table.empty
    let arr := match parent.get? key with
      | some (.array tables) => tables.push (.inlineTable newTable)
      | _ => #[.inlineTable newTable]
    root.insertPath parentPath (.inlineTable (parent.insert key (.array arr)))

/-- Parse a complete TOML document -/
partial def parseDocument : Sift.Parser Unit Table := do
  let mut root := Table.empty
  let mut currentPath : List String := []
  let mut state : DocParserState := {}

  skipTrivia

  while !(← atEnd) do
    -- Check what we're looking at
    match ← peek with
    | some '[' =>
      -- Could be table header or array of tables
      if (← peekString 2) == some "[[" then
        -- Array of tables
        let path ← parseArrayOfTablesHeader
        skipLineTrivia
        let _ ← skipNewline
        skipTrivia

        -- Record this as an array of tables path
        state := { state with arrayOfTables := state.arrayOfTables.push path }
        currentPath := path

        -- Append a new empty table to the array
        root := appendToArrayOfTables root path Table.empty
      else
        -- Regular table header
        let path ← parseTableHeader
        skipLineTrivia
        let _ ← skipNewline
        skipTrivia

        -- Check for conflicts
        checkTableConflict state path

        -- Record as explicit table
        state := { state with explicitTables := state.explicitTables.push path }
        currentPath := path

        -- Ensure the table exists
        root := root.insertPath path (.inlineTable Table.empty)

    | some '#' =>
      -- Comment line
      skipComment
      let _ ← skipNewline
      skipTrivia

    | some '\n' | some '\r' =>
      -- Empty line
      let _ ← skipNewline
      skipTrivia

    | some c =>
      if isBareKeyChar c || c == '"' || c == '\'' then
        -- Key-value pair
        let keyParts ← parseDottedKey
        skipWs
        let _ ← char '='
        skipWs
        let value ← parseValue
        skipLineTrivia
        let _ ← skipNewline
        skipTrivia

        -- Insert into current table context
        let fullPath := currentPath ++ keyParts

        -- For array of tables, we need to insert into the last element
        if state.arrayOfTables.contains currentPath then
          -- Get the array
          let parentPath := currentPath.dropLast
          let arrKey := currentPath.getLast!
          let parent := root.getTablePath? parentPath |>.getD Table.empty
          match parent.get? arrKey with
          | some (.array tables) =>
            if tables.size > 0 then
              -- Modify the last table in the array
              match tables[tables.size - 1]! with
              | .inlineTable lastTable =>
                let updated := lastTable.insertPath keyParts value
                let newArr := tables.set! (tables.size - 1) (.inlineTable updated)
                root := root.insertPath parentPath (.inlineTable (parent.insert arrKey (.array newArr)))
              | _ => pure ()
          | _ => pure ()
        else
          -- Regular table
          root := root.insertPath fullPath value
      else
        Parser.fail s!"unexpected character: '{c}'"

    | none =>
      -- End of input, will exit loop
      pure ()

  return root

/-- Parse TOML from a string -/
def parse (input : String) : Except Sift.ParseError Table :=
  Sift.Parser.parse parseDocument input

end Totem.Parser
