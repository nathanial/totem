/-
  Totem.Parser.DateTime
  TOML datetime parsing (RFC 3339)
-/
import Totem.Parser.Primitives
import Totem.Core.Value

namespace Totem.Parser

open Totem Parser

/-- Parse local date: YYYY-MM-DD -/
def parseLocalDate : Parser LocalDate := do
  let pos ← getPosition
  let yearStr ← readExactDigits 4
  expect '-'
  let monthStr ← readExactDigits 2
  expect '-'
  let dayStr ← readExactDigits 2

  let year := Int.ofNat yearStr.toNat!
  let month := monthStr.toNat!
  let day := dayStr.toNat!

  -- Basic validation
  if month < 1 || month > 12 then
    throw (.invalidDateTime pos s!"invalid month: {month}")
  if day < 1 || day > 31 then
    throw (.invalidDateTime pos s!"invalid day: {day}")

  return { year, month, day }

/-- Parse fractional seconds, returning nanoseconds -/
def parseFractionalSeconds : Parser Nat := do
  let mut result : Nat := 0
  let mut count := 0
  let mut going := true
  while going do
    match ← peek? with
    | some c =>
      if isDigit c && count < 9 then
        let _ ← next
        result := result * 10 + (c.toNat - '0'.toNat)
        count := count + 1
      else if isDigit c then
        -- Skip extra digits beyond nanosecond precision
        let _ ← next
      else
        going := false
    | none => going := false
  -- Pad to nanoseconds
  while count < 9 do
    result := result * 10
    count := count + 1
  return result

/-- Parse local time: HH:MM:SS[.nnnnnnnnn] -/
def parseLocalTime : Parser LocalTime := do
  let pos ← getPosition
  let hourStr ← readExactDigits 2
  expect ':'
  let minuteStr ← readExactDigits 2
  expect ':'
  let secondStr ← readExactDigits 2

  let hour := hourStr.toNat!
  let minute := minuteStr.toNat!
  let second := secondStr.toNat!

  -- Basic validation
  if hour > 23 then
    throw (.invalidDateTime pos s!"invalid hour: {hour}")
  if minute > 59 then
    throw (.invalidDateTime pos s!"invalid minute: {minute}")
  if second > 60 then  -- Allow 60 for leap seconds
    throw (.invalidDateTime pos s!"invalid second: {second}")

  -- Optional fractional seconds
  let nanosecond ← if (← peek?) == some '.' then
    let _ ← next
    parseFractionalSeconds
  else
    pure 0

  return { hour, minute, second, nanosecond }

/-- Parse timezone offset: Z, +HH:MM, -HH:MM -/
def parseTimezoneOffset : Parser TimezoneOffset := do
  let pos ← getPosition
  match ← peek? with
  | some 'Z' | some 'z' =>
    let _ ← next
    return { minutes := 0 }
  | some '+' =>
    let _ ← next
    let hourStr ← readExactDigits 2
    expect ':'
    let minuteStr ← readExactDigits 2
    let hours := hourStr.toNat!
    let mins := minuteStr.toNat!
    if hours > 23 || mins > 59 then
      throw (.invalidDateTime pos "invalid timezone offset")
    return { minutes := Int.ofNat (hours * 60 + mins) }
  | some '-' =>
    let _ ← next
    let hourStr ← readExactDigits 2
    expect ':'
    let minuteStr ← readExactDigits 2
    let hours := hourStr.toNat!
    let mins := minuteStr.toNat!
    if hours > 23 || mins > 59 then
      throw (.invalidDateTime pos "invalid timezone offset")
    return { minutes := -(Int.ofNat (hours * 60 + mins)) }
  | _ =>
    throw (.invalidDateTime pos "expected timezone offset (Z, +HH:MM, or -HH:MM)")

/-- Parse full datetime with timezone: YYYY-MM-DDTHH:MM:SS[.nnn][Z|+HH:MM|-HH:MM] -/
def parseFullDateTime : Parser DateTime := do
  let date ← parseLocalDate
  -- Accept T or space as separator
  match ← peek? with
  | some 'T' | some 't' | some ' ' => let _ ← next
  | _ =>
    let pos ← getPosition
    throw (.invalidDateTime pos "expected T or space between date and time")
  let time ← parseLocalTime
  let timezone ← parseTimezoneOffset
  return { date, time, timezone := some timezone }

/-- Parse local datetime (no timezone): YYYY-MM-DDTHH:MM:SS[.nnn] -/
def parseLocalDateTime : Parser DateTime := do
  let date ← parseLocalDate
  match ← peek? with
  | some 'T' | some 't' | some ' ' => let _ ← next
  | _ =>
    let pos ← getPosition
    throw (.invalidDateTime pos "expected T or space between date and time")
  let time ← parseLocalTime
  return { date, time, timezone := none }

/-- Check what kind of datetime this is and parse accordingly -/
def parseDateTimeOrDate : Parser Value := do
  let startState ← get
  let date ← parseLocalDate

  -- Check if there's a time component
  match ← peek? with
  | some 'T' | some 't' | some ' ' =>
    set startState
    -- Check if there's a timezone after the time
    let dt ← parseLocalDateTime
    match ← peek? with
    | some 'Z' | some 'z' | some '+' | some '-' =>
      -- Has timezone, reparse as full datetime
      set startState
      return .dateTime (← parseFullDateTime)
    | _ =>
      -- No timezone, it's a local datetime
      return .localDateTime dt
  | _ =>
    -- Just a date
    return .localDate date

/-- Check if current position looks like a datetime (starts with 4 digits and hyphen) -/
def looksLikeDateTime : Parser Bool := do
  let startState ← get
  let mut count := 0
  -- Check for 4 digits
  while count < 4 do
    match ← peek? with
    | some c =>
      if isDigit c then
        let _ ← next
        count := count + 1
      else
        set startState
        return false
    | none =>
      set startState
      return false
  -- Check for hyphen
  let result := (← peek?) == some '-'
  set startState
  return result

end Totem.Parser
