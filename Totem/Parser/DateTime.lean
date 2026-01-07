/-
  Totem.Parser.DateTime
  TOML datetime parsing (RFC 3339) using Sift
-/
import Sift
import Totem.Parser.Primitives
import Totem.Core.Value

namespace Totem.Parser

open Sift Totem

/-- Parse local date: YYYY-MM-DD -/
def parseLocalDate : Sift.Parser Unit LocalDate := do
  let yearStr ← readExactDigits 4
  let _ ← char '-'
  let monthStr ← readExactDigits 2
  let _ ← char '-'
  let dayStr ← readExactDigits 2

  let year := Int.ofNat yearStr.toNat!
  let month := monthStr.toNat!
  let day := dayStr.toNat!

  -- Basic validation
  if month < 1 || month > 12 then
    Parser.fail s!"invalid month: {month}"
  if day < 1 || day > 31 then
    Parser.fail s!"invalid day: {day}"

  return { year, month, day }

/-- Parse fractional seconds, returning nanoseconds -/
partial def parseFractionalSeconds : Sift.Parser Unit Nat := do
  let mut result : Nat := 0
  let mut count := 0
  let mut going := true
  while going do
    match ← peek with
    | some c =>
      if isDigit c && count < 9 then
        let _ ← anyChar
        result := result * 10 + (c.toNat - '0'.toNat)
        count := count + 1
      else if isDigit c then
        -- Skip extra digits beyond nanosecond precision
        let _ ← anyChar
      else
        going := false
    | none => going := false
  -- Pad to nanoseconds
  while count < 9 do
    result := result * 10
    count := count + 1
  return result

/-- Parse local time: HH:MM:SS[.nnnnnnnnn] -/
def parseLocalTime : Sift.Parser Unit LocalTime := do
  let hourStr ← readExactDigits 2
  let _ ← char ':'
  let minuteStr ← readExactDigits 2
  let _ ← char ':'
  let secondStr ← readExactDigits 2

  let hour := hourStr.toNat!
  let minute := minuteStr.toNat!
  let second := secondStr.toNat!

  -- Basic validation
  if hour > 23 then
    Parser.fail s!"invalid hour: {hour}"
  if minute > 59 then
    Parser.fail s!"invalid minute: {minute}"
  if second > 60 then  -- Allow 60 for leap seconds
    Parser.fail s!"invalid second: {second}"

  -- Optional fractional seconds
  let nanosecond ← if (← peek) == some '.' then
    let _ ← anyChar
    parseFractionalSeconds
  else
    pure 0

  return { hour, minute, second, nanosecond }

/-- Parse timezone offset: Z, +HH:MM, -HH:MM -/
def parseTimezoneOffset : Sift.Parser Unit TimezoneOffset := do
  match ← peek with
  | some 'Z' | some 'z' =>
    let _ ← anyChar
    return { minutes := 0 }
  | some '+' =>
    let _ ← anyChar
    let hourStr ← readExactDigits 2
    let _ ← char ':'
    let minuteStr ← readExactDigits 2
    let hours := hourStr.toNat!
    let mins := minuteStr.toNat!
    if hours > 23 || mins > 59 then
      Parser.fail "invalid timezone offset"
    return { minutes := Int.ofNat (hours * 60 + mins) }
  | some '-' =>
    let _ ← anyChar
    let hourStr ← readExactDigits 2
    let _ ← char ':'
    let minuteStr ← readExactDigits 2
    let hours := hourStr.toNat!
    let mins := minuteStr.toNat!
    if hours > 23 || mins > 59 then
      Parser.fail "invalid timezone offset"
    return { minutes := -(Int.ofNat (hours * 60 + mins)) }
  | _ =>
    Parser.fail "expected timezone offset (Z, +HH:MM, or -HH:MM)"

/-- Parse full datetime with timezone: YYYY-MM-DDTHH:MM:SS[.nnn][Z|+HH:MM|-HH:MM] -/
def parseFullDateTime : Sift.Parser Unit DateTime := do
  let date ← parseLocalDate
  -- Accept T or space as separator
  match ← peek with
  | some 'T' | some 't' | some ' ' => let _ ← anyChar
  | _ => Parser.fail "expected T or space between date and time"
  let time ← parseLocalTime
  let timezone ← parseTimezoneOffset
  return { date, time, timezone := some timezone }

/-- Parse local datetime (no timezone): YYYY-MM-DDTHH:MM:SS[.nnn] -/
def parseLocalDateTime : Sift.Parser Unit DateTime := do
  let date ← parseLocalDate
  match ← peek with
  | some 'T' | some 't' | some ' ' => let _ ← anyChar
  | _ => Parser.fail "expected T or space between date and time"
  let time ← parseLocalTime
  return { date, time, timezone := none }

/-- Check what kind of datetime this is and parse accordingly -/
def parseDateTimeOrDate : Sift.Parser Unit Value := do
  let date ← lookAhead parseLocalDate
  -- Re-parse and check if there's a time component
  let _ ← parseLocalDate
  match ← peek with
  | some 'T' | some 't' | some ' ' =>
    let _ ← anyChar
    let time ← parseLocalTime
    match ← peek with
    | some 'Z' | some 'z' | some '+' | some '-' =>
      -- Has timezone
      let tz ← parseTimezoneOffset
      return .dateTime { date, time, timezone := some tz }
    | _ =>
      -- No timezone, it's a local datetime
      return .localDateTime { date, time, timezone := none }
  | _ =>
    -- Just a date
    return .localDate date

/-- Check if current position looks like a datetime (starts with 4 digits and hyphen) -/
def looksLikeDateTime : Sift.Parser Unit Bool := do
  lookAhead do
    let mut count := 0
    -- Check for 4 digits
    while count < 4 do
      match ← peek with
      | some c =>
        if isDigit c then
          let _ ← anyChar
          count := count + 1
        else
          return false
      | none => return false
    -- Check for hyphen
    return (← peek) == some '-'

end Totem.Parser
