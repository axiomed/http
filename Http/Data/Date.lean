namespace Http.Data

/-! Definition of the [Date] structure that corresponds to a date that the HTTP/1.1 protocol
    supports.
-/

abbrev Second := Fin 60

abbrev Minute := Fin 60

abbrev Hour := Fin 24

abbrev Day := Fin 32

abbrev Year := { x: Nat // x ≥ 1970 ∧ x ≤ 9999 }

abbrev Month := Fin 12

abbrev Weekday := Fin 7

/-- Epoch in seconds -/
abbrev Epoch := Fin 253402300800

def Weekday.toShortenedStr : Weekday → String
  | 0 => "Mon"
  | 1 => "Tue"
  | 2 => "Wed"
  | 3 => "Thu"
  | 4 => "Fri"
  | 5 => "Sat"
  | 6 => "Sun"

def Weekday.parse? : String → Option Weekday
  | "Mon" => some 0
  | "Tue" => some 1
  | "Wed" => some 2
  | "Thu" => some 3
  | "Fri" => some 4
  | "Sat" => some 5
  | "Sun" => some 6
  | _     => none

def Weekday.toStr : Weekday → String
  | 0 => "Monday"
  | 1 => "Tuesday"
  | 2 => "Wednesday"
  | 3 => "Thursday"
  | 4 => "Friday"
  | 5 => "Saturday"
  | 6 => "Sunday"

def Month.toShortenedStr : Month → String
  | 0 => "Jan"
  | 1 => "Feb"
  | 2 => "Mar"
  | 3 => "Apr"
  | 4 => "May"
  | 5 => "Jun"
  | 6 => "Jul"
  | 7 => "Aug"
  | 8 => "Sep"
  | 9 => "Oct"
  | 10 => "Nov"
  | 11 => "Dec"

structure Date where
  seconds: Second
  minutes: Minute
  hours: Hour
  day: Day
  month: Month
  year: Year
  weekday: Weekday
  deriving Repr

def Date.parseRFC850 (input: String) : Option Date :=
  sorry

def Date.parseFixDate (s: String) : Option Date :=
  sorry

def Date.parseAscTime (s: String) : Option Date :=
  sorry

def Fin.byMod (x: Nat) (y: Nat) (h₁: y > 0 := by decide) : Fin y :=
  ⟨x % y, by simp [Nat.mod_lt x h₁]⟩

def Fin.divMax (x: Fin n) (y: Nat) (div: 0 < y := by decide) (divv: y ∣ n := by decide) : Fin (n/y) := by
  refine ⟨x.val / y, ?_⟩
  let t := Nat.div_lt_iff_lt_mul div (x := x) (y := n/y)
  apply t.mpr
  simp [Nat.div_mul_cancel (m := n) (n := y) divv]

def Date.fromUnixEpoch (secsSinceEpoch: Epoch) : Date := Id.run $ do
  let daySecs : Fin 86400 := Fin.byMod secsSinceEpoch 86400

  let seconds := Fin.byMod daySecs 60
  let minutes := Fin.divMax (Fin.byMod daySecs 3600) 60
  let hours := Fin.divMax daySecs 3600

  let leapYearEpoch := 11017
  let daysPer400Y := 365 * 400 + 97
  let daysPer100Y := 365 * 100 + 24
  let daysPer4Y := 365 * 4 + 1

  let daysSinceEpoch := Fin.divMax secsSinceEpoch 86400

  let days : Int := daysSinceEpoch - leapYearEpoch

  let mut quadracentennialCycles := days / daysPer400Y;

  let mut remDays := days % daysPer400Y;

  if remDays < 0 then
    remDays := remDays + daysPer400Y
    quadracentennialCycles := quadracentennialCycles - 1

  let mut centenialCycles := remDays / daysPer100Y;

  if centenialCycles = 4 then
    centenialCycles := centenialCycles - 1

  remDays := remDays - centenialCycles * daysPer100Y

  let mut quadrennialCycles := remDays / daysPer4Y;

  if quadrennialCycles = 25 then
    quadrennialCycles := quadrennialCycles - 1

  remDays := remDays - quadrennialCycles * daysPer4Y

  let mut remYears := remDays / 365;

  if remYears = 4 then
    remYears := remYears - 1

  remDays := remDays - remYears * 365

  let mut year := 2000 + remYears + 4 * quadrennialCycles + 100 * centenialCycles + 400 * quadracentennialCycles

  let months := [31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 29];

  let mut mon : Fin 12 := 0;

  for monLen in months do
    mon := mon + 1;
    if remDays < monLen then
      break
    remDays := remDays - monLen

  let mday : Fin 32 := Fin.ofNat (Int.toNat $ remDays + 1)

  if mon.val + 2 > 12 then
    year := year + 1
    mon := mon - 10
  else
    mon := mon - 2

  let mut wday := (3 + days) % 7

  let weekday : Fin 7 :=
    Fin.ofNat $ Int.toNat $
      if wday <= 0
        then wday + 7
        else wday

  let myear :=
    if h : year.toNat ≥ 1970 ∧ year.toNat ≤ 9999
      then ⟨year.toNat, h⟩
      else ⟨1970, by decide⟩

  return Date.mk
    seconds
    minutes
    hours
    mday
    mon
    myear
    weekday
