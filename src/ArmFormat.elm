module ArmFormat exposing (dateString,remainingString,TimeSpan(..))

import List.Extra as List
import Maybe.Extra as Maybe
import Time exposing (Month(..), Weekday(..))
import Time.Extra as Time exposing (Interval(..))

remainingString : List TimeSpan -> Time.Posix -> Time.Posix -> String
remainingString timeSpans start end =
  let
    intervals =
      timeSpans
        |> nothingIfEmpty
        |> Maybe.withDefault [Ms]

    previous timeSpan =
      listPrevOf timeSpan timeSpans

    appendArmTimeSpan timeSpan string =
      timeSpan
        |> timeSpanToArm
        |> String.append string

    remainingInterval timeSpan =
      let
        impl adjustedInterval adjustedStart =
          Time.diff (intervalFromTimeSpan adjustedInterval) Time.utc adjustedStart end
            |> nothingIfZero
            |> Maybe.map (String.fromInt >> (\diff -> String.append diff " "))
            |> Maybe.map (appendArmTimeSpan adjustedInterval)
      in
      case previous timeSpan of
        Nothing ->
          impl timeSpan start
        Just prev ->
          Time.diff (intervalFromTimeSpan prev) Time.utc start end
            |> \diff -> Time.add (intervalFromTimeSpan prev) diff Time.utc start
            |> impl timeSpan
  in
    intervals
      |> List.filterMap remainingInterval
      |> String.join ", "


dateString zone time =
  let
    currentYear =
      Time.toYear zone time
        |> String.fromInt
    currentDay =
      Time.toDay zone time
        |> String.fromInt
        |> String.padLeft 2 '0'
    currentWeekday =
      Time.toWeekday zone time
        |> toArmWeekday
    currentMonth =
      Time.toMonth zone time
        |> toArmMonth
    currentTime =
      [ Time.toHour
      , Time.toMinute
      , Time.toSecond
      ]
        |> List.map
          (uncurry
            >> (|>) (zone,time)
            >> String.fromInt
            >> String.padLeft 2 '0'
          )
        |> String.join ":"
    currentMillis =
      Time.toMillis zone time
      |> String.fromInt
      |> String.padLeft 3 '0'
  in
  currentWeekday ++ ", "
    ++ currentDay ++ "/" ++ currentMonth ++ "/" ++ currentYear ++ " "
    ++ currentTime ++ ":" ++ currentMillis

type TimeSpan
  = Yr
  | Mnt
  | Dy
  | Hr
  | Min
  | Sec
  | Ms

-- HELPERS

intervalFromTimeSpan : TimeSpan -> Interval
intervalFromTimeSpan timeSpan =
  case timeSpan of
    Yr -> Year
    Mnt -> Month
    Dy -> Day
    Hr -> Hour
    Min -> Minute
    Sec -> Second
    Ms -> Millisecond

timeSpanToArm timeSpan =
  case timeSpan of
    Yr -> "տարի"
    Mnt -> "ամիս"
    Dy -> "օր"
    Hr -> "ժամ"
    Min -> "րոպե"
    Sec -> "վայրկյան"
    Ms -> "միլիվայրկյան"
 
uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f =
  \(a,b) -> f a b


toArmMonth : Month -> String
toArmMonth month =
  case month of
    Jan -> "հունվար"
    Feb -> "փետրվար"
    Mar -> "մարտ"
    Apr -> "ապրիլ"
    May -> "մայիս"
    Jun -> "հունիս"
    Jul -> "հուլիս"
    Aug -> "օգոստոս"
    Sep -> "սեպտեմբեր"
    Oct -> "հոկտեմբեր"
    Nov -> "նոյեմբեր"
    Dec -> "դեկտեմբեր"

toArmWeekday : Weekday -> String
toArmWeekday weekday =
  case weekday of
    Mon -> "երկուշաբթի"
    Tue -> "երեքշաբթի"
    Wed -> "չորեքշաբթի"
    Thu -> "հինգշաբթի"
    Fri -> "ուրբաթ"
    Sat -> "շաբաթ"
    Sun -> "կիրակի"

nothingIfZero a =
  case a of
    0 -> Nothing
    _ -> Just a

nothingIfEmpty a =
  case a of
    [] -> Nothing
    _ -> Just a

fst (a,_) = a
snd (_,a) = a

listPrevOf : a -> List a -> Maybe a
listPrevOf item list =
  list
    |> List.zip (Nothing :: List.map Just list)
    |> List.find (snd >> (==) item)
    |> Maybe.andThen fst
