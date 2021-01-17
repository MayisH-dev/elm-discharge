module ArmFormat exposing (dateString,remainingString,timeSpanToString,timeSpanToInt,TimeSpan(..))

import Tuple2 as Tuple
import List.Extra as List
import Maybe.Extra as Maybe
import Time exposing (Month(..), Weekday(..))
import Time.Extra as Time exposing (Interval(..))

remainingString : List TimeSpan -> Time.Zone -> Time.Posix -> Time.Posix -> String
remainingString timeSpans timeZone start end =
  timeSpans
    |> List.zip (Nothing :: List.map (intervalFromTimeSpan >> Just) timeSpans)
    |> List.map
        ( \(prev,ts) ->
            prev
              |> Maybe.map
                (\p ->
                  Time.diff p timeZone start end
                    |> \diff -> Time.add p diff timeZone start
                )
              |> Maybe.withDefault start
              |> \s -> Time.diff (intervalFromTimeSpan ts) timeZone s end
              |> Tuple.pair ts
        )
    |> List.filter (Tuple.second >> (/=) 0)
    |> List.map
        ( Tuple.mapBoth timeSpanToString String.fromInt
          >> \(t,r) -> r ++ " " ++ t
        )
    |> String.join ", "

dateString : Time.Zone -> Time.Posix -> String
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
          ( Tuple.uncurry
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

timeSpanToInt : TimeSpan -> Int
timeSpanToInt timeSpan =
  case timeSpan of
    Yr -> 0
    Mnt -> 1
    Dy -> 2
    Hr -> 3
    Min -> 4
    Sec -> 5
    Ms -> 6

timeSpanToString timeSpan =
  case timeSpan of
    Yr -> "տարի"
    Mnt -> "ամիս"
    Dy -> "օր"
    Hr -> "ժամ"
    Min -> "րոպե"
    Sec -> "վայրկյան"
    Ms -> "միլիվայրկյան"


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
