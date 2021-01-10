module ArmFormat exposing (dateString,remainingString)

import Time exposing (Month(..), Weekday(..))
import Time.Extra as Time exposing (Interval(..))

remainingString : Time.Posix -> Time.Posix -> String
remainingString start end =
  let
    previous interval =
      case interval of
        Month -> Just Year
        Day -> Just Month
        Hour  -> Just Day
        Minute  -> Just Hour
        Second  -> Just Minute
        Millisecond -> Just Second
        _ -> Nothing
    toArmInterval interval =
      case interval of
        Year -> Just "տարի"
        Month  -> Just "ամիս"
        Day -> Just "օր"
        Hour  -> Just "ժամ"
        Minute  -> Just "րոպե"
        Second  -> Just "վայրկյան"
        Millisecond -> Just "միլիվայրկյան"
        _ -> Nothing
    appendArmInterval interval string =
      toArmInterval interval
        |> Maybe.map (String.append string)
    remainingInterval interval =
      let
        impl adjustedInterval adjustedStart =
          Time.diff adjustedInterval Time.utc adjustedStart end
            |> maybeFromInt
            |> Maybe.map (\diff -> String.append (String.fromInt diff) " ")
            |> Maybe.andThen (appendArmInterval adjustedInterval)
      in
      case previous interval of
        Nothing -> impl interval start
        Just prev ->
          Time.diff prev Time.utc start end
            |> \diff -> Time.add prev diff Time.utc start
            |> impl interval
  in
    [ Year
    , Month
    , Day
    , Hour
    , Minute
    , Second
    , Millisecond
    ]
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


-- HELPERS
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

maybeFromInt a =
  case a of
    0 -> Nothing
    _ -> Just a
