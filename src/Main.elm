module Main exposing (main)


import Browser
import Browser.Events
import Html exposing (..)
import Task
import Time exposing (Month(..), Weekday(..))
import Time.Extra as Time exposing (Interval(..))



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0)
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onAnimationFrame Tick


-- VIEW


view : Model -> Html Msg
view {time,zone} =
  let 
    uvalnyatTime =
      Time.millisToPosix 1627549200000 
  in
  div []
    [ h1 [] [ text <| "Հիմա " ++ dateString zone time ]
    , h1 [] [ text <| "Ուվալնյատ " ++ dateString zone uvalnyatTime ]
    , h1 [] [ text <| "Mnac " ++ remainingString time uvalnyatTime ]
    ]

-- HELPERS
uncurry : (a -> b -> c) -> (a, b) -> c
uncurry f = 
  \(a,b) -> f a b

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

remainingString : Time.Posix -> Time.Posix -> String
remainingString start end = 
  let
    previous interval =
      case interval of
        Month -> "Amis"
        Day -> "Or"
        Hour  -> "Jam"
        Minute  -> "Rope"
        Second  -> "Vayrkyan"
        Millisecond -> "Milivayrkyan"
        _ -> "Never"
    toArmInterval interval =
      case interval of
        Year -> "Tari"
        Month  -> "Amis"
        Day -> "Or"
        Hour  -> "Jam"
        Minute  -> "Rope"
        Second  -> "Vayrkyan"
        Millisecond -> "Milivayrkyan"
        _ -> "Never"
    remainingInterval interval =
      Time.diff interval Time.utc (Time.ceiling interval Time.utc start) (Time.floor interval Time.utc end)
        |> maybeFromInt
        |> Maybe.map (String.fromInt)
        |> Maybe.map (\diff -> diff ++ " " ++ toArmInterval interval)
        |> Maybe.withDefault ""
    remainingYear =
      remainingInterval Year
    remainingDay = 
      remainingInterval Day
    remainingMonth =
      remainingInterval Month 
    remainingHour =
      remainingInterval Hour 
    remainingMinute = 
      remainingInterval Minute 
    remainingSecond =
      remainingInterval Second 
    remainingMillis =
      remainingInterval Millisecond
  in
    [ remainingYear
    , remainingMonth
    , remainingDay
    , remainingHour
    , remainingMinute
    , remainingSecond
    , remainingMillis
    ]
      |> List.filter (String.isEmpty >> not)
      |> String.join ", " 
    
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
