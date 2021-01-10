module Main exposing (main)


import Browser
import Browser.Events
import Html exposing (..)
import ArmFormat
import Task
import Time


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
    [ h1 [] [ text <| "Հիմա: " ++ ArmFormat.dateString zone time ]
    , h1 [] [ text <| "Ուվալնյատ: " ++ ArmFormat.dateString zone uvalnyatTime ]
    , h1 [] [ text <| "Մնաց: " ++ ArmFormat.remainingString time uvalnyatTime ]
    ]
