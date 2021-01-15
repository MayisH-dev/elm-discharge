module Main exposing (main)


import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import ArmFormat exposing (TimeSpan(..))
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
    labels =
      [ Yr
      , Mnt
      , Dy
      , Hr
      , Min
      , Sec
      , Ms
      ]
    dischargeTime =
      Time.millisToPosix 1627549200000
  in
  div []
    [ p [] [ text <| "Հիմա. " ++ ArmFormat.dateString zone time ]
    , p [] [ text <| "Զորացրում. " ++ ArmFormat.dateString zone dischargeTime ]
    , p [] [ text <| "Մնաց. " ++ ArmFormat.remainingString labels time dischargeTime ]
    -- , div [] 
    --   [ label [] [ text "Տարի", input [ type_ "checkbox" ] [] ]
    --   , label [] [ text "Տարի", input [ type_ "checkbox" ] [] ]
    --   , label [] [ text "Տարի", input [ type_ "checkbox" ] [] ]
    --   , label [] [ text "Տարի", input [ type_ "checkbox" ] [] ]
    --   , label [] [ text "Տարի", input [ type_ "checkbox" ] [] ]
    --   , label [] [ text "Տարի", input [ type_ "checkbox" ] [] ]
    --   ]
    ]
