module Main exposing (main)


import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time
import List.Extra as List


import ArmFormat exposing (TimeSpan(..))


-- MAIN


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , timeSpans : List TimeSpan
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) [ Yr , Mnt , Dy , Hr , Min , Sec , Ms ]
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | Select TimeSpan
  | Deselect TimeSpan



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

    Select timeSpan ->
      ( { model
        | timeSpans =
            timeSpan :: model.timeSpans
              |> List.sortBy ArmFormat.timeSpanToInt
              |> List.uniqueBy ArmFormat.timeSpanToInt
        }
      , Cmd.none
      )
    
    Deselect timeSpan ->
      ( { model
        | timeSpans =
            model.timeSpans
              |> List.remove timeSpan
        }
      , Cmd.none
      )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onAnimationFrame Tick


-- VIEW


view : Model -> Browser.Document Msg
view {time,zone,timeSpans} =
  let
    allTimeSpans =
      [ Yr , Mnt , Dy , Hr , Min , Sec , Ms ]
    dischargeTime =
      Time.millisToPosix 1627549200000
    body =
      [ p [] [ text <| "Հիմա. " ++ ArmFormat.dateString zone time ]
      , p [] [ text <| "Զորացրում. " ++ ArmFormat.dateString zone dischargeTime ]
      , fieldset []
          <| legend [] [ text "Ժամանակացույց" ]
            :: List.map2 viewTimeSpanToggle allTimeSpans (List.map (\x -> List.member x timeSpans) allTimeSpans)
      , p [] [ text <| "Մնաց. " ++ ArmFormat.remainingString timeSpans time dischargeTime ]
      ]
    title =
      "Զորացրում"
  in
    Browser.Document title body


-- HELPERS

viewTimeSpanToggle : TimeSpan -> Bool -> Html Msg
viewTimeSpanToggle timeSpan isChecked =
  let
      command checkState =
        case checkState of
          True ->
            Select timeSpan

          False ->
            Deselect timeSpan

  in
    label [] [ input [ onCheck command, checked isChecked, type_ "checkbox" ] [], text <| ArmFormat.timeSpanToString timeSpan ]
