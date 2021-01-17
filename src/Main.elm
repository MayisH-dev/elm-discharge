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
import Ports


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
  , discharges : List { date : Time.Posix, name : String, hasGithub : Bool }
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model
      Time.utc
      (Time.millisToPosix 0)
      [ Yr , Mnt , Dy , Hr , Min , Sec , Ms ]
      [ { date = Time.millisToPosix 1627549200000
        , name = "MayisH-dev"
        , hasGithub = True
        }
      ]
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
view { time, zone, timeSpans, discharges } =
  let
    viewDischarge { date, name, hasGithub } =
      div []
        [ h3 []
           ( if hasGithub then
              [ a [ href <| "https://github.com/" ++ name ] [ text name ] ]

            else
              [ text name ]
           )
        , p [] [ text <| "Զորացրում. " ++ ArmFormat.dateString zone date ]
        , p [] [ text <| "Մնաց. " ++ ArmFormat.remainingString timeSpans zone time date ]
        ]

    body =
      [ h1 [] [ text <| "Հիմա. " ++ ArmFormat.dateString zone time ]
      , fieldset [] <| legend [
          [ text "Ժամանակացույց" ]
            :: viewTimeSpanToggles timeSpans
      , h2 [] [ text "Զորացրումներ" ]
      , div []  <| List.map viewDischarge discharges
      ]
    title =
      discharges
        |> List.head
        |> Maybe.map
          ( .date
            >> ArmFormat.remainingString [Dy,Hr] zone time
            >> String.append "Մնաց։ "
          )
        |> Maybe.withDefault "Զորացրում"
  in
    Browser.Document title body


-- HELPERS

viewTimeSpanToggles : List TimeSpan -> List (Html Msg)
viewTimeSpanToggles timeSpans =
  List.map2 viewTimeSpanToggle allTimeSpans (List.map (\x -> List.member x timeSpans) allTimeSpans)



viewTimeSpanToggle : TimeSpan -> Bool -> Html Msg
viewTimeSpanToggle timeSpan isChecked =
  let
      command checkState =
        if checkState then
          Select timeSpan

        else
          Deselect timeSpan

  in
    label []
      [ input [ onCheck command, checked isChecked, type_ "checkbox" ] []
      , text <| ArmFormat.timeSpanToString timeSpan
      ]

allTimeSpans =
  [ Yr , Mnt , Dy , Hr , Min , Sec , Ms ]
