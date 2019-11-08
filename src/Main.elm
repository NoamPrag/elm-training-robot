module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode
import Robot exposing (Model, Msg, keyDown, keyUp, moveRobot, view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { robot1 : Robot.Model
    , robot2 : Robot.Model
    }


type Msg
    = Robot1Msg Robot.Msg
    | Robot2Msg Robot.Msg
    | KeyDown String
    | KeyUp String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { robot1 =
            { x = 900
            , y = 500
            , upKey = "ArrowUp"
            , downKey = "ArrowDown"
            , rightKey = "ArrowRight"
            , leftKey = "ArrowLeft"
            , color = "blue"
            , speed = 5
            , rotation = 0
            , angle = 0
            , velocity = 0
            }
      , robot2 =
            { x = 100
            , y = 100
            , upKey = "w"
            , downKey = "s"
            , rightKey = "d"
            , leftKey = "a"
            , color = "red"
            , speed = 5
            , rotation = 0
            , angle = 0
            , velocity = 0
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            ( { model
                | robot1 = Robot.moveRobot <| Robot.keyDown key model.robot1
                , robot2 = Robot.moveRobot <| Robot.keyDown key model.robot2
              }
            , Cmd.none
            )

        KeyUp key ->
            ( { model
                | robot1 = Robot.moveRobot <| Robot.keyUp key model.robot1
                , robot2 = Robot.moveRobot <| Robot.keyUp key model.robot2
              }
            , Cmd.none
            )

        default ->
            ( { model
                | robot1 = Robot.moveRobot model.robot1
                , robot2 = Robot.moveRobot model.robot2
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ map Robot1Msg (Robot.view model.robot1)
        , map Robot2Msg (Robot.view model.robot2)
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Json.Decode.map KeyDown <| Json.Decode.field "key" Json.Decode.string)
        , Browser.Events.onKeyUp (Json.Decode.map KeyUp <| Json.Decode.field "key" Json.Decode.string)
        ]
