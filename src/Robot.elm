module Robot exposing (Model, Msg, keyDown, keyUp, moveRobot, view)

import Browser
import Browser.Events
import Html
import Html.Attributes as Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode
import Time


type alias Model =
    { x : Int
    , y : Int
    , upKey : String
    , downKey : String
    , rightKey : String
    , leftKey : String
    , speed : Float
    , angle : Int
    , color : String
    , velocity : Float
    , rotation : Float
    }


type Msg
    = KeyDown String
    | KeyUp String


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Json.Decode.map KeyDown <| Json.Decode.field "key" Json.Decode.string)
        , Browser.Events.onKeyUp (Json.Decode.map KeyUp <| Json.Decode.field "key" Json.Decode.string)
        ]


moveRobot : Model -> Model
moveRobot model =
    { model
        | x = model.x + round (sin (degrees (toFloat model.angle)) * (model.speed * model.velocity))
        , y = model.y - round (cos (degrees (toFloat model.angle)) * (model.speed * model.velocity))
        , angle = modBy 360 (model.angle + (round <| model.speed * model.rotation))
    }


keyDown : String -> Model -> Model
keyDown key model =
    if key == model.upKey then
        { model | velocity = 1 }

    else if key == model.downKey then
        { model | velocity = -1 }

    else if key == model.rightKey then
        { model | rotation = 1 }

    else if key == model.leftKey then
        { model | rotation = -1 }

    else
        model


keyUp : String -> Model -> Model
keyUp key model =
    if key == model.upKey || key == model.downKey then
        { model | velocity = 0 }

    else if key == model.rightKey || key == model.leftKey then
        { model | rotation = 0 }

    else
        model


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div
            [ style "background-color" model.color
            , style "color" "yellow"
            , style "width" "30px"
            , style "height" "30px"
            , style "text-align" "center"
            , style "position" "absolute"
            , style "left" (String.fromInt model.x ++ "px")
            , style "top" (String.fromInt model.y ++ "px")
            , style "transform" ("rotate(" ++ (String.fromInt model.angle ++ "deg)"))
            ]
            [ Html.text "↑" ]
        ]
