module Counter exposing (Model, Msg, init, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Events exposing (onClick)

main = Browser.sandbox {init = init, update = update, view = view}

init = 0

type Msg = Increment | Decrement | MultiplyByTwo | DivideByTwo | Square | Sqrt

type alias Model = Float

update : Msg -> Model -> Model
update msg model = 
    case msg of
        Increment -> model + 1
        Decrement -> model - 1
        MultiplyByTwo -> model * 2
        DivideByTwo -> model / 2
        Square -> model ^ 2
        Sqrt -> sqrt model

view : Model -> Html.Html Msg
view model = 
    div [] 
        [ button [onClick Increment] [Html.text "+"]
        , button [onClick Decrement] [Html.text "-"]
        , button [onClick MultiplyByTwo] [Html.text "X2"]
        , button [onClick DivideByTwo] [Html.text "/2"]
        , button [onClick Square] [Html.text "^2"]
        , button [onClick Sqrt] [Html.text "v-"]
        , div [] [Html.text (String.fromFloat model)]
        ]