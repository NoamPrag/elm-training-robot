module Counter exposing (Model, Msg, init, update, view)

import Browser
import Browser.Events
import Html exposing (Attribute, Html, div, input, text)
import Html.Events exposing (onClick, onInput)
import Json.Decode
import String exposing (dropRight, length, toFloat, toInt)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { value : String, secondNumber : Float }


type alias Flags =
    ()


type Msg
    = ChangedAddText String
    | KeyDown String
    | Add
    | Subtract
    | Multiply
    | Divide
    | Power
    | Sqrt


parseNumber : String -> Float
parseNumber text =
    let
        theMaybe =
            toFloat text
    in
    case theMaybe of
        Just val ->
            val

        Nothing ->
            0


typeNumber : String -> String -> String
typeNumber value key =
    let
        numberToAdd =
            toInt key
    in
    if key == "Backspace" then
        if length value == 1 then
            "0"

        else
            dropRight 1 value

    else if key == "Enter" then
        "0"

    else if key == "." && Maybe.withDefault 0.5 (toFloat value) / 1 == 0 then
        value ++ "."

    else
        case numberToAdd of
            Nothing ->
                value

            Just number ->
                if not (value == "0") then
                    value ++ String.fromInt number

                else
                    String.fromInt number


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { value = "0", secondNumber = 0 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown (Json.Decode.map KeyDown <| Json.Decode.field "key" Json.Decode.string)


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text model.value
        , div [] []
        , Html.div [] [ Html.input [ onInput ChangedAddText ] [] ]
        , Html.div []
            [ Html.button [ onClick Add ] [ Html.text "+" ]
            , Html.button [ onClick Subtract ] [ Html.text "-" ]
            , Html.button [ onClick Multiply ] [ Html.text "X" ]
            , Html.button [ onClick Divide ] [ Html.text "/" ]
            , Html.button [ onClick Power ] [ Html.text "^" ]
            , Html.button [ onClick Sqrt ] [ Html.text "v-" ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "key Name" msg
    in
    case msg of
        ChangedAddText textThatWasTyped ->
            ( { model | secondNumber = parseNumber textThatWasTyped }, Cmd.none )

        Add ->
            ( { model | value = String.fromFloat (parseNumber model.value + model.secondNumber) }, Cmd.none )

        Subtract ->
            ( { model | value = String.fromFloat (parseNumber model.value - model.secondNumber) }, Cmd.none )

        Multiply ->
            ( { model | value = String.fromFloat (parseNumber model.value * model.secondNumber) }, Cmd.none )

        Divide ->
            ( { model | value = String.fromFloat (parseNumber model.value / model.secondNumber) }, Cmd.none )

        Power ->
            ( { model | value = String.fromFloat (parseNumber model.value ^ model.secondNumber) }, Cmd.none )

        Sqrt ->
            ( { model | value = String.fromFloat (sqrt (parseNumber model.value)) }, Cmd.none )

        KeyDown keycode ->
            case keycode of
                "+" ->
                    ( { model | value = String.fromFloat (parseNumber model.value + model.secondNumber) }, Cmd.none )

                "-" ->
                    ( { model | value = String.fromFloat (parseNumber model.value - model.secondNumber) }, Cmd.none )

                "*" ->
                    ( { model | value = String.fromFloat (parseNumber model.value * model.secondNumber) }, Cmd.none )

                "/" ->
                    ( { model | value = String.fromFloat (parseNumber model.value / model.secondNumber) }, Cmd.none )

                default ->
                    ( { model | value = typeNumber model.value keycode }, Cmd.none )
