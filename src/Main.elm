module Main exposing (..)

import Browser
import Console exposing (Message(..))
import Html exposing (Html, h1, h3, main_, section, text)
import Html.Attributes



-- MODEL


type alias Gradient =
    { hue : Float
    , saturation : Float
    , lightness : Float
    , step : Float
    }


setHue : Float -> Gradient -> Gradient
setHue hue gradient =
    { gradient | hue = clamp 0 360 hue }


setStep : Float -> Gradient -> Gradient
setStep step gradient =
    { gradient | step = clamp 0 100 step }


type alias Model =
    { console : Console.Console Msg
    , counter : Int
    , motd : String
    , background : Gradient
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Console.new
            |> Console.addMessage "Set counter & message"
                (Console.constructor2
                    SetCounterAndMotd
                    (Console.argInt "Counter")
                    (Console.argString "Message")
                )
            |> Console.addMessage "Increment counter"
                (Console.constructor1
                    Increment
                    (Console.argInt "Amount")
                )
            |> Console.addMessage "Set Background Hue"
                (Console.constructor1
                    SetBackgroundHue
                    (Console.argFloat "Hue")
                )
            |> Console.addMessage "Set Background Step"
                (Console.constructor1
                    SetBackgroundStep
                    (Console.argFloat "Step")
                )
        )
        0
        "Hello"
        (Gradient 200 75 50 100)
    , Cmd.none
    )



-- UPDATE


type Msg
    = Increment Int
    | SetCounterAndMotd Int String
    | SetBackgroundHue Float
    | SetBackgroundStep Float
    | ConsoleMsg (Console.ConsoleMsg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment v ->
            ( { model | counter = model.counter + v }, Cmd.none )

        SetCounterAndMotd count message ->
            ( { model | counter = count, motd = message }, Cmd.none )

        SetBackgroundHue hue ->
            ( { model | background = setHue hue model.background }, Cmd.none )

        SetBackgroundStep step ->
            ( { model | background = setStep step model.background }, Cmd.none )

        ConsoleMsg cmsg ->
            case Console.update cmsg model.console of
                ( console, Nothing ) ->
                    ( { model | console = console }, Cmd.none )

                ( console, Just m ) ->
                    { model | console = console } |> update m



-- VIEW


gradientString : Gradient -> String
gradientString gradient =
    let
        colorString h s l =
            "hsl("
                ++ String.fromFloat h
                ++ ", "
                ++ String.fromFloat s
                ++ "%, "
                ++ String.fromFloat l
                ++ "%)"
    in
    "linear-gradient(-20deg,"
        ++ colorString gradient.hue gradient.saturation gradient.lightness
        ++ ", "
        ++ colorString (gradient.hue + gradient.step) gradient.saturation gradient.lightness
        ++ ")"


view : Model -> Html Msg
view model =
    main_ []
        [ Html.map ConsoleMsg (Console.viewConsole model.console)
        , section
            [ Html.Attributes.id "app"
            , Html.Attributes.style "background" (gradientString model.background)
            ]
            [ h1 [] [ text <| "Counter: " ++ String.fromInt model.counter ]
            , h3 [] [ text <| "Message: " ++ model.motd ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
