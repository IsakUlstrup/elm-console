module Main exposing (..)

import Browser
import Console exposing (Argument, ArgumentValue(..), Command, CommandValidator, Console, ConsoleMsg(..))
import Element
import Html exposing (Html, div, text)
import Html.Attributes
import View



---- MODEL ----


type alias Model =
    { backgroundColor : String
    , counter : Int
    , flag : Bool
    , console : Console Msg
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        "white"
        0
        False
        (Console.new commandPresets)
    , Cmd.none
    )



---- UPDATE ----


commandPresets : List (Command Msg)
commandPresets =
    [ Console.init "Increment counter" (always <| Ok Inc)
    , Console.init "Decrement counter" (always <| Ok Dec)
    , Console.init "Flag" flagValidator |> Console.addBool "Flag"
    , Console.init "Background color" backgroundColorValidator |> Console.addString "Color"
    , Console.init "Background color" backgroundColorRgbValidator |> Console.addInt "Red" |> Console.addInt "Green" |> Console.addInt "Blue"
    ]


type Msg
    = NoOp
    | Inc
    | Dec
    | SetFlag Bool
    | SetSolidBackground String
    | SetRGBBackground Int Int Int
    | ConsoleMsg (ConsoleMsg Msg)


incrementValdiator : CommandValidator Msg
incrementValdiator _ =
    Ok Inc


backgroundColorValidator : CommandValidator Msg
backgroundColorValidator arguments =
    case arguments of
        [ Console.ArgString (Just color) ] ->
            Ok <| SetSolidBackground color

        _ ->
            Err "CmdBgColor: invalid arguments"


backgroundColorRgbValidator : CommandValidator Msg
backgroundColorRgbValidator arguments =
    case arguments of
        [ Console.ArgInt (Just red), Console.ArgInt (Just green), Console.ArgInt (Just blue) ] ->
            Ok <| SetRGBBackground red green blue

        _ ->
            Err "CmdBgColorRGB: invalid arguments"


flagValidator : CommandValidator Msg
flagValidator arguments =
    case arguments of
        [ Console.ArgBool (Just flag) ] ->
            Ok <| SetFlag flag

        _ ->
            Err "SetFlag: invalid arguments"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Inc ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        Dec ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        SetFlag flag ->
            ( { model | flag = flag }, Cmd.none )

        SetSolidBackground color ->
            ( { model | backgroundColor = color }, Cmd.none )

        SetRGBBackground red green blue ->
            ( { model
                | backgroundColor =
                    "rgb("
                        ++ String.fromInt red
                        ++ ", "
                        ++ String.fromInt green
                        ++ ", "
                        ++ String.fromInt blue
                        ++ ")"
              }
            , Cmd.none
            )

        ConsoleMsg consoleMsg ->
            let
                -- get new console state, a maybe message and a command
                ( console, mmsg, cmd ) =
                    Console.update consoleMsg model.console
            in
            case mmsg of
                Just m ->
                    { model | console = console } |> update m

                Nothing ->
                    ( { model | console = console }, Cmd.map ConsoleMsg cmd )



-- consoleUpdate consoleMsg model
---- VIEW ----


view : Model -> Html Msg
view model =
    div
        []
        [ Html.map ConsoleMsg (Element.layout [] (View.viewConsole model.console))
        , Html.main_ [ Html.Attributes.style "background" model.backgroundColor ]
            [ text ("Counter: " ++ String.fromInt model.counter)
            ]
        ]



---- SUBS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Sub.map ConsoleMsg Console.keyListener
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
