module Main exposing (..)

import Browser
import Console exposing (ConsoleMsg, Message(..))
import Html exposing (Html, h1, h3, main_, section, text)



-- MODEL


counterMotdMsg : Message Msg
counterMotdMsg =
    Console.argString "Message" <|
        \s ->
            Console.argInt "Counter" <|
                \i ->
                    Console.constructor <|
                        SetCounterAndMotd i s


type alias Model =
    { console : Console.Console Msg
    , counter : Int
    , motd : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (Console.new
            |> Console.addMessage "Set message & counter" counterMotdMsg
            |> Console.addMessage "Set message & counter2" counterMotdMsg
            |> Console.addMessage "Set message & counter3" counterMotdMsg
            |> Console.addMessage "Set message & counter4" counterMotdMsg
            |> Console.addMessage "Set message & counter5" counterMotdMsg
        )
        0
        "Hello"
    , Cmd.none
    )



-- UPDATE


type Msg
    = Increment Int
    | SetCounterAndMotd Int String
    | NoOp
    | ConsoleMsg (Console.ConsoleMsg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Increment v ->
            ( { model | counter = model.counter + v }, Cmd.none )

        SetCounterAndMotd count message ->
            ( { model | counter = count, motd = message }, Cmd.none )

        ConsoleMsg cmsg ->
            let
                ( newConsole, mmsg ) =
                    Console.update cmsg model.console
            in
            case mmsg of
                Nothing ->
                    ( { model | console = newConsole }, Cmd.none )

                Just m ->
                    { model | console = newConsole } |> update m



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ Html.map ConsoleMsg (Console.viewConsole model.console)
        , section []
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
