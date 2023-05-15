module Console exposing
    ( ArgumentInput
    , Console
    , ConsoleInput
    , ConsoleMsg
    , Message(..)
    , addMessage
    , argInt
    , argString
    , constructor1
    , constructor2
    , new
    , update
    , viewConsole
    )

import Dict exposing (Dict)
import Html exposing (Html, aside, div, form, input, label, li, p, text, ul)
import Html.Attributes exposing (autocomplete, class, for, id, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


type alias ArgumentInput =
    { name : String
    , value : String
    }


type Message a
    = ArgInt ArgumentInput (Int -> Message a)
    | ArgBool ArgumentInput (Bool -> Message a)
    | ArgString ArgumentInput (String -> Message a)
    | Constructor a


constructor : a -> Message a
constructor c =
    Constructor c


constructor2 : (a -> b -> c) -> ((a -> d) -> e) -> ((b -> Message c) -> d) -> e
constructor2 c x y =
    x <| \a -> y <| \b -> constructor <| c a b


constructor1 : (a -> b) -> ((a -> Message b) -> c) -> c
constructor1 c x =
    x <| \a -> constructor <| c a


argInt : String -> (Int -> Message a) -> Message a
argInt name x =
    ArgInt (ArgumentInput name "") x


argString : String -> (String -> Message a) -> Message a
argString name =
    ArgString (ArgumentInput name "")


type ConsoleInput a
    = Filter String
    | MessagePreset String (Message a)


type alias Console a =
    { input : ConsoleInput a
    , messages : Dict String (Message a)
    }


new : Console a
new =
    Console (Filter "") Dict.empty


addMessage : String -> Message a -> Console a -> Console a
addMessage name msg console =
    { console | messages = Dict.insert name msg console.messages }


parseBool : String -> Result String Bool
parseBool b =
    case String.toLower b of
        "true" ->
            Ok True

        "false" ->
            Ok False

        _ ->
            Err "invalid boolean"


parseString : String -> Result String String
parseString s =
    case s of
        "" ->
            Err "Empty string is not allowed"

        _ ->
            Ok s


parseInt : String -> Result String Int
parseInt i =
    String.toInt i |> Result.fromMaybe "Invalid integer"



-- parseFloat : String -> Result String Float
-- parseFloat f =
--     String.toFloat f |> Result.fromMaybe "Invalid float"
-- UPDATE


setInput : ArgumentInput -> Message msg -> Message msg
setInput input args =
    case args of
        ArgInt i m ->
            if i.name == input.name then
                ArgInt { i | value = input.value } m

            else
                ArgInt i <| \x -> setInput input (m x)

        ArgBool i m ->
            if i.name == input.name then
                ArgBool { i | value = input.value } m

            else
                ArgBool i <| \x -> setInput input (m x)

        ArgString i m ->
            if i.name == input.name then
                ArgString { i | value = input.value } m

            else
                ArgString i <| \x -> setInput input (m x)

        Constructor _ ->
            args


type ConsoleMsg a
    = UpdateMessage ArgumentInput
    | SetFilter String
    | SetMessage String (Message a)
    | ExecMsg


update : ConsoleMsg msg -> Console msg -> ( Console msg, Maybe msg )
update msg console =
    case msg of
        UpdateMessage input ->
            case console.input of
                MessagePreset n m ->
                    ( { console | input = MessagePreset n <| setInput input m }, Nothing )

                _ ->
                    ( console, Nothing )

        SetFilter filter ->
            ( { console | input = Filter filter }, Nothing )

        SetMessage n m ->
            ( { console | input = MessagePreset n m }, Nothing )

        ExecMsg ->
            case console.input of
                MessagePreset _ m ->
                    case construct m of
                        Ok mm ->
                            ( console, Just mm )

                        Err _ ->
                            ( console, Nothing )

                _ ->
                    ( console, Nothing )



-- TODO: better error messages


construct : Message a -> Result String a
construct msg =
    case msg of
        ArgInt n k ->
            Result.andThen
                (\x -> construct (k x))
                (parseInt n.value)

        ArgBool b k ->
            Result.andThen
                (\x -> construct (k x))
                (parseBool b.value)

        ArgString s k ->
            Result.andThen
                (\x -> construct (k x))
                (parseString s.value)

        Constructor a ->
            Ok a



-- stringFromBool : Bool -> String
-- stringFromBool b =
--     if b then
--         "true"
--     else
--         "false"
-- VIEW


viewStringInput : ArgumentInput -> Html (ConsoleMsg a)
viewStringInput input_ =
    div [ class "argument", class "string" ]
        [ label [ for input_.name ] [ text <| input_.name ++ " (String)" ]
        , input
            [ value input_.value
            , id input_.name
            , required True
            , autocomplete False
            , onInput <| \i -> UpdateMessage { input_ | value = i }
            ]
            []
        ]


viewIntInput : ArgumentInput -> Html (ConsoleMsg a)
viewIntInput input_ =
    div [ class "argument", class "int" ]
        [ label [ for input_.name ] [ text <| input_.name ++ " (Integer)" ]
        , input
            [ value input_.value
            , id input_.name
            , required True
            , type_ "number"
            , autocomplete False
            , onInput <| \i -> UpdateMessage { input_ | value = i }
            ]
            []
        ]


viewBoolInput : ArgumentInput -> Html (ConsoleMsg a)
viewBoolInput input_ =
    div [ class "argument", class "bool" ]
        [ label [ for input_.name ] [ text <| input_.name ++ " (Boolean)" ]
        , input
            [ value input_.value
            , id input_.name
            , required True
            , autocomplete False
            , onInput <| \i -> UpdateMessage { input_ | value = i }
            ]
            []
        ]


viewCommand : List (Html (ConsoleMsg a)) -> Message msg -> List (Html (ConsoleMsg a))
viewCommand a g =
    case g of
        ArgInt i k ->
            viewCommand
                (viewIntInput i :: a)
                (k 0)

        ArgBool i k ->
            viewCommand
                (viewBoolInput i :: a)
                (k False)

        ArgString i k ->
            viewCommand
                (viewStringInput i :: a)
                (k "")

        Constructor _ ->
            a


viewArguments : List (Html (ConsoleMsg a)) -> Message msg -> List (Html (ConsoleMsg a))
viewArguments a g =
    case g of
        ArgInt i k ->
            viewArguments
                ((text <| i.name ++ "(Int)") :: a)
                (k 0)

        ArgBool i k ->
            viewArguments
                ((text <| i.name ++ "(Bool)") :: a)
                (k False)

        ArgString i k ->
            viewArguments
                ((text <| i.name ++ "(String)") :: a)
                (k "")

        Constructor _ ->
            a


viewMessagePreset : ( String, Message a ) -> Html (ConsoleMsg a)
viewMessagePreset ( n, m ) =
    li [ onClick <| SetMessage n m ] [ text n, p [] (viewArguments [] m |> List.reverse |> List.intersperse (text " > ")) ]


filterPass : String -> String -> v -> Bool
filterPass filter name _ =
    String.contains (String.toLower filter) (String.toLower name)


viewConsole : Console msg -> Html (ConsoleMsg msg)
viewConsole console =
    aside [ class "console" ]
        [ case console.input of
            MessagePreset n m ->
                form [ class "message-preset", onSubmit ExecMsg ]
                    ([ input [ onClick <| SetFilter "", type_ "button", value "<" ] []
                     , p [] [ text n ]
                     ]
                        ++ (viewCommand [] m |> List.reverse)
                        ++ [ input [ type_ "submit", value "Go!" ] [] ]
                    )

            Filter f ->
                form [ class "filter" ]
                    [ input [ onInput <| SetFilter, value f ] []
                    , ul [ class "filter-matches" ] (List.map viewMessagePreset (console.messages |> Dict.filter (filterPass f) |> Dict.toList))
                    ]
        ]
