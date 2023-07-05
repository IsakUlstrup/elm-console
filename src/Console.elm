module Console exposing
    ( ArgumentInput
    , Console
    , ConsoleInput
    , ConsoleMsg
    , Message(..)
    , addMessage
    , argBool
    , argFloat
    , argInt
    , argString
    , constructor1
    , constructor2
    , constructor3
    , new
    , update
    , viewConsole
    )

import Dict exposing (Dict)
import Html exposing (Attribute, Html, aside, div, form, hr, input, label, li, p, small, strong, text, ul)
import Html.Attributes exposing (autocomplete, autofocus, class, for, id, placeholder, required, step, style, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onMouseDown, onSubmit)


type alias ArgumentInput =
    { name : String
    , value : String
    }


type Message a
    = ArgInt ArgumentInput (Int -> Message a)
    | ArgFloat ArgumentInput (Float -> Message a)
    | ArgBool ArgumentInput (Bool -> Message a)
    | ArgString ArgumentInput (String -> Message a)
    | Constructor a


constructor : a -> Message a
constructor c =
    Constructor c


constructor1 : (a -> b) -> ((a -> Message b) -> c) -> c
constructor1 c x =
    x <| \a -> constructor <| c a


constructor2 : (a -> b -> c) -> ((a -> d) -> e) -> ((b -> Message c) -> d) -> e
constructor2 c x y =
    x <| \a -> y <| \b -> constructor <| c a b


constructor3 : (a -> b -> c -> d) -> ((a -> e) -> f) -> ((b -> g) -> e) -> ((c -> Message d) -> g) -> f
constructor3 c x y z =
    x <| \a -> y <| \b -> z <| \d -> constructor <| c a b d


argInt : String -> (Int -> Message a) -> Message a
argInt name x =
    ArgInt (ArgumentInput name "") x


argFloat : String -> (Float -> Message a) -> Message a
argFloat name x =
    ArgFloat (ArgumentInput name "") x


argBool : String -> (Bool -> Message a) -> Message a
argBool name =
    ArgBool (ArgumentInput name "")


argString : String -> (String -> Message a) -> Message a
argString name =
    ArgString (ArgumentInput name "")


type ConsoleInput a
    = Filter String
    | MessagePreset String (Message a)


type alias Console a =
    { input : ConsoleInput a
    , messages : Dict String (Message a)
    , messageHistory : List ( String, Message a )
    , showPresets : Bool
    }


new : Console a
new =
    Console (Filter "") Dict.empty [] False


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


parseFloat : String -> Result String Float
parseFloat f =
    String.toFloat f |> Result.fromMaybe "Invalid float"



-- UPDATE


setInput : ArgumentInput -> Message msg -> Message msg
setInput input args =
    case args of
        ArgInt i m ->
            if i.name == input.name then
                ArgInt { i | value = input.value } m

            else
                ArgInt i <| \x -> setInput input (m x)

        ArgFloat i m ->
            if i.name == input.name then
                ArgFloat { i | value = input.value } m

            else
                ArgFloat i <| \x -> setInput input (m x)

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
    | ShowPresets Bool
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

        ShowPresets flag ->
            ( { console | showPresets = flag }, Nothing )

        ExecMsg ->
            case console.input of
                MessagePreset name m ->
                    case construct m of
                        Ok mm ->
                            ( { console
                                | messageHistory = ( name, m ) :: console.messageHistory |> List.take 5
                                , input = Filter ""
                                , showPresets = False
                              }
                            , Just mm
                            )

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

        ArgFloat n k ->
            Result.andThen
                (\x -> construct (k x))
                (parseFloat n.value)

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


stringFromBool : Bool -> String
stringFromBool b =
    if b then
        "true"

    else
        "false"



-- VIEW


viewInput : Int -> String -> List (Attribute (ConsoleMsg a)) -> ArgumentInput -> Html (ConsoleMsg a)
viewInput index type_ attrs input_ =
    div [ class "argument", class <| String.toLower type_ ]
        [ label [ for input_.name ] [ text <| input_.name ++ " (" ++ type_ ++ ")" ]
        , input
            ([ value input_.value
             , id input_.name
             , required True
             , autocomplete False
             , autofocus <| index == 0
             , onInput <| \i -> UpdateMessage { input_ | value = i }
             ]
                ++ attrs
            )
            []
        ]


viewCommand : Int -> List (Html (ConsoleMsg a)) -> Message msg -> List (Html (ConsoleMsg a))
viewCommand index a g =
    case g of
        ArgInt i k ->
            viewCommand (index + 1)
                (viewInput index "Int" [ type_ "number" ] i :: a)
                (k 0)

        ArgFloat i k ->
            viewCommand (index + 1)
                (viewInput index
                    "Float"
                    [ type_ "number"
                    , step "any"
                    ]
                    i
                    :: a
                )
                (k 0)

        ArgBool i k ->
            viewCommand (index + 1)
                (viewInput index "Bool" [] i :: a)
                (k False)

        ArgString i k ->
            viewCommand (index + 1)
                (viewInput index "String" [] i :: a)
                (k "")

        Constructor _ ->
            a


viewArguments : List (Html (ConsoleMsg a)) -> Message msg -> List (Html (ConsoleMsg a))
viewArguments a g =
    case g of
        ArgInt i k ->
            case parseInt i.value of
                Ok int ->
                    viewArguments
                        ((text <| i.name ++ ": " ++ String.fromInt int) :: a)
                        (k int)

                Err _ ->
                    viewArguments
                        ((text <| i.name ++ "(Int)") :: a)
                        (k 0)

        ArgFloat i k ->
            case parseFloat i.value of
                Ok float ->
                    viewArguments
                        ((text <| i.name ++ ": " ++ String.fromFloat float) :: a)
                        (k float)

                Err _ ->
                    viewArguments
                        ((text <| i.name ++ "(Float)") :: a)
                        (k 0)

        ArgBool i k ->
            case parseBool i.value of
                Ok bool ->
                    viewArguments
                        ((text <| i.name ++ ": " ++ stringFromBool bool) :: a)
                        (k bool)

                Err _ ->
                    viewArguments
                        ((text <| i.name ++ "(Bool)") :: a)
                        (k False)

        ArgString i k ->
            case parseString i.value of
                Ok string ->
                    viewArguments
                        ((text <| i.name ++ ": " ++ string) :: a)
                        (k string)

                Err _ ->
                    viewArguments
                        ((text <| i.name ++ "(String)") :: a)
                        (k "")

        Constructor _ ->
            a


viewMessagePreset : String -> ( String, Message a ) -> Html (ConsoleMsg a)
viewMessagePreset filter ( n, m ) =
    li [ onMouseDown <| SetMessage n m ]
        [ p []
            [ strong [] [ text filter ]
            , text <|
                if not <| String.isEmpty filter then
                    String.split (String.toLower filter) (String.toLower n) |> List.drop 1 |> String.concat

                else
                    n
            ]
        , small []
            (viewArguments [] m
                |> List.reverse
                |> List.intersperse (text " > ")
            )
        ]


filterPass : String -> String -> v -> Bool
filterPass filter name _ =
    String.contains (String.toLower filter) (String.toLower name)



-- STYLE


type alias Property =
    ( String, String )


type alias Selector =
    ( String, List Property )


selectors : List Selector
selectors =
    [ ( ".console"
      , [ ( "padding", "0.5rem" )
        , ( "display", "flex" )
        , ( "gap", "0.5rem" )
        , ( "font-family", "monospace" )
        , ( "font-size", "16px" )
        , ( "color", "#262626" )
        , ( "background-color", "rgba(200, 200, 200, 0.5)" )
        ]
      )
    , ( ".message-preset"
      , [ ( "flex-wrap", "wrap" )
        ]
      )
    , ( ".console hr"
      , [ ( "border-top", "1px solid grey" ) ]
      )
    , ( ".console input"
      , [ ( "padding", "0.5rem" ) ]
      )
    , ( ".console input[type=search]"
      , [ ( "flex", "auto" ) ]
      )
    , ( ".console .filter-input"
      , [ ( "width", "100%" )
        , ( "display", "flex" )
        , ( "gap", "0.5rem" )
        ]
      )
    , ( ".console .message-presets"
      , [ ( "position", "absolute" )
        , ( "top", "3rem" )
        , ( "left", "0.5rem" )
        , ( "right", "0.5rem" )
        , ( "list-style", "none" )
        , ( "background-color", "rgba(200, 200, 200, 0.5)" )
        ]
      )
    , ( ".console .message-presets ul"
      , [ ( "list-style", "none" )
        ]
      )
    , ( ".console .message-presets ul li"
      , [ ( "padding", "0.5rem" )
        ]
      )
    , ( ".console .message-presets ul li:hover"
      , [ ( "background", "rgba(255, 255, 255, 0.7)" )
        , ( "cursor", "pointer" )
        ]
      )
    ]


propertyString : Property -> String
propertyString ( property, value ) =
    property ++ ": " ++ value ++ ";"


selectorString : Selector -> String
selectorString ( selector, properties ) =
    selector ++ " {" ++ (properties |> List.map propertyString |> List.intersperse "\n" |> String.concat) ++ "}"


styleNode : List Selector -> Html msg
styleNode cs =
    Html.node "style"
        []
        (List.map (selectorString >> Html.text) cs)


viewConsole : Console msg -> Html (ConsoleMsg msg)
viewConsole console =
    aside
        [ class "console-container"
        , style "position" "absolute"
        , style "left" "0"
        , style "right" "0"
        ]
        [ styleNode selectors
        , case console.input of
            MessagePreset n m ->
                form [ class "message-preset", class "console", onSubmit ExecMsg ]
                    ([ input [ onClick <| SetFilter "", type_ "button", value "<" ] []
                     , p [] [ text n ]
                     ]
                        ++ (viewCommand 0 [] m |> List.reverse)
                        ++ [ input [ type_ "submit", value "Go!" ] [] ]
                    )

            Filter f ->
                form [ class "filter", class "console" ]
                    [ div [ class "filter-input" ]
                        [ input
                            [ onInput <| SetFilter
                            , value f
                            , type_ "search"
                            , onFocus <| ShowPresets True
                            , onBlur <| ShowPresets False
                            , placeholder "Filter messages"
                            ]
                            []
                        ]
                    , div [ class "message-presets" ] <|
                        if console.showPresets then
                            [ ul [ class "history" ] (List.map (viewMessagePreset f) console.messageHistory)
                            , hr [] []
                            , ul [ class "filter-matches" ] (List.map (viewMessagePreset f) (console.messages |> Dict.filter (filterPass f) |> Dict.toList))
                            ]

                        else
                            []
                    ]
        ]
