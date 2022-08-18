module View exposing (viewConsole)

import Console exposing (Argument(..), Command, Console, ConsoleMsg(..))
import Element exposing (Element, below, column, el, fill, height, mouseOver, none, padding, pointer, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onFocus)
import Element.Font as Font
import Element.Input as Input
import Element.Region exposing (aside)
import Html.Attributes exposing (id, type_)
import Html.Events
import Json.Decode as Decode



---- HELPERS ----


sizing :
    { small : Int
    , medium : Int
    , large : Int
    , xlarge : Int
    }
sizing =
    { small = 5
    , medium = 10
    , large = 20
    , xlarge = 40
    }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


button : String -> ConsoleMsg cmd -> Element (ConsoleMsg cmd)
button label event =
    Input.button
        [ padding sizing.medium
        , Border.color <| rgb255 0 0 0
        , Border.solid
        , Border.rounded 3
        , height fill
        , Background.color <| rgb255 200 200 200
        ]
        { onPress = Just <| event
        , label = text label
        }



---- VIEW ----


viewCommandFilter : Console cmd -> Element (ConsoleMsg cmd)
viewCommandFilter console =
    row [ width fill, height fill ]
        [ button "x" HidePresets
        , Input.text [ onFocus ShowPresets, height fill ]
            { onChange = SetCommandFilter
            , text = console.commandFilter
            , placeholder = Just <| Input.placeholder [] (text "Filter")
            , label = Input.labelHidden "Command filter"
            }
        ]


viewArgInput : Int -> Argument -> Element (ConsoleMsg cmd)
viewArgInput index arg =
    let
        boolString b =
            if b then
                "True"

            else
                "False"

        elementId =
            if index == 0 then
                Element.htmlAttribute <| id Console.firstArgId

            else
                Element.htmlAttribute <| id (String.fromInt index)

        ( label, type_, value ) =
            case arg of
                ArgInt l v ->
                    ( l, "Int", v |> Maybe.map String.fromInt )

                ArgBool l v ->
                    ( l, "Bool", v |> Maybe.map boolString )

                ArgString l v ->
                    ( l, "String", v )
    in
    Input.text [ width <| px 100, elementId ]
        { onChange = UpdateArgument index
        , text = Maybe.withDefault "" value
        , placeholder = Just <| Input.placeholder [] (text type_)
        , label = Input.labelLeft [] (text label)
        }


viewArg : Argument -> Element (ConsoleMsg cmd)
viewArg arg =
    let
        ( label, type_ ) =
            case arg of
                ArgInt l _ ->
                    ( l, "Int" )

                ArgBool l _ ->
                    ( l, "Bool" )

                ArgString l _ ->
                    ( l, "String" )
    in
    el [] (text (label ++ " (" ++ type_ ++ ")"))


viewCommandPreset : Command cmd -> Element (ConsoleMsg cmd)
viewCommandPreset command =
    row
        [ padding sizing.medium
        , spacing sizing.medium
        , width fill
        , pointer
        , onClick (SetCurrentCommand <| Just command)
        , mouseOver [ Background.color <| rgb255 150 150 150 ]
        ]
        [ text command.description
        , row [ spacing 10 ] (List.map viewArg command.arguments)
        ]


viewCommandInput : Command cmd -> Element (ConsoleMsg cmd)
viewCommandInput command =
    row
        [ padding sizing.medium
        , spacing sizing.medium
        , width fill
        , Font.color <| rgb255 200 200 200
        , onEnter <| Execute command

        -- , pointer
        -- , onClick (SetCurrentCommand <| Just command)
        -- , mouseOver [ Background.color <| rgb255 150 150 150 ]
        ]
        [ text command.description
        , row [ spacing 10 ] (List.indexedMap viewArgInput command.arguments)
        ]


viewPresets : Console cmd -> Element (ConsoleMsg cmd)
viewPresets console =
    if console.showPresets then
        column
            [ width fill, Background.color <| rgb255 200 200 200 ]
            (List.map viewCommandPreset (Console.getCommandPresets console))

    else
        none


viewConsole : Console cmd -> Element (ConsoleMsg cmd)
viewConsole console =
    el
        [ width fill
        , height <| px 60
        , padding sizing.medium
        , aside
        , Font.size 12
        , Background.color <| rgb255 50 50 50
        ]
        (case console.currentCommand of
            Just cmd ->
                row [ width fill, height fill ]
                    [ button "x" (SetCurrentCommand Nothing)
                    , viewCommandInput cmd
                    , button "Go!" (Execute cmd)
                    ]

            Nothing ->
                column
                    [ width fill, height fill, below (viewPresets console) ]
                    [ viewCommandFilter console
                    ]
        )
