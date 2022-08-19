module Console exposing
    ( Argument
    , ArgumentValue(..)
    , Command
    , CommandValidator
    , Console
    , ConsoleMsg(..)
    , addBool
    , addInt
    , addRecentCommand
    , addString
    , consoleId
    , executeCommand
    , filterId
    , firstArgId
    , focusFirstArgument
    , getCommandPresets
    , hidePresets
    , init
    , isValid
    , new
    , resetCommandFilter
    , resetCurrentCommand
    , setArgAtIndex
    , setCurrentCommand
    , setFilter
    , showPresets
    , update
    , updateAtIndex
    , updateCurrentCommandArgument
    , validate
    )

import Browser.Dom
import Element.Region exposing (description)
import Task



---- CONSTANTS ----


consoleId : String
consoleId =
    "console-root"


firstArgId : String
firstArgId =
    "console-argument-first"


filterId : String
filterId =
    "command-filter"



---- TYPES ----


type alias CommandValidator msg =
    List ArgumentValue -> Result String msg


type alias Argument =
    { value : ArgumentValue
    , name : String
    , validationError : Maybe String
    }


type ArgumentValue
    = ArgInt (Maybe Int)
    | ArgString (Maybe String)
    | ArgBool (Maybe Bool)


type alias Command cmd =
    { description : String
    , arguments : List Argument
    , validate : List ArgumentValue -> Result String cmd
    }


type alias Console cmd =
    { currentCommand : Maybe (Command cmd)
    , commandPresets : List (Command cmd)
    , recentCommands : List (Command cmd)
    , showPresets : Bool
    , visible : Bool
    , commandFilter : String
    }


type ConsoleMsg cmd
    = Execute (Command cmd)
    | UpdateArgument Int String
    | SetCurrentCommand (Maybe (Command cmd))
    | ShowPresets
    | HidePresets
    | SetCommandFilter String
    | SelectedInput



---- ARGUMENT BUILDER ----


{-| Init for arguments builder
-}
init : String -> (List ArgumentValue -> Result String cmd) -> Command cmd
init description validator =
    Command description [] validator


{-| Add a new int argument with provided label
-}
addInt : String -> Command cmd -> Command cmd
addInt label command =
    { command | arguments = command.arguments ++ [ Argument (ArgInt Nothing) label Nothing ] }


{-| Add a new string argument with provided label
-}
addString : String -> Command cmd -> Command cmd
addString label command =
    { command | arguments = command.arguments ++ [ Argument (ArgString Nothing) label Nothing ] }


{-| Add a new bool argument with provided label
-}
addBool : String -> Command cmd -> Command cmd
addBool label command =
    { command | arguments = command.arguments ++ [ Argument (ArgBool Nothing) label Nothing ] }



---- ARGUMENTS ----


setArgAtIndex : Int -> Argument -> Command cmd -> Maybe (Command cmd)
setArgAtIndex index arg cmd =
    Just <| updateAtIndex index (always arg) cmd


updateAtIndex : Int -> (Argument -> Argument) -> Command cmd -> Command cmd
updateAtIndex target f command =
    let
        helper t fn i a =
            if i == t then
                fn a

            else
                a
    in
    { command | arguments = List.indexedMap (helper target f) command.arguments }


argHasValue : Argument -> Bool
argHasValue arg =
    case arg.value of
        ArgBool (Just _) ->
            True

        ArgInt (Just _) ->
            True

        ArgString (Just _) ->
            True

        _ ->
            False


isValid : Command cmd -> Bool
isValid command =
    List.all argHasValue command.arguments


validate : Command cmd -> Result String cmd
validate command =
    command.validate (List.map .value command.arguments)



---- CONSOLE ----


{-| Handle console messages and commands
-}
update : ConsoleMsg cmd -> Console cmd -> ( Console cmd, Maybe cmd, Cmd (ConsoleMsg cmd) )
update consoleMsg console =
    case consoleMsg of
        Execute cmd ->
            case validate cmd of
                Ok m ->
                    ( executeCommand cmd console, Just m, Cmd.none )

                -- { model | console = executeCommand cmd model.console } |> update m
                Err _ ->
                    ( console, Nothing, Cmd.none )

        UpdateArgument index argument ->
            ( updateCurrentCommandArgument index argument console
            , Nothing
            , Cmd.none
            )

        SetCurrentCommand cmd ->
            ( console
                |> setCurrentCommand cmd
                |> hidePresets
                |> resetCommandFilter
            , Nothing
            , focusFirstArgument cmd
            )

        ShowPresets ->
            ( showPresets console
            , Nothing
            , Cmd.none
            )

        HidePresets ->
            ( hidePresets console
            , Nothing
            , Cmd.none
            )

        SetCommandFilter filter ->
            ( setFilter filter console
            , Nothing
            , Cmd.none
            )

        SelectedInput ->
            ( console, Nothing, Cmd.none )


new : List (Command cmd) -> Console cmd
new presets =
    Console Nothing presets [] False True ""



-- TODO input string and validate


updateCurrentCommandArgument : Int -> String -> Console cmd -> Console cmd
updateCurrentCommandArgument index argInput console =
    let
        stringToBool i =
            case String.toLower i of
                "true" ->
                    Just True

                "false" ->
                    Just False

                _ ->
                    Nothing

        updateIfValidInput i a =
            case a.value of
                ArgInt _ ->
                    { a | value = ArgInt (String.toInt i) }

                ArgBool _ ->
                    { a | value = ArgBool (stringToBool i) }

                ArgString _ ->
                    { a | value = ArgString (Just i) }
    in
    { console | currentCommand = console.currentCommand |> Maybe.map (updateAtIndex index (updateIfValidInput argInput)) }


setCurrentCommand : Maybe (Command cmd) -> Console cmd -> Console cmd
setCurrentCommand cmd console =
    { console | currentCommand = cmd }


showPresets : Console cmd -> Console cmd
showPresets console =
    { console | showPresets = True }


hidePresets : Console cmd -> Console cmd
hidePresets console =
    { console | showPresets = False }


setFilter : String -> Console cmd -> Console cmd
setFilter filter console =
    { console | commandFilter = filter }


addRecentCommand : Command cmd -> Console cmd -> Console cmd
addRecentCommand cmd console =
    { console | recentCommands = List.take 5 (cmd :: console.recentCommands) }


resetCurrentCommand : Console cmd -> Console cmd
resetCurrentCommand console =
    { console | currentCommand = Nothing }


resetCommandFilter : Console cmd -> Console cmd
resetCommandFilter console =
    { console | commandFilter = "" }


{-| Return command presets based on current filter
-}
getCommandPresets : Console cmd -> List (Command cmd)
getCommandPresets console =
    console.commandPresets
        |> List.filter (commandFilter console.commandFilter)


commandFilter : String -> Command cmd -> Bool
commandFilter filter command =
    String.contains (String.toLower filter) (String.toLower <| command.description)


executeCommand : Command cmd -> Console cmd -> Console cmd
executeCommand cmd console =
    console
        |> resetCurrentCommand
        |> resetCommandFilter
        |> hidePresets
        |> addRecentCommand cmd


focusFirstArgument : Maybe a -> Cmd (ConsoleMsg cmd)
focusFirstArgument m =
    case m of
        Just _ ->
            Task.attempt (\_ -> SelectedInput) (Browser.Dom.focus firstArgId)

        Nothing ->
            Task.attempt (\_ -> SelectedInput) (Browser.Dom.focus filterId)



---- VIEW ----
-- argumentColor : Argument -> String
-- argumentColor arg =
--     case arg of
--         ArgBool _ _ ->
--             "cyan"
--         ArgInt _ _ ->
--             "magenta"
--         ArgString _ _ ->
--             "yellow"
-- stringToBool : String -> Maybe Bool
-- stringToBool s =
--     case String.toLower s of
--         "true" ->
--             Just True
--         "false" ->
--             Just False
--         _ ->
--             Nothing
-- updateIntMsg : Int -> String -> String -> ConsoleMsg cmd
-- updateIntMsg index label v =
--     UpdateArgument index (ArgInt label (v |> String.toInt))
-- updateStringMsg : Int -> String -> String -> ConsoleMsg cmd
-- updateStringMsg index label v =
--     UpdateArgument index (ArgString label (Just v))
-- updateBoolMsg : Int -> String -> String -> ConsoleMsg cmd
-- updateBoolMsg index label v =
--     UpdateArgument index (ArgBool label (v |> stringToBool))
