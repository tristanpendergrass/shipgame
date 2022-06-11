module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dice exposing (..)
import Dict
import Dict.Extra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lamdera
import List.Extra
import List.Nonempty
import Lobby exposing (GameWrapper(..), Lobby)
import Player exposing (Player, PlayerId)
import Random
import SelectionList exposing (SelectionList)
import ShipGame exposing (..)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , state = Unconnected
      }
    , Cmd.none
    )


createMainMenuUpdate : Model -> (MainMenuState -> ( MainMenuState, Cmd FrontendMsg )) -> ( Model, Cmd FrontendMsg )
createMainMenuUpdate model updateFn =
    case model.state of
        MainMenu mainMenuState ->
            let
                ( newMainMenuState, cmd ) =
                    updateFn mainMenuState
            in
            ( { model | state = MainMenu newMainMenuState }, cmd )

        _ ->
            ( model, Cmd.none )


createInGameUpdate : Model -> (InGameState -> ( InGameState, Cmd FrontendMsg )) -> ( Model, Cmd FrontendMsg )
createInGameUpdate model updateFn =
    case model.state of
        InGame inGameState ->
            let
                ( newInGameState, cmd ) =
                    updateFn inGameState
            in
            ( { model | state = InGame newInGameState }, cmd )

        _ ->
            ( model, Cmd.none )


getPlayerId : Model -> Maybe PlayerId
getPlayerId model =
    case model.state of
        Unconnected ->
            Nothing

        MainMenu { id } ->
            Just id

        InGame { id } ->
            Just id


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )

        updateMainMenu =
            createMainMenuUpdate model

        updateInGame =
            createInGameUpdate model
    in
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            noOp

        NoOpFrontendMsg ->
            noOp

        HandleJoinCodeInput newJoinCode ->
            updateMainMenu
                (\mainMenuState ->
                    ( { mainMenuState
                        | joinCode = newJoinCode
                        , joinCodeIsInvalid = False
                      }
                    , Cmd.none
                    )
                )

        HandleJoinCodeSubmit ->
            updateMainMenu
                (\mainMenuState ->
                    ( { mainMenuState | joinCodeIsInvalid = False, formSubmitted = True }
                    , Lamdera.sendToBackend (JoinGame mainMenuState.joinCode)
                    )
                )

        HandleCreateGameButtonClick ->
            updateMainMenu
                (\mainMenuState ->
                    ( { mainMenuState | joinCode = "", joinCodeIsInvalid = False, formSubmitted = True }
                    , Lamdera.sendToBackend CreateLobby
                    )
                )

        HandleNameInput newName ->
            updateInGame
                (\inGameState ->
                    ( { inGameState | nameInput = newName }, Cmd.none )
                )

        HandleNameSubmit ->
            updateInGame
                (\inGameState ->
                    ( inGameState, Lamdera.sendToBackend (NamePlayer inGameState.nameInput) )
                )

        HandleStartGameClick ->
            updateInGame
                (\inGameState ->
                    ( inGameState, Lamdera.sendToBackend (StartGame inGameState.lobby.id) )
                )

        HandleEndGameClick ->
            updateInGame
                (\inGameState ->
                    ( inGameState, Lamdera.sendToBackend (EndGame inGameState.lobby.id) )
                )

        HandleRoll ->
            updateInGame
                (\inGameState ->
                    ( inGameState, Lamdera.sendToBackend (UpdateGameWithRoll inGameState.lobby.id) )
                )

        HandlePass ->
            updateInGame
                (\inGameState ->
                    ( inGameState, Lamdera.sendToBackend (UpdateGame inGameState.lobby.id Pass) )
                )

        HandleKeep index ->
            updateInGame
                (\inGameState ->
                    ( inGameState, Lamdera.sendToBackend (UpdateGame inGameState.lobby.id (Keep index)) )
                )

        ReturnToMainMenu ->
            case getPlayerId model of
                Nothing ->
                    noOp

                Just playerId ->
                    ( { model
                        | state =
                            MainMenu
                                { id = playerId
                                , joinCode = ""
                                , joinCodeIsInvalid = False
                                , formSubmitted = False
                                }
                      }
                    , Lamdera.sendToBackend ExitLobby
                    )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    let
        noOp =
            ( model, Cmd.none )

        updateMainMenu =
            createMainMenuUpdate model

        updateInGame =
            createInGameUpdate model
    in
    case msg of
        NoOpToFrontend ->
            ( model
            , Cmd.none
            )

        GoToMainMenu playerId ->
            ( { model
                | state =
                    MainMenu
                        { id = playerId
                        , joinCode = "TOUJ"
                        , joinCodeIsInvalid = False
                        , formSubmitted = False
                        }
              }
            , Cmd.none
            )

        GoToInGame playerId lobby playerData ->
            ( { model
                | state =
                    InGame
                        { id = playerId
                        , lobby = lobby
                        , playerData = playerData
                        , nameInput = "Joe"
                        }
              }
            , Cmd.none
            )

        UpdateLobby newLobby ->
            updateInGame
                (\inGameState ->
                    ( { inGameState | lobby = newLobby }, Cmd.none )
                )

        JoinGameFailed ->
            updateMainMenu
                (\mainMenuState ->
                    ( { mainMenuState
                        | joinCodeIsInvalid = True
                        , formSubmitted = False
                      }
                    , Cmd.none
                    )
                )

        UpdatePlayerData newPlayerData ->
            updateInGame
                (\inGameState ->
                    ( { inGameState
                        | playerData = newPlayerData
                      }
                    , Cmd.none
                    )
                )



-- VIEW


type DiceDisplayMode
    = DiceNotSelectable
    | DiceNotSelected
    | DiceSelected


renderDice : Dice -> String
renderDice dice =
    let
        renderValues : List ( Int, Bool ) -> List String
        renderValues diceValues =
            List.map
                (\( value, keep ) ->
                    if keep then
                        String.fromInt value ++ "*"

                    else
                        String.fromInt value
                )
                diceValues
    in
    case dice of
        NeverRolled ->
            "Not rolled"

        RolledOnce diceValues ->
            "Rolled once (" ++ String.join ", " (renderValues diceValues) ++ ")"

        RolledTwice diceValues ->
            "Rolled twice (" ++ String.join ", " (renderValues diceValues) ++ ")"

        RolledThrice diceValues ->
            "Rolled thrice (" ++ String.join ", " (renderValues diceValues) ++ ")"


renderPlayer : InGameState -> ShipGamePlayer -> Bool -> Html FrontendMsg
renderPlayer inGameState player isSelected =
    let
        displayName =
            inGameState.playerData
                |> Dict.get player.id
                |> Maybe.andThen (\playerData -> playerData.displayName)
                |> Maybe.withDefault "Anonymous"
    in
    div [ class "flex flex-col items-center space-y-4" ]
        [ div
            [ class "text-lg"
            , class <|
                if isSelected then
                    "underline"

                else
                    ""
            ]
            [ text displayName ]
        ]


renderCenterColumn : InGameState -> ShipGame -> Html FrontendMsg
renderCenterColumn inGameState game =
    let
        currentPlayer =
            SelectionList.getSelected game.players

        youAreCurrentPlayer =
            currentPlayer.id == inGameState.id

        turnText =
            let
                turnTextContent =
                    if youAreCurrentPlayer then
                        "It's your turn"

                    else
                        "It's " ++ currentPlayerName ++ "'s turn"

                currentPlayerName =
                    Dict.get currentPlayer.id inGameState.playerData
                        |> Maybe.andThen .displayName
                        |> Maybe.withDefault "Anonymous"
            in
            div [ class "text-gray-100 text-2xl" ] [ text turnTextContent ]

        passButton =
            div [ class "flex w-full justify-center h-16" ]
                [ button
                    [ class "btn"
                    , class <|
                        if Dice.doneRolling game.dice then
                            "btn-primary"

                        else
                            "btn-outline"
                    , class <|
                        if youAreCurrentPlayer then
                            ""

                        else
                            "invisible"
                    , onClick HandlePass
                    ]
                    [ text "Pass" ]
                ]

        timeRolledText =
            let
                ( currentDieRoll, tooltipText ) =
                    case game.dice of
                        Dice.NeverRolled ->
                            ( 0, "Three rolls remaining" )

                        Dice.RolledOnce _ ->
                            ( 1, "Two rolls remaining" )

                        Dice.RolledTwice _ ->
                            ( 2, "One roll remaining" )

                        Dice.RolledThrice _ ->
                            ( 3, "No rolls remaining" )
            in
            div [ class "text-gray-100 tooltip cursor-pointer", attribute "data-tip" tooltipText ] [ text <| String.fromInt currentDieRoll ++ "/3" ]

        rollButton =
            let
                rollText =
                    case game.dice of
                        Dice.NeverRolled ->
                            "Roll"

                        _ ->
                            "Roll again"
            in
            button
                [ class "btn btn-primary w-32"
                , class <|
                    if youAreCurrentPlayer && Dice.canRollAgain game.dice then
                        "visible"

                    else
                        "invisible"
                , onClick HandleRoll
                ]
                [ text rollText ]

        die : { value : Int, displayMode : DiceDisplayMode, index : Int } -> Html FrontendMsg
        die { value, displayMode, index } =
            div
                [ class "flex justify-center items-center w-12 h-12 bg-gray-100 rounded shadow-lg cursor-pointer"
                , class "text-gray-900 text-2xl leading-none font-bold"
                , case displayMode of
                    DiceNotSelectable ->
                        class "border-4 border-gray-800"

                    DiceNotSelected ->
                        class "hover:border-4 hover:border-yellow-500/50"

                    DiceSelected ->
                        class "border-4 border-yellow-500"
                , onClick (HandleKeep index)
                ]
                [ div [] [ text <| String.fromInt value ] ]

        ( diceForFirstRow, diceForSecondRow ) =
            case Dice.toDisplayValues game.dice of
                Just [ first, second, third, fourth, fifth ] ->
                    let
                        renderDie : ( Int, Bool ) -> Int -> Html FrontendMsg
                        renderDie ( dieValue, dieIsSelected ) index =
                            if youAreCurrentPlayer then
                                die
                                    { value = dieValue
                                    , displayMode =
                                        if dieIsSelected then
                                            DiceSelected

                                        else
                                            DiceNotSelected
                                    , index = index
                                    }

                            else
                                die
                                    { value = dieValue
                                    , displayMode =
                                        if dieIsSelected then
                                            DiceSelected

                                        else
                                            DiceNotSelectable
                                    , index = index
                                    }
                    in
                    ( [ renderDie first 0, renderDie second 1 ]
                    , [ renderDie third 2, renderDie fourth 3, renderDie fifth 4 ]
                    )

                _ ->
                    ( [], [] )
    in
    div [ class "flex flex-col items-center space-y-8 p-8 overflow-y-auto overflow-x-hidden" ]
        [ turnText
        , passButton
        , div [ class "border border-gray-100 rounded w-full px-4 py-8 relative" ]
            [ div [ class "absolute top-2 right-2 leading-none" ] [ timeRolledText ]
            , div [ class "flex flex-col items-center space-y-12" ]
                [ rollButton
                , div [ class "flex flex-col w-full items-center space-y-4" ]
                    [ div [ class "flex w-full justify-center space-x-4" ] diceForFirstRow
                    , div [ class "flex w-full justify-center space-x-4" ] diceForSecondRow
                    ]
                ]
            ]
        ]


renderShip : Ship -> Html FrontendMsg
renderShip ship =
    let
        shipText =
            case ship of
                ShipWithNothing ->
                    "Incomplete"

                ShipWithOne ->
                    "Incomplete"

                ShipWithTwo ->
                    "Incomplete"

                ShipWithThree ->
                    "Incomplete"

                ShipWithFour num1 ->
                    "Ship (" ++ String.fromInt num1 ++ ")"

                ShipWithFive num1 num2 ->
                    "Ship (" ++ String.fromInt num1 ++ ", " ++ " " ++ String.fromInt num2 ++ ")"
    in
    span [ class "text-gray-100" ] [ text shipText ]


renderShips : { name : Maybe String, pastShips : List Ship, isSelected : Bool, isYou : Bool } -> Html FrontendMsg
renderShips { name, pastShips, isSelected, isYou } =
    let
        renderDisplayName =
            div
                [ class "text-gray-100"
                , if isYou then
                    class "font-bold underline"

                  else
                    class ""
                ]
                [ text <|
                    if isYou then
                        "Your ships"

                    else
                        Maybe.withDefault "Anonymous" name ++ "'s ships"
                ]
    in
    div [ class "flex flex-col items-center space-y-2 w-full" ]
        [ renderDisplayName
        , div [ class "border border-gray-100 rounded w-full h-64 p-12" ]
            [ div [ class "flex flex-col w-full items-center space-y-2" ]
                (List.map renderShip pastShips)
            ]
        ]


renderSideColumn : InGameState -> List ( ShipGamePlayer, Bool ) -> Html FrontendMsg
renderSideColumn inGameState players =
    div [ class "flex flex-col items-center space-y-16 p-4 overflow-y-auto overflow-x-hidden" ]
        (players
            |> List.map
                (\( { id, pastShips }, isSelected ) ->
                    let
                        playerName =
                            Dict.get id inGameState.playerData
                                |> Maybe.andThen .displayName

                        isYou =
                            inGameState.id == id
                    in
                    renderShips { name = playerName, pastShips = pastShips, isSelected = isSelected, isYou = isYou }
                )
        )


renderShipGame : InGameState -> ShipGame -> Html FrontendMsg
renderShipGame inGameState game =
    let
        currentPlayer =
            SelectionList.getSelected game.players

        splitListLeftRight : List a -> ( List a, List a )
        splitListLeftRight =
            List.foldr
                (\item ( left, right ) ->
                    if List.length left <= List.length right then
                        ( item :: left, right )

                    else
                        ( left, item :: right )
                )
                ( [], [] )

        leftPlayers =
            game.players
                |> SelectionList.toList
                |> List.Extra.removeIfIndex (\i -> modBy 2 i == 1)

        rightPlayers =
            game.players
                |> SelectionList.toList
                |> List.Extra.removeIfIndex (\i -> modBy 2 i == 0)
    in
    div [ class "flex w-full h-full justify-center space-x-12" ]
        [ div [ class "w-72 h-full bg-white/25 rounded-lg" ] [ renderSideColumn inGameState leftPlayers ]
        , div [ class "w-96 h-full bg-blue-900 rounded-lg" ] [ renderCenterColumn inGameState game ]
        , div [ class "w-72 h-full bg-white/25 rounded-lg" ] [ renderSideColumn inGameState rightPlayers ]
        ]


renderFinished : InGameState -> GameSummary -> Html FrontendMsg
renderFinished inGameState gameSummary =
    let
        comparePlayersByScore : ShipGamePlayer -> ShipGamePlayer -> Order
        comparePlayersByScore player1 player2 =
            compare (ShipGame.getPlayerScore player2) (ShipGame.getPlayerScore player1)
    in
    div [ class "flex w-full h-full justify-center space-x-12" ]
        [ div [ class "w-72 h-full bg-white/25 rounded-lg" ] []
        , div [ class "w-96 h-full bg-blue-900 rounded-lg" ]
            [ div [ class "flex flex-col items-center space-y-8 p-8 overflow-y-auto overflow-x-hidden" ]
                [ span [ class "font-bold text-2xl" ] [ text "Finished" ]
                , button [ class "btn btn-secondary", onClick ReturnToMainMenu ] [ text "Return to main menu" ]
                , div [ class "flex flex-col items-center w-full space-y-4" ]
                    (gameSummary
                        |> List.sortWith comparePlayersByScore
                        |> List.indexedMap
                            (\index playerSummary ->
                                let
                                    playerName =
                                        Dict.get playerSummary.id inGameState.playerData
                                            |> Maybe.andThen .displayName
                                            |> Maybe.withDefault "Anonymous"

                                    isYou =
                                        inGameState.id == playerSummary.id

                                    name =
                                        span
                                            [ class <|
                                                if isYou then
                                                    "underline"

                                                else
                                                    ""
                                            ]
                                            [ text playerName ]
                                in
                                div
                                    [ class "flex flex-col w-full items-center space-y-4"
                                    , class <|
                                        if index == 0 then
                                            "border-yellow-500 border-2 rounded py-4"

                                        else
                                            ""
                                    ]
                                <|
                                    List.concat
                                        [ [ name ]
                                        , List.map renderShip playerSummary.pastShips
                                        ]
                            )
                    )
                ]
            ]
        , div [ class "w-72 h-full bg-white/25 rounded-lg" ] []
        ]


view : Model -> Browser.Document FrontendMsg
view model =
    let
        css =
            -- There's an experimental technique to include styles in header instead of body https://dashboard.lamdera.app/docs/html-head
            -- We're not using it for now because it's experimental but might be useful if we want to eliminate the flicker from the css loading in
            [ Html.node "link" [ rel "stylesheet", href "/output.css" ] []
            ]
    in
    { title = "Shipgame"
    , body =
        css
            ++ [ div [ class "w-screen h-screen flex flex-col items-center pt-16 pb-4 space-y-12 bg-blue-500 overflow-y-auto" ]
                    [ div [ class "text-green-200 font-bold text-9xl" ] [ text "Shipgame" ]
                    , case model.state of
                        Unconnected ->
                            div [] [ text "Connecting to server..." ]

                        MainMenu { joinCode, joinCodeIsInvalid, formSubmitted } ->
                            if not formSubmitted then
                                let
                                    textClasses =
                                        "font-red text-4xl"
                                in
                                div [ class "flex flex-col justify-center space-y-4" ]
                                    [ h1 [ class textClasses ] [ text "Join a game" ]
                                    , Html.form [ onSubmit HandleJoinCodeSubmit, class "flex flex-col items-center space-y-4" ]
                                        [ div [ class "form-control" ]
                                            [ label [ class "label", for "join-code-input" ] [ text "Join code" ]
                                            , input [ onInput HandleJoinCodeInput, value joinCode, class "input input-bordered input-primary", id "join-code-input" ] []
                                            , div
                                                [ class "text-red-500"
                                                , class
                                                    (if joinCodeIsInvalid then
                                                        "visible"

                                                     else
                                                        "invisible"
                                                    )
                                                ]
                                                [ text "Join code was incorrect" ]
                                            ]
                                        , div [] [ button [ class "btn btn-primary", type_ "submit" ] [ text "Join" ] ]
                                        ]
                                    , button [ onClick HandleCreateGameButtonClick, class "btn btn-secondary" ] [ text "Create game" ]
                                    ]

                            else
                                -- Trying to connect to new or existing game
                                text "Connecting to game"

                        -- ConfirmingName _ _ ->
                        --     text "Entering lobby"
                        InGame inGameState ->
                            let
                                { lobby, id, playerData, nameInput } =
                                    inGameState
                            in
                            case lobby.gameWrapper of
                                Lobby.NotStarted playerIds ->
                                    if Dict.member id playerData then
                                        -- Show game
                                        div [ class "flex flex-col justify-center space-y-4" ]
                                            [ div [ class "text-lg" ] [ text "Game not started" ]
                                            , div [] [ text <| "Join Code: " ++ lobby.joinCode ]
                                            , div [] [ text "Players:" ]
                                            , div []
                                                (playerIds
                                                    |> List.map
                                                        (\playerId ->
                                                            let
                                                                displayName =
                                                                    Dict.get playerId playerData
                                                                        |> Maybe.andThen .displayName
                                                                        |> Maybe.withDefault "Anonymous"
                                                            in
                                                            div [] [ text displayName ]
                                                        )
                                                )
                                            , div [] [ button [ onClick HandleStartGameClick, class "btn btn-primary" ] [ text "Start game" ] ]
                                            ]

                                    else
                                        -- Show player naming screen
                                        div [ class "flex flex-col justify-center space-y-4" ]
                                            [ div [ class "inline-block" ] [ text "My name is" ]
                                            , Html.form [ onSubmit HandleNameSubmit, class "flex flex-col items-center space-y-4 form-control" ]
                                                [ div [] [ input [ onInput HandleNameInput, value nameInput, class "input input-bordered input-primary" ] [] ]
                                                , div [] [ button [ type_ "submit", class "btn btn-primary" ] [ text "Submit" ] ]
                                                ]
                                            ]

                                Lobby.InProgress shipGame ->
                                    renderShipGame inGameState shipGame

                                Lobby.Finished gameSummary ->
                                    renderFinished inGameState gameSummary
                    ]
               ]
    }
