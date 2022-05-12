module FrontendDev exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dice exposing (..)
import Dict
import Evergreen.V2.Lobby exposing (Lobby)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lamdera
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
    let
        initialLobby =
            Lobby.create 0 "JKLM" 3 (Random.initialSeed 0)

        lobbyWithPlayers =
            initialLobby
                |> Lobby.addPlayer 2
                |> Maybe.andThen (Lobby.addPlayer 1)
                |> Maybe.andThen (Lobby.addPlayer 0)
                |> Maybe.withDefault initialLobby

        lobbyWithGame =
            lobbyWithPlayers
                |> Lobby.startGame
                |> Result.withDefault initialLobby

        playerData : PlayerData
        playerData =
            Dict.fromList
                [ ( 0, { id = 0, displayName = Just "Player 1" } )
                , ( 1, { id = 1, displayName = Just "Player 2" } )
                , ( 2, { id = 2, displayName = Just "Player 3" } )
                , ( 3, { id = 3, displayName = Just "Player 4" } )
                ]
    in
    ( { key = key
      , state =
            Debug.log "Initial state"
                (InGame
                    { id = 0
                    , lobby = lobbyWithGame
                    , playerData = playerData
                    , nameInput = ""
                    }
                )
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    ( model, Cmd.none )


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
            button
                [ class "btn btn-primary"
                , class <|
                    if youAreCurrentPlayer then
                        "visible"

                    else
                        "invisible"
                ]
                [ text "Roll" ]

        die value =
            div [ class "w-12 h-12 bg-gray-100 rounded text-gray-900 text-2xl leading-none shadow-lg font-bold flex justify-center items-center" ]
                [ div [] [ text <| String.fromInt value ] ]
    in
    div [ class "flex flex-col items-center space-y-8 p-8 " ]
        [ turnText
        , passButton
        , div [ class "border border-gray-100 rounded w-full px-4 py-8 relative" ]
            [ div [ class "absolute top-2 right-2 leading-none" ] [ timeRolledText ]
            , div [ class "flex flex-col items-center space-y-12" ]
                [ rollButton
                , div [ class "flex flex-col w-full items-center space-y-4" ]
                    [ div [ class "flex w-full justify-center space-x-4" ] [ die 1, die 2 ]
                    , div [ class "flex w-full justify-center space-x-4" ] [ die 3, die 4, die 5 ]
                    ]
                ]
            ]
        ]


renderShipGame : InGameState -> ShipGame -> Html FrontendMsg
renderShipGame inGameState game =
    let
        currentPlayer =
            SelectionList.getSelected game.players
    in
    div [ class "flex w-full h-full justify-center space-x-12" ]
        [ div [ class "w-72 h-full bg-white/25 rounded-lg" ] []
        , div [ class "w-96 h-full bg-blue-900 rounded-lg" ] [ renderCenterColumn inGameState game ]
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
            ++ [ div [ class "w-screen h-screen flex flex-col items-center py-16 space-y-12 bg-blue-500" ]
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
                                                        ""

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
                    ]
               ]
    }
