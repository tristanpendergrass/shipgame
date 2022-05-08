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
                |> Lobby.addPlayer 1
                |> Maybe.andThen (Lobby.addPlayer 2)
                |> Maybe.andThen (Lobby.addPlayer 3)
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
                    { id = 3
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


renderShipGame : PlayerId -> ShipGame -> Html FrontendMsg
renderShipGame playerId { round, players, dice } =
    let
        currentPlayer =
            SelectionList.getSelected players
    in
    div [ class "flex flex-col w-64 items-center space-y-4" ]
        -- Print round
        -- Dice
        -- Status of four players
        -- Player Name
        -- Current Ship
        -- Score
        [ div [ class "text-xl" ] [ text <| "Round: " ++ String.fromInt round ]
        , div [ class "text-2xl" ] [ text <| "Dice: " ++ renderDice dice ]
        , div [] [ button [ class "btn", disabled <| not (Debug.log "curent player id" currentPlayer.id == Debug.log "plkayer Id" playerId), onClick HandleRoll ] [ text "Roll" ] ]
        , div [ class "flex" ]
            (players
                |> SelectionList.toTupleList
                |> List.map
                    (\( player, selected ) ->
                        div
                            [ class "w-64 h-64 border border-black rounded"
                            , class <|
                                if selected then
                                    "border-red-500"

                                else
                                    ""
                            ]
                            []
                    )
            )
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
            ++ [ div [ class "w-screen h-screen flex flex-col justify-center items-center" ]
                    [ div []
                        [ case model.state of
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
                            InGame { id, lobby, playerData, nameInput } ->
                                case lobby.gameWrapper of
                                    Lobby.NotStarted playerIds ->
                                        if Dict.member id playerData then
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
                                            div [ class "flex flex-col justify-center space-y-4" ]
                                                [ div [ class "inline-block" ] [ text "My name is" ]
                                                , Html.form [ onSubmit HandleNameSubmit, class "flex flex-col items-center space-y-4 form-control" ]
                                                    [ div [] [ input [ onInput HandleNameInput, value nameInput, class "input input-bordered input-primary" ] [] ]
                                                    , div [] [ button [ type_ "submit", class "btn btn-primary" ] [ text "Submit" ] ]
                                                    ]
                                                ]

                                    Lobby.InProgress shipGame ->
                                        renderShipGame id shipGame
                        ]
                    ]
               ]
    }
