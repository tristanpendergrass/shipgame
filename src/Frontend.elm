module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dice exposing (..)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lamdera
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


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
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
            case model.state of
                MainMenu mainMenuState ->
                    ( { model
                        | state = MainMenu { mainMenuState | joinCode = newJoinCode, joinCodeIsInvalid = False }
                      }
                    , Cmd.none
                    )

                _ ->
                    noOp

        HandleJoinCodeSubmit ->
            case model.state of
                MainMenu mainMenuState ->
                    ( { model
                        | state = MainMenu { mainMenuState | joinCodeIsInvalid = False, formSubmitted = True }
                      }
                    , Lamdera.sendToBackend (JoinGame mainMenuState.joinCode)
                    )

                _ ->
                    noOp

        HandleCreateGameButtonClick ->
            case model.state of
                MainMenu mainMenuState ->
                    ( { model
                        | state = MainMenu { mainMenuState | joinCode = "", joinCodeIsInvalid = False, formSubmitted = True }
                      }
                    , Lamdera.sendToBackend CreateLobby
                    )

                _ ->
                    noOp

        HandleNameInput newName ->
            case model.state of
                InGame inGameState ->
                    Debug.todo "Implement"

                _ ->
                    noOp

        HandleNameSubmit ->
            case model.state of
                InGame inGameState ->
                    Debug.todo "Implement"

                _ ->
                    noOp

        HandleStartGameClick ->
            case model.state of
                InGame { lobby } ->
                    -- TODO: disable start game button?
                    ( model, Lamdera.sendToBackend (StartGame lobby.id) )

                _ ->
                    noOp

        HandleEndGameClick ->
            case model.state of
                InGame { lobby } ->
                    ( model, Lamdera.sendToBackend (EndGame lobby.id) )

                _ ->
                    noOp

        HandleRoll ->
            case model.state of
                InGame { lobby } ->
                    ( model, Lamdera.sendToBackend (UpdateGameWithRoll lobby.id) )

                _ ->
                    noOp


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOpToFrontend ->
            ( model
            , Cmd.none
            )

        GoToMainMenu playerId ->
            case model.state of
                Unconnected ->
                    ( { model
                        | state =
                            MainMenu
                                { id = playerId
                                , joinCode = "JKLM"
                                , joinCodeIsInvalid = False
                                , formSubmitted = False
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    noOp

        GoToInGame playerId lobby playerData ->
            ( { model
                | state =
                    InGame
                        { id = playerId
                        , lobby = lobby
                        , playerData = playerData
                        }
              }
            , Cmd.none
            )

        UpdateLobby newLobby ->
            case model.state of
                InGame inGameState ->
                    ( { model
                        | state =
                            InGame
                                { inGameState
                                    | lobby = newLobby
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    noOp

        JoinGameFailed ->
            case model.state of
                MainMenu mainMenuState ->
                    Debug.todo "Implement"

                _ ->
                    noOp

        UpdatePlayerData newPlayerData ->
            case model.state of
                InGame inGameState ->
                    ( { model
                        | state =
                            InGame
                                { inGameState
                                    | playerData = newPlayerData
                                }
                      }
                    , Cmd.none
                    )

                _ ->
                    noOp


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
    div []
        -- Print round
        -- Dice
        -- Status of four players
        -- Player Name
        -- Current Ship
        -- Score
        [ div [] [ text <| "Round: " ++ String.fromInt round ]
        , div [] [ text <| "Dice: " ++ renderDice dice ]
        , div [] [ button [ disabled <| not (currentPlayer.id == playerId), onClick HandleRoll ] [ text "Roll" ] ]
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

                            -- NamingPlayer _ name gameState ->
                            --     div [ class "flex flex-col justify-center space-y-4" ]
                            --         [ div [ class "inline-block" ] [ text "My name is" ]
                            --         , Html.form [ onSubmit HandleNameSubmit, class "flex flex-col items-center space-y-4 form-control" ]
                            --             [ div [] [ input [ onInput HandleNameInput, value name, class "input input-bordered input-primary" ] [] ]
                            --             , div [] [ button [ type_ "submit", class "btn btn-primary" ] [ text "Submit" ] ]
                            --             ]
                            --         ]
                            -- ConfirmingName _ _ ->
                            --     text "Entering lobby"
                            InGame { id, lobby, playerData } ->
                                case lobby.gameWrapper of
                                    Lobby.NotStarted playerIds ->
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

                                    Lobby.InProgress shipGame ->
                                        renderShipGame id shipGame
                        ]
                    ]
               ]
    }
