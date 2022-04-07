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
                MainMenu playerId _ _ ->
                    ( { model
                        | state = MainMenu playerId newJoinCode False
                      }
                    , Cmd.none
                    )

                _ ->
                    noOp

        HandleJoinCodeSubmit ->
            case model.state of
                MainMenu playerId joinCode _ ->
                    ( { model
                        | state = ConnectingToGame playerId
                      }
                    , Lamdera.sendToBackend (JoinGame joinCode)
                    )

                _ ->
                    noOp

        HandleCreateGameButtonClick ->
            case model.state of
                MainMenu playerId _ _ ->
                    ( { model
                        | state = ConnectingToGame playerId
                      }
                    , Lamdera.sendToBackend CreateLobby
                    )

                _ ->
                    noOp

        HandleNameInput newName ->
            case model.state of
                NamingPlayer playerId _ gameState ->
                    ( { model | state = NamingPlayer playerId newName gameState }, Cmd.none )

                _ ->
                    noOp

        HandleNameSubmit ->
            case model.state of
                NamingPlayer playerId name gameState ->
                    ( { model | state = ConfirmingName playerId gameState }, Lamdera.sendToBackend (NamePlayer gameState.id name) )

                _ ->
                    noOp

        HandleStartGameClick ->
            case model.state of
                InGame _ lobby ->
                    ( model, Lamdera.sendToBackend (StartGame lobby.id) )

                _ ->
                    noOp

        HandleEndGameClick ->
            case model.state of
                InGame _ lobby ->
                    ( model, Lamdera.sendToBackend (EndGame lobby.id) )

                _ ->
                    noOp

        HandleRoll ->
            case model.state of
                InGame _ lobby ->
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

        AssignPlayerId playerId ->
            case model.state of
                Unconnected ->
                    ( { model | state = MainMenu playerId "JKLM" False }
                    , Cmd.none
                    )

                _ ->
                    noOp

        UpdateLobby newGame ->
            case model.state of
                Unconnected ->
                    noOp

                ConnectingToGame playerId ->
                    ( { model | state = NamingPlayer playerId "Jim" newGame }
                    , Cmd.none
                    )

                ConfirmingName playerId _ ->
                    ( { model | state = InGame playerId newGame }
                    , Cmd.none
                    )

                InGame playerId _ ->
                    ( { model | state = InGame playerId newGame }
                    , Cmd.none
                    )

                _ ->
                    noOp

        JoinGameFailed ->
            case model.state of
                ConnectingToGame playerId ->
                    ( { model | state = MainMenu playerId "" True }
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
    { title = ""
    , body =
        css
            ++ [ div [ class "w-screen flex flex-col justify-center items-center" ]
                    [ div [ class "inline-block w-64" ] [ img [ src "https://lamdera.app/lamdera-logo-black.png" ] [] ]
                    , div
                        [ style "font-family" "sans-serif"
                        , style "padding-top" "40px"
                        ]
                        [ case model.state of
                            Unconnected ->
                                div [] [ text "Connecting to server..." ]

                            MainMenu _ joinCode showErrorMessage ->
                                div []
                                    [ h1 [] [ text "Join a game" ]
                                    , Html.form [ onSubmit HandleJoinCodeSubmit ]
                                        [ div
                                            [ style "color" "red"
                                            , style "opacity"
                                                (if showErrorMessage then
                                                    "100%"

                                                 else
                                                    "0"
                                                )
                                            ]
                                            [ text "Join code was incorrect" ]
                                        , div [] [ input [ onInput HandleJoinCodeInput, value joinCode ] [] ]
                                        , div [] [ button [ class "btn btn-primary", type_ "submit" ] [ text "Join" ] ]
                                        , div [] [ div [ class "badge" ] [ text "Badge" ] ]
                                        ]
                                    , h1 [] [ text "Create a game" ]
                                    , Html.button [ onClick HandleCreateGameButtonClick ] [ text "Create game" ]
                                    ]

                            ConnectingToGame _ ->
                                text "Connecting to game"

                            NamingPlayer _ name gameState ->
                                div []
                                    [ h1 [] [ text "My name is" ]
                                    , Html.form [ onSubmit HandleNameSubmit ]
                                        [ div [] [ input [ onInput HandleNameInput, value name ] [] ]
                                        , div [] [ button [ type_ "submit" ] [ text "Submit" ] ]
                                        ]
                                    ]

                            ConfirmingName _ _ ->
                                text "Entering lobby"

                            InGame myPlayerId lobby ->
                                case lobby.gameWrapper of
                                    Lobby.NotStarted playerIds ->
                                        div []
                                            [ div [] [ text "Game not started" ]
                                            , div [] [ text <| "Join Code: " ++ lobby.joinCode ]
                                            , div [] [ text "Players:" ]
                                            , div []
                                                (playerIds
                                                    |> List.map
                                                        (\playerId ->
                                                            let
                                                                displayName =
                                                                    Dict.get playerId lobby.playerData
                                                                        |> Maybe.andThen .displayName
                                                                        |> Maybe.withDefault "Anonymous"
                                                            in
                                                            div [] [ text displayName ]
                                                        )
                                                )
                                            , div [] [ button [ onClick HandleStartGameClick ] [ text "Start game" ] ]
                                            ]

                                    Lobby.InProgress shipGame ->
                                        renderShipGame myPlayerId shipGame
                        ]
                    ]
               ]
    }
