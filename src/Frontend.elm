module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lamdera
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
                    , Lamdera.sendToBackend CreateGame
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
                    ( { model | state = EnteringLobby playerId gameState }, Lamdera.sendToBackend (NamePlayer gameState.id name) )

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

        UpdateGame newGame ->
            case model.state of
                Unconnected ->
                    noOp

                ConnectingToGame playerId ->
                    ( { model | state = NamingPlayer playerId "" newGame }
                    , Cmd.none
                    )

                EnteringLobby playerId _ ->
                    ( { model | state = InGame playerId newGame }
                    , Cmd.none
                    )

                InGame playerId _ ->
                    ( { model | state = InGame playerId newGame }
                    , Cmd.none
                    )

                _ ->
                    Debug.todo "Implement"

        JoinGameFailed ->
            case model.state of
                ConnectingToGame playerId ->
                    ( { model | state = MainMenu playerId "" True }
                    , Cmd.none
                    )

                _ ->
                    noOp


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ div [ style "text-align" "center", style "padding-top" "40px" ]
            [ img [ src "https://lamdera.app/lamdera-logo-black.png", width 150 ] []
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
                                , div [] [ button [ type_ "submit" ] [ text "Join" ] ]
                                ]
                            , h1 [] [ text "Create a game" ]
                            , Html.button [ onClick HandleCreateGameButtonClick ] [ text "Create game" ]
                            ]

                    InGame _ game ->
                        div []
                            [ div [] [ text <| "Game ID: " ++ String.fromInt game.id ]
                            , div [] [ text <| "Join Code: " ++ game.joinCode ]
                            , div [] [ text <| "Players:" ]
                            , ul [] <|
                                (game.players
                                    |> Dict.values
                                    |> List.map
                                        (\player ->
                                            li [] [ text <| Maybe.withDefault "Anonymous" player.displayName ]
                                        )
                                )
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

                    EnteringLobby _ _ ->
                        text "Entering lobby"
                ]
            ]
        ]
    }
