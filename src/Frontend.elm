module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
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
      , state = OutOfGame "JKLM"
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
                OutOfGame _ ->
                    ( { model
                        | state = OutOfGame newJoinCode
                      }
                    , Cmd.none
                    )

                _ ->
                    noOp

        HandleJoinCodeSubmit ->
            case model.state of
                OutOfGame joinCode ->
                    ( { model
                        | state = ConnectingToGame
                      }
                    , Lamdera.sendToBackend (JoinGame joinCode)
                    )

                _ ->
                    noOp

        HandleCreateGameButtonClick ->
            case model.state of
                OutOfGame _ ->
                    ( { model
                        | state = ConnectingToGame
                      }
                    , Lamdera.sendToBackend CreateGame
                    )

                _ ->
                    noOp

        HandleNameInput newName ->
            case model.state of
                NamingPlayer _ gameState ->
                    ( { model | state = NamingPlayer newName gameState }, Cmd.none )

                _ ->
                    noOp

        HandleNameSubmit ->
            case model.state of
                NamingPlayer name gameState ->
                    ( { model | state = EnteringLobby gameState }, Lamdera.sendToBackend (NamePlayer gameState.id name) )

                _ ->
                    noOp


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model
            , Cmd.none
            )

        UpdateGame game ->
            ( { model | state = NamingPlayer "" game }
            , Cmd.none
            )


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
                    OutOfGame joinCode ->
                        div []
                            [ h1 [] [ text "Join a game" ]
                            , Html.form [ onSubmit HandleJoinCodeSubmit ]
                                [ input [ onInput HandleJoinCodeInput, value joinCode ] []
                                ]
                            , h1 [] [ text "Create a game" ]
                            , Html.button [ onClick HandleCreateGameButtonClick ] [ text "Create game" ]
                            ]

                    InGame game ->
                        div []
                            [ div [] [ text "In game" ]
                            , div [] [ text <| "Join Code: " ++ game.joinCode ]
                            , div [] [ text <| "Players: " ++ String.fromInt (Dict.keys game.players |> List.length) ]
                            ]

                    ConnectingToGame ->
                        text "Connecting to game"

                    NamingPlayer name gameState ->
                        div []
                            [ h1 [] [ text "My name is" ]
                            , Html.form [ onSubmit HandleNameSubmit ]
                                [ input [ onInput HandleNameInput, value name ] []
                                ]
                            ]

                    EnteringLobby _ ->
                        text "Entering lobby"
                ]
            ]
        ]
    }
