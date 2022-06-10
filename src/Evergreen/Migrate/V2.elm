module Evergreen.Migrate.V2 exposing (..)

import Dict
import Evergreen.V1.Types as Old
import Evergreen.V2.Types as New
import Frontend
import Lamdera.Migrations exposing (..)
import Random
import Sessions


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { key = old.key, state = New.Unconnected }, Cmd.none )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { lobbies = Dict.empty
          , seed = Random.initialSeed 0
          , playerIdNonce = 0
          , lobbyIdNonce = 0
          , sessions = Sessions.create
          , playerData = Dict.empty
          }
        , Cmd.none
        )


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgMigrated ( New.NoOpFrontendMsg, Cmd.none )


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgMigrated ( New.NoOpToBackend, Cmd.none )


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgMigrated ( New.NoOpBackendMsg, Cmd.none )


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgMigrated ( New.NoOpToFrontend, Cmd.none )
