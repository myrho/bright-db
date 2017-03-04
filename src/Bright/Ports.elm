port module Bright.Ports exposing (..)

import Task exposing (Task)
import Graph exposing (Uri, Predicate)
import Bright.DB exposing (Object)
import Json.Decode as Dec
import Json.Encode as Enc


port appLoad : Enc.Value -> Cmd msg


{-| send remoteOperations to another peer
-}
port send : Enc.Value -> Cmd msg


{-| receive subscriptions from other peers
   receive remoteOperations
-}
port receive : (Dec.Value -> msg) -> Sub msg


port appSave : (Dec.Value -> msg) -> Sub msg


{-| save app changeset to bright
-}
port save : Enc.Value -> Cmd msg


{-| load bright changes into app
-}
port load : (Dec.Value -> msg) -> Sub msg


port localQuery : Enc.Value -> Cmd msg


port appReceive : (Dec.Value -> msg) -> Sub msg
