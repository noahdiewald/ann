port module Interface exposing
    ( receivedSelection
    , requestSelection
    )

import Json.Encode as E


port requestDocSelection : () -> Cmd msg


port receivedDocSelection : (String -> msg) -> Sub msg


requestSelection : () -> Cmd msg
requestSelection =
    requestDocSelection


receivedSelection : (String -> msg) -> Sub msg
receivedSelection =
    receivedDocSelection
