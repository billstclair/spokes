# Spokes Server

This directory contains Elm code for the Spokes server.

Node.JS code, scripts, and instructions for building the server are in the [server](../../../server/) directory.

## Protocol

Messages are all JSON. There are two basic types:

    ["req","<msg>",{"<key>":"<value>",...}]
    ["rsp","<msg>",{"<key>":"<value>",...}]
    
`"req"` denotes a request message, sent from client to server. `"rsp"` denotes a response message, sent from server to clients. `<name>` is the message name. The `"<key>":"<value>"` pairs are specific to each message.

## Messages

If a literal is included as a parameter value, the parameter is optional, and the literal is the default value. If the literal value is a name surrounded by angle-brackets, the parameter is required.

### Basic game play

Message | Parameters | Description
------- | ---------- | -----------
`"req","new"` | `{"players":"2"}` | Start a new game. Error if a game is in play.
`"rsp","new"` | `{"gameid":"<gameid>"}` | Return the identifier for the new game.
`"req","join"` | `{"gameid":"<gameid>", "name":"<name>"}` | Join a game in progress.
`"rsp","join"` | `{"gameid":"<gameid>", "name":<name>", "number":"<n>"}` | Join successful. Return player number. This is sent to every already-joined player.
`"rsp","placephase"` | `{"gameid":"<gameid>", "turn":"<turn>", "resolver":"<n>"}` | Begin place phase with the given player as resolver. `<turn>` is the turn number.
`"req","place"` | `{"gameid":"<gameid>", "placement":"<placement>"}` | Place a stone. `<placement>` is as in the numbered boxes at the top of the client page, e.g. `WB2` or `BD13`.
`"rsp","place"` | `{"gameid":"<gameid>", "number":"<n>"}` | Acknowledge placement by player number `<n>`. Sent to all players.
`"rsp","placed"` | `{"gameid":"<gameid>", "placements":"<placements>"}` | Place phase complete. `<placements>` is a comma-separated list of placements, e.g. `WB2` or `BD13`. Sent to all players. Until receiving this, a player may send another `place` request to change his move.
`"req","resolve"` | `{"gameid":"<gameid>", "color":"<color>", "from":"<from>", "to":"<to>"}` | Move one stone or block. `<color>` is `black`, `white`, or `block`. `<from>` and `<to>` are node names, e.g. `B2` or `D13`.
`"rsp","resolve"` | `{"gameid":"<gameid>", "color":"<color>", "from":"<from>", "to":"<to>"}` | Acknowlege a resolution. Sent to all players.  When all resolutions are done, a `placephase` response will be sent.


### Errors

Message | Parameters | Description
------- | ---------- | -----------
`"req","undo"` | `{"gameid":"<gameid>", "req":"<req>"}` | The current resolver can send an undo message, to undo the last `resolve` or the last `placed` response. `<req>` is an exact copy of that message.
`"rsp","undo"` | `{"gameid":"<gameid>", "req":"<req>"}` | Echoed to all players to acknowledge the undo.
`"rsp","error" | {"req":"<req>", "id":"<id>", "text":"<text>"}` | Sent in response to an illegal or mal-formed request. `<req>` is the request. `<id>` is the machine-readable error identifier. `<text>` is additional, human-readable, information about the error.


### Chat

Message | Parameters | Description
------- | ---------- | -----------
`"req","chat"` | `{"gameid":"<gameid>", "text":"<text>"}` | Send a chat message. `<text>` is the message text.
`"rsp","chat"` | `{"gameid":"<gameid>", "text":"<text>", "number":"<n>"}` | Acknowledge a chat message. `<n>` is the number of the player who sent it. Sent to all players.
