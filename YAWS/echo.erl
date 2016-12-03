out(A) ->
    case get_upgrade_header(A#arg.headers) of
    undefined ->
        {content, "text/plain", "You're not a web sockets client! Go away!"};
    "WebSocket" ->
        WebSocketOwner = spawn(fun() -> websocket_owner() end),
        {websocket, WebSocketOwner, passive}
    end.
