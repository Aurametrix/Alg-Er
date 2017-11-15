-module(rooster).

-export([stop/0, start/2]).

-type route() :: {atom(), string(), any(), list(map())}.
-type config() :: #{ip          => {integer(), integer(), integer(), integer()},
                    port        => integer(),
                    static_path => list(string()),
                    ssl         => any(),
                    ssl_opts    => any()}.

-type state() :: #{routes => list(route()),
                   middleware => list(map())}.

-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.

-spec start(config(), state()) -> 'ignore' | {'error', _} | {'ok', pid()}.
start(SrvConf, State) ->
  ensure_started(crypto),
  rooster_state:start_link([SrvConf, State]),
  application:start(rooster).

-spec stop() -> ok.
stop() ->
  application:stop(rooster).
  
%%% usage

% authentication

-export([auth/0]).

basic_auth(#{authorization := Auth} = Req) ->
  Authorizated = rooster_basic_auth:is_authorized(Auth, {"admin", "admin"}),
  case Authorizated of
    true ->
      Req;
    _ ->
      {break, {403, #{reason => <<"Acess Forbidden">>}}}
  end.

auth() ->
  #{name => auth,
    enter => fun basic_auth/1}.
  
%   SSL
  
#{port     => 8080,
  ssl      => {ssl, true},
  ssl_opts => {ssl_opts, [{certfile, "{PATH}/server_cert.pem"},
                          {keyfile, "{PATH}/server_key.pem"}]}}
