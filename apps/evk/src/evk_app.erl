%%% @doc evk application module
%%%
%%% @end

-module (evk_app).
-author ('bourinov@gmail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    evk_sup:start_link().

stop(_State) ->
    ok.