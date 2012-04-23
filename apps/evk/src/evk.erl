%%% @doc evk main interface module
%%%
%%% @end
-module (evk).
-author ('bourinov@gmail.com').

-include ("evk_log.hrl").

-export([start/0]).

-export([rl/0, rl/1]).


%% @doc evk startup function which starts whole poker application
-spec start() -> ok.
start() ->
    [application:start(App) || App <- [compiler, syntax_tools, lager, ?MODULE]],
    ok.

%% @doc Reload whole application
-spec rl() -> ok.
rl() ->
    rl(?MODULE).

%% @doc Reload whole application specified by AppModuleName — name
%%      of module which belongs to desired application
%% @end
-spec rl(AppModuleName :: atom()) -> ok.
rl(AppModuleName) ->
    {ok, App} = application:get_application(AppModuleName),
    {ok, Keys} = application:get_all_key(App),
    Modules = case lists:keysearch(modules, 1, Keys) of
        {value, {modules, Mods}} -> 
            Mods;
        _ -> 
            []
    end,
    Reload = fun(Module) ->
        code:purge(Module),
        code:load_file(Module),
        Module
    end,
    Res = [Reload(Mod) || Mod <- Modules],
    ?INFO("Reloaded: ~p", [Res]),
    ok.
