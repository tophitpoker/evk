%%% @doc evk root supervisor
%%%
%%% @end
-module(evk_sup).

-include("evk_log.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(APP, evk).

%% API functions
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
    ?DBG("Start supervisor", []),

    IsPidEnable = appenv:get(?APP, enable_pid),
    if
        IsPidEnable ->
            % Store BEAM process identificator in file
            PidDir = appenv:get(?APP, pid_dir),
            PidName = atom_to_list(node()) ++ "." ++ "pid",
            FileName = filename:join([PidDir, PidName]),
            case file:open(FileName, [write]) of
                {ok, FileId} ->
                    io:fwrite(FileId, "~s~n", [os:getpid()]),
                    file:close(FileId);
                {error, Error} ->
                    ?ERR("Cannot write pid-file to ~p: ~p", [FileName, Error])
            end;
        true ->
            no_pid
    end,

    % Restart = permanent,
    % Shutdown = 2000,
    Children = [

    % {poker_eval_sup,
    %     {poker_eval_sup, start_link, []},
    %     Restart, Shutdown, supervisor, [?MODULE]},

    ],
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    {ok, { {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts}, Children} }.





