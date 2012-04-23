%%% @doc evk module
%%%
%%% @end
-module (evk).

-export([make_url/4]).

-type params() :: [{Key :: atom(), Value :: binary()}].

-define(API_URL, "http://api.vk.com/api.php").

%%
%% secure.* methods
%%
-define(SECURE_SEND_NOTIFICATION, <<"secure.sendNotification">>).
-define(SECURE_GET_APP_BALANCE, <<"secure.getAppBalance">>).
-define(SECURE_GET_BALANCE, <<"secure.getBalance">>).
-define(SECURE_WITHDRAW_VOTES, <<"secure.withdrawVotes">>).
-define(SECURE_GET_TRANSACTIONS_HISTORY, <<"secure.getTransactionsHistory">>).
-define(SECURE_ADD_RATING, <<"secure.addRating">>).
-define(SECURE_SET_COUNTER, <<"secure.setCounter">>).

%%
-define(GET_PROFILES, <<"getProfiles">>).

%% @doc Build url request to http://api.vk.com/
%% @param AppId - vkontakte application id
%% @param SecretKey - application  secret key
%% @param Method - method request
%% @param Params - list of param to request
%% @end
-spec make_url(integer(), binary() | maybe_improper_list(any(),binary() | []) | byte(), Method :: binary(), Params :: params()) -> binary().
make_url(AppId, SecretKey, Method, Params) ->
    % Add api_id to params
    NewParams1 = lists:append([{api_id, AppId}], Params),
    % Add version to params
    NewParams2 = lists:append([{v, <<"3.0">>}], NewParams1),
    % Add method to params
    NewParams3 = lists:append([{method, Method}], NewParams2),
    % Get timestamp
    Timestamp = get_timestamp(),
    % Add timestamp to params
    NewParams4 = lists:append([{timestamp, Timestamp}], NewParams3),
    % Add format to params
    NewParams5 = lists:append([{format, <<"json">>}], NewParams4),
    % Add random to params
    NewParams6 = lists:append([{random, list_to_binary(integer_to_list(random:uniform(10000)))}], NewParams5),
    % Sort params by alphabetical
    SortParams = lists:keysort(1, NewParams6),
    % Make sig string
    AppendedList = lists:map(fun(Data) ->
        % Get key and value
        {Key, Value} = Data,
        % Build sig
        atom_to_list(Key) ++ "=" ++ binary_to_list(Value)
    end, SortParams),

    % Flat this list and add SeckertKey to the end of sig
    Sig = lists:append(lists:flatten(AppendedList), binary_to_list(SecretKey)),
    % Convert sig to md5
    Md5Sig = erlang:md5(Sig),
    % Add sig to params
    NewParams7 = lists:append(SortParams, [{sig, Md5Sig}]),
    % Build query
    list_to_binary(lists:flatten(?API_URL ++ "?" ++ params(NewParams7))).

%% @doc Add symbol between params
%% @end
-spec params(Params :: [{atom, binary()}, ...]) -> string().
params(Params) ->
    Url = lists:map(fun(Data) ->
        % Get key and value
        {Key, Value} = Data,
        % Check key
        if
            Key /= sig ->
                atom_to_list(Key) ++ "=" ++ binary_to_list(Value) ++ "&";
            true ->
                atom_to_list(Key) ++ "=" ++ binary_to_list(Value)
        end
    end, Params),

    lists:flatten(Url).

%% @doc Get time stamp
%% @end
-spec get_timestamp() -> binary().
get_timestamp() ->
    {M, S, _} = now(),
    list_to_binary(io_lib:format("~p", [M * 1000000 + S])).




