%%% @doc evk module
%%%
%%% @end
-module (evk).

-export([make_url/4]).

-include("evk.hrl").

-export([md5str/1]).

%% @doc Build url request to http://api.vk.com/
%% @param AppId - vkontakte application id
%% @param SecretKey - application  secret key
%% @param Method - method request
%% @param Params - list of param to request
%% @end
-spec make_url(integer(), binary() | maybe_improper_list(any(),binary() | []) | byte(), Method :: binary(), Params :: [{Key :: atom(), Value :: binary()}]) -> binary() | error.
make_url(_, _, <<"">>, _) ->
    error;
make_url(_, <<"">>, _, _) ->
    error;
make_url(<<"">>, _, _, _) ->
    error;
make_url(AppId, SecretKey, Method, Params) ->
    % Add app_id to params
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
    Md5Sig = list_to_binary(md5str(Sig)),
    % Add sig to params
    NewParams7 = lists:append(SortParams, [{sig, Md5Sig}]),
    % Build query
    list_to_binary(lists:flatten(?EVK_API_URL ++ "?" ++ params(NewParams7))).
    

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
                if
                    Key == message ->
                        atom_to_list(Key) ++ "=" ++ url_encode(binary_to_list(Value)) ++ "&";
                    true ->
                        atom_to_list(Key) ++ "=" ++ binary_to_list(Value) ++ "&"
                end;
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

%%
%% Taken from http://sacharya.com/md5-in-erlang/
%%
-spec md5str(Data :: string()) -> string().
md5str(Data) ->
    <<X:128/big-unsigned-integer>> = erlang:md5(Data),
    lists:flatten(io_lib:format("~32.16.0b", [X])).

%%
%% Yaws snippet (https://github.com/klacke/yaws)
%%
url_encode([H|T]) ->
	    if
	        H >= $a, $z >= H ->
	            [H|url_encode(T)];
	        H >= $A, $Z >= H ->
	            [H|url_encode(T)];
	        H >= $0, $9 >= H ->
	            [H|url_encode(T)];
	        H == $_; H == $.; H == $-; H == $/; H == $: ->
	            [H|url_encode(T)];
	        true ->
	            case integer_to_hex(H) of
	                [X, Y] ->
	                    [$%, X, Y | url_encode(T)];
	                [X] ->
	                    [$%, $0, X | url_encode(T)]
	            end
	     end;
	
	url_encode([]) ->
	    [].
	 
integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.
	
	
old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).
