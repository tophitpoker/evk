%%% @doc evk defines
%%%
%%% @end

-ifndef(_EVK_H).
-define(_EVK_H, true).

%%
%% secure.* methods
%%
-define(EVK_SECURE_SEND_NOTIFICATION, <<"secure.sendNotification">>).
-define(EVK_SECURE_GET_APP_BALANCE, <<"secure.getAppBalance">>).
-define(EVK_SECURE_GET_BALANCE, <<"secure.getBalance">>).
-define(EVK_SECURE_WITHDRAW_VOTES, <<"secure.withdrawVotes">>).
-define(EVK_SECURE_GET_TRANSACTIONS_HISTORY, <<"secure.getTransactionsHistory">>).
-define(EVK_SECURE_ADD_RATING, <<"secure.addRating">>).
-define(EVK_SECURE_SET_COUNTER, <<"secure.setCounter">>).

%%
-define(EVK_GET_PROFILES, <<"getProfiles">>).

-define(EVK_API_URL, "http://api.vk.com/api.php").

-endif.  % _EVK_H
