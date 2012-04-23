## evk ##

Erlang library to access API of VK.com social network.

### Get VK API Request URL ###

To build VK API Request URL use the following function:

```erlang
RequestURl = evk:make_url(AppId, SecretKey, Method, MethodParams).
```

More detailed example:

```erlang
% Application ID
AppId = <<"100">>,
% Application Secret Key
AppSecretKey = <<"SecretKey">>,
% Building API request url
RequestURl = evk:make_url(AppId, AppSecretKey, ?GET_PROFILES, [{uids, <<"1">>}, {fields, <<"online">>}]).
% ...
% Call RequestURl and parse result
% PROFIT!
```

### Licence ###

MIT - http://en.wikipedia.org/wiki/MIT_License
