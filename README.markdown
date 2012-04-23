## App evk ##

## Get request url usage: ##

To build url request call ```evk:make_url(AppId, SecretKey, Method, MethodParams).```

```erlang
evk:make_url(<<"100">>, <<"SecretKey">>, <<"getProfiles">>, [{uids, <<"100">>}, {fields, <<"online">>}]).
```

## Licence ##

MIT - http://en.wikipedia.org/wiki/MIT_License
