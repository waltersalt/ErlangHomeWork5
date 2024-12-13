-module(cache).
-author("soldatenko").
-export([create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).

create(TableName) ->
  ets:new(TableName, [named_table, public, set]).

insert(TableName, Key, Value) ->
  ets:insert(TableName, {Key, Value, undefined}),
  ok.

insert(TableName, Key, Value, ExpiryTime) when is_integer(ExpiryTime) ->
  {{_, _, _}, {Hour, Minute, Second}} = calendar:local_time(),
  CurrentSeconds = calendar:time_to_seconds({Hour, Minute, Second}),
  ExpiryTimestamp = CurrentSeconds + ExpiryTime,
  ets:insert(TableName, {Key, Value, ExpiryTimestamp}),
  ok.

lookup(TableName, Key) ->
  case ets:lookup(TableName, Key) of
    [{Key, Value, Timestamp}] ->
      {{_, _, _}, {Hour, Minute, Second}} = calendar:local_time(),
      CurrentSeconds = calendar:time_to_seconds({Hour, Minute, Second}),
      if
        Timestamp == undefined orelse Timestamp > CurrentSeconds ->
          {ok, Value};
        true ->
          undefined
      end;
    [] -> undefined
  end.

delete_obsolete(TableName) ->
  {{_, _, _}, {Hour, Minute, Second}} = calendar:local_time(),
  CurrentSeconds = calendar:time_to_seconds({Hour, Minute, Second}),
  MatchSpec = [{{'_', '_', '$1'}, [{'<', '$1', CurrentSeconds}], ['true']}],
  ets:select_delete(TableName, MatchSpec),
  ok.