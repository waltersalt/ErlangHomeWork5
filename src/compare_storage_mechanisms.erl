-module(compare_storage_mechanisms).
-author("soldatenko").
-export([compare_storage_mechanisms/0, compare_map/0, compare_proplist/0, compare_dict/0, compare_record/0, compare_ets/0, compare_dets/0]).

-record(my_record, {key, value}).

compare_map() ->
  Map = #{"key" => "value"},
  maps:put("new_key", "new_value", Map),
  maps:get("key", Map).

compare_proplist() ->
  Proplist = [{"key", "value"}],
  NewProplist = [{"new_key", "new_value"} | Proplist],
  proplists:get_value("key", NewProplist).


compare_dict() ->
  Dict = dict:new(),
  Dict1 = dict:store("key", "value", Dict),
  dict:fetch("key", Dict1).

compare_record() ->
  Record = #my_record{key = "key", value = "value"},
  Record.

compare_ets() ->
  ets:new(my_table, [named_table, public]),
  ets:insert(my_table, {key, "value"}),
  ets:lookup(my_table, key).

compare_dets() ->
  dets:open_file(my_table, [{file, "my_table.dets"}]),
  dets:insert(my_table, {key, "value"}),
  dets:lookup(my_table, key).

compare_storage_mechanisms() ->
  MapResult = timer:tc(fun() -> compare_map() end),
  ProplistResult = timer:tc(fun() -> compare_proplist() end),
  DictResult = timer:tc(fun() -> compare_dict() end),
  RecordResult = timer:tc(fun() -> compare_record() end),
  EtsResult = timer:tc(fun() -> compare_ets() end),
  DetsResult = timer:tc(fun() -> compare_dets() end),

  io:format("Map Test: ~p~n", [MapResult]),
  io:format("Proplist Test: ~p~n", [ProplistResult]),
  io:format("Dict Test: ~p~n", [DictResult]),
  io:format("Record Test: ~p~n", [RecordResult]),
  io:format("ETS Test: ~p~n", [EtsResult]),
  io:format("DETS Test: ~p~n", [DetsResult]),

  [MapResult, ProplistResult, DictResult, RecordResult, EtsResult, DetsResult].
