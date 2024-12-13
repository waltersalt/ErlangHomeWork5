-module(cache_test).
-author("soldotenko").
-include_lib("eunit/include/eunit.hrl").
-import(my_cache, [create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).

create_test() ->
  TableName = my_cache:create(test_table),
  ?assertEqual(test_table, TableName).

insert_and_lookup_test() ->
  my_cache:insert(test_table, "key1", "value1"),
  ?assertEqual({ok, "value1"}, my_cache:lookup(test_table, "key1")).

insert_with_expiry_test() ->
  my_cache:insert(test_table, "key2", "value2", 2),
  timer:sleep(3000),
  ?assertEqual(undefined, my_cache:lookup(test_table, "key2")).

delete_obsolete_test() ->
  ets:delete_all_objects(test_table),
  my_cache:insert(test_table, "key3", "value3", 1),
  my_cache:insert(test_table, "key4", "value4"),
  timer:sleep(2000),
  my_cache:delete_obsolete(test_table),
  ?assertEqual([{"key4", "value4", undefined}], ets:tab2list(test_table)).