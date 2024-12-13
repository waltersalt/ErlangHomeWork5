-module(compare_storage_mechanisms_test).
-include_lib("eunit/include/eunit.hrl").
-author("soldotenko").
-import(compare_storage_mechanisms, [compare_storage_mechanisms/0]).

compare_storage_mechanisms_test() ->
  io:format("Starting storage comparison tests...~n"),
  Results = compare_storage_mechanisms:compare_storage_mechanisms(),
  ?assert(length(Results) > 0).