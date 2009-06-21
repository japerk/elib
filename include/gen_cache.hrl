% if app has defined CACHE, then use as name
-ifdef(CACHE).
-define(recall(M, F, A), gen_cache:recall_apply(?CACHE, M, F, A)).
-else.
-define(recall(Name, M, F, A), gen_cache:recall_apply(Name, M, F, A)).
-endif.
