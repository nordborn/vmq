-module(vmq_backend_behavior).

-author("vladimirb").

-callback topic_new(vmq:topic()) -> ok.
-callback topic_clean(vmq:topic()) -> ok.
-callback topic_delete(vmq:topic()) -> ok.
-callback topic_length(vmq:topic()) -> non_neg_integer().

-callback put(vmq:topic(), vmq:data()) -> ok.
-callback put_many(vmq:topic(), [vmq:data()]) -> ok.
-callback consume(vmq:topic()) -> vmq:data() | '$empty'.
-callback consume_all(vmq:topic()) -> [vmq:data()].
