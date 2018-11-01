-module(record_unsafe_union@foreign).
-export([unsafeUnionFn/0]).

% Actually safe
unsafeUnionFn() -> fun (R1, R2) -> maps:merge(R2, R1) end.
