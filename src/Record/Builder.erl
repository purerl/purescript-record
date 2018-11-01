-module(record_builder@foreign).
-export([copyRecord/1, unsafeInsert/3, unsafeModify/3, unsafeDelete/2, unsafeRename/3]).

% Note that record refers to the PureScript type, which are represented by maps.

% Don't need to copy, maps are immutable
copyRecord(Rec) -> Rec.

% Actually safe
unsafeInsert(Ls, A, Rec) -> Rec#{ to_label(Ls) => A }.

% Actually safe
unsafeModify(Ls, F, Rec) ->
  L = to_label(Ls),
  #{ L := V } = Rec,  
  Rec#{ L := F(V) }.

% Actually safe
unsafeDelete(Ls, Rec) -> maps:remove(to_label(Ls), Rec).

% Actually safe
unsafeRename(L1s, L2s, Rec) ->
  L1 = to_label(L1s),
  L2 = to_label(L2s),
  #{ L1 := V } = Rec,
  Rec1 = maps:remove(L1, Rec),
  Rec1#{ L2 => V }.

%% Private Util

to_label(S) -> binary_to_atom(S, utf8).
