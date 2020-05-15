-module(utils).

rotate_about(Fun, L) ->
    {value, Idx} = lists:search(Fun, L),
    {L2, L3} = lists:split(Idx, L),
    L3 ++ L2.
