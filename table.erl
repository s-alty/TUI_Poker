-module(table).

-include("records.hrl").
% We should use refs to indetify each table and each player

start() -> spawn(?MODULE, loop, [[], 0]).

get_players(Table) ->
    Table ! {self(), list_players},
    receive
        {Table, Players} -> Players
    end.


%% TODO: check for table size limit 10 players?
loop(Players) ->
    receive
        {Pid, join, Name} ->
            % keep track of the Pid alongside the player for sending updates
            NewPlayer = {Pid, #player{ref=make_ref(), name=Name}},
            Pid ! {joined, ok}
            loop([NewPlayer|Players]);
        {Pid, leave, Ref} ->
            loop(lists:filter(fun({P, #player{ref=R}}) -> P =:= Pid and R =:= Ref end, Players));
        {Pid, list_players} ->
            Pid ! {self(), Players},
            loop(Players);
        {send_update, For, Message} ->
            case lists:filter(fun({_, #player{ref=R}}) -> R =:= For end, Players) of
                [{Pid, _}|[]] -> Pid ! Message;
                 _ -> false %% player wasn't there
            end,
            loop(Players);
        {broadcast, Message} ->
            lists:map(fun({Pid, _}) -> Pid ! Message end, Players),
            loop(Players)
    end.
