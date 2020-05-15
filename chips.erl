-module(chips).

-export([start/0, loop/0, increment/2, decrement/2]).

start() -> register(chip_counter, spawn(chips, loop, maps:new())).


increment(Player, Value) ->
    Ref = make_ref(),
    chip_counter ! {self(), Ref, {increment, Player, Value}},
    receive {Ref, Balance} -> Balance end.

decrement(Player, Value) ->
    Ref = make_ref(),
    chip_counter ! {self(), Ref, {decrement, Player, Value}},
    receive {Ref, Balance} -> Balance end.

loop(State) ->
    receive
        {From, Ref, {query, Player}} ->
            From ! {Ref, lookup_chip_count(Player, State)},
            loop(State);
        {From, Ref, {increment, Player, Value}} ->
            Old = lookup_chip_count(Player, State),
            Updated = Old + Value,
            State2 = state#{Player => Updated},
            From ! {Ref, Updated},
            loop(State2);
        {From, Ref, {decrement, Player, Value}} ->
            Old = lookup_chip_count(Player, State),
            case Old >= Value of
                true ->
                    Remainder = Old - Value,
                    State2 = state#{Player => Remainder},
                    From ! {Ref, Remainder},
                    loop(State2);
                false ->
                    From ! {Ref, error},
                    loop(State)
            end
    end.

lookup_chip_count(Value, Map) ->
    Result = maps:find(Value, Map),
    case Result of
        error -> 0;
        _ -> Result
    end.
