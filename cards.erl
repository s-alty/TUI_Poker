-module(cards).
-export([make_deck/0]).

-include("records.hrl").

make_deck() ->
    Suits = [clubs, diamonds, spades, hearts],
    Ranks = [ace, king, queen, jack, 10, 9, 8, 7, 6, 5, 4, 3, 2],
    lists:sort(fun (_, _) -> rand:uniform() > 0.5 end, [#card{suit=S, rank=R} || S <- Suits, R <- Ranks]).
