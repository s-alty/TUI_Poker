-module(cards).
-compile([export_all]).
%% -export([make_deck/0, straights/1]).

-include("records.hrl").


make_deck() ->
    Suits = [clubs, diamonds, spades, hearts],
    Ranks = [ace, king, queen, jack, 10, 9, 8, 7, 6, 5, 4, 3, 2],
    lists:sort(fun (_, _) -> rand:uniform() > 0.5 end, [#card{suit=S, rank=R} || S <- Suits, R <- Ranks]).


predRank(ace) -> king;
predRank(king) -> queen;
predRank(queen) -> jack;
predRank(jack) -> 10;
predRank(2) -> ace;
predRank(N) -> N - 1.


%% a straight is a list of cards in descending order, all adjacent.
%% straights(Cards) computes all straights of length 5,
%% but assumes Cards are in descending order.
%% TODO need a special case for low aces without wraparound


straights(Cards) -> straights(5, Cards).

%% Straights of length 1 == every card.
straights(1, Cards) -> [ [Card] || Card <- Cards ];

straights(0, _) -> [[]];
straights(_, []) -> [];

straights(N, [ C | Cs ]) ->
    %% Some straights start with C, and some don't.
    straights(N, C#card.rank, [ C | Cs ])
    ++
    straights(N, Cs).

%% straights/3 finds nonempty straights with a given rank.
straights(_N, _HighRank, []) -> [];

straights(1, HighRank, Cards) ->
    [ [C] || C <- Cards, C#card.rank == HighRank ];

%% When we like the high card,
%% get all the slightly-smaller straights and extend them.
%% Also, drop it and keep looking.
straights(N, HighRank, [ C | Cs ]) when HighRank == C#card.rank ->
    [ [C | S] || S <- straights(N - 1, predRank(HighRank), Cs) ]
    ++
    straights(N, HighRank, Cs);

%% When we don't like the high card,
%% drop it and keep looking.
straights(N, HighRank, [ _ | Cs ]) -> straights(N, HighRank, Cs).


example() -> [
        #card{suit=clubs, rank=ace},
        #card{suit=clubs, rank=king},
        #card{suit=clubs, rank=queen},
        #card{suit=clubs, rank=jack},
        #card{suit=clubs, rank=10},
        #card{suit=clubs, rank=9}
    ].
main(_) ->
    io:format("~p~n", [straights(example())]).
