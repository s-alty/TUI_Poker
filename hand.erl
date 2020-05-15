-module(hand).

-include("records.hrl").
-define(SB, 100).
-define(BB, 200).

start(Table) -> spawn(?MODULE, hand_loop, [Table, 0]).

hand_loop(Table, HandNumber) ->
    Players = table:get_players(Table),
    [#player{ref=BBPlayer}] = lists:filter(fun(P) -> P#player.is_bb end, Players),
    chips:decrement(BBPlayer, BB), %% todo if they can't cover we need to kick them and restart the hand
    [#player{ref=SBPlayer}] = lists:filter(fun(P) -> P#player.is_sb end, Players),
    chips:decrement(SBPlayer, SB),

    Deck = cards:make_deck(),
    {Hands, Deck1} = deal_hands(Players),
    lists:foreach(fun({Ref, Cards}) -> table ! {send_update, Ref, {hole_cards, Cards}} end, Hands),

    OrderedPlayers = utils:rotate_about(fun(P) -> P#player.is_bb end, Players)
    {Players1, Pot} = handle_betting(Table, OrderedPlayers, [], BB, BB+SB),
    case Players1 of
        [Winner] ->
            #player{ref=Ref} = Winner,
            Table ! {boadcast, {winner, Winner}},
            chips:increment(Ref, Pot),
            hand_loop(Table, HandNumber + 1);
            _ -> 0
    end,

    [Burn1,Flop1,Flop2,Flop3|Deck2] = Deck1,
    table ! {broadcast, {flop, [Flop1, Flop2, Flop3]}},
    {Players2, Pot2} = handle_betting(Table, postflop_order(Players, Players1), [], 0, Pot1),
    case Players2 of
        [Winner] ->
            #player{ref=Ref} = Winner,
            Table ! {boadcast, {winner, Winner}},
            chips:increment(Ref, Pot2),
            hand_loop(Table, HandNumber + 1);
            _ -> 0
    end,


    [Burn2,Turn|Deck3] = Deck2,
    table ! {broadcast, {turn, Turn}}
    {Players3, Pot3} = handle_betting(Table, postflop_order(Players, Players2), [], 0, Pot2),
    case Players3 of
        [Winner] ->
            #player{ref=Ref} = Winner,
            Table ! {boadcast, {winner, Winner}},
            chips:increment(Ref, Pot3),
            hand_loop(Table, HandNumber + 1);
            _ -> 0
    end,


    [Burn3,River|_] = Deck3,
    table ! {broadcast, {river, River}},
    {Players4, Pot4} = handle_betting(Table, postflop_order(Players, Players3), [], 0, Pot3),
    case Players4 of
        [Winner] ->
            #player{ref=Ref} = Winner,
            Table ! {boadcast, {winner, Winner}},
            chips:increment(Ref, Pot4),
            hand_loop(Table, HandNumber + 1);
            _ -> 0
    end,

    % handle_showdown
    % recurse
    loop(Table, HandNumber + 1).

handle_betting(Table, [], PlayersInvolved, _, PotSize) -> {PlayersInHand, PotSize};
handle_betting(Table, [Player|PlayersLeftToAct], PlayersInvolved, BetSize, PotSize) ->
    #player{ref=Ref, current_bet=Current} = Player,
    Table ! {send_update, Ref, action},
    receive
        {Ref, fold} ->
            PlayersLeftToAct1 = PlayersLeftToAct,
            PlayersInvolved1 = PlayersInvolved,
            BetSize1 = BetSize,
            PotSize1 = PotSize;
        {Ref, call} ->
            AmountToContribute = BetSize - Current,
            case chips:decrement(Ref, AmountToContribute) of
                error -> %% TODO how to handle this?
                    _ -> ok
            end,
            UpdatedPlayer = Player#player{current_bet=Betsize},
            PlayersLeftToAct1 = PlayersLeftToAct,
            PlayerdInvolved1 = PlayersInvolved ++ UpdatedPlayer,
            BetSize1 = BetSize,
            PotSize1 = PotSize + AmountContributed;
        {Ref, raise, Amount} ->
            %% TODO: check that Amount is 2x bigger than current BetSize
            AmountToContribute = Amount - Current,
            case chips:decrement(Ref, AmountToContribute) of
                error -> %% TODO how to handle this?;
                    _ -> ok
            end,
            UpdatedPlayer = Player#player{current_bet=Amount},
            PlayersLeftToAct1 = PlayersLeftToAct ++ PlayersInvolved,
            PlayersInvolved1 = [UpdatedPlayer],
            BetSize1 = Amount,
            PotSize1 = PotSize + AmountContributed;
    after 60000 ->
            PlayersLeftToAct1 = PlayersLeftToAct,
            PlayersInvolved1 = PlayersInvolved,
            BetSize1 = BetSize,
            PotSize1 = PotSize;
    end,
    %% broadcast outcome and recurse
    Table ! {broadcast, {update, Betsize1, PotSize1, PlayersInvolved1, PlayersLeftToAct1}},
    handle_betting(PlayersLeftToAct1, PlayersInvolved1, BetSize1, PotSize1).


handle_showdown(CommunityCards, Potsize, [], PlayersLeft) ->
    Hands = cards:best_hands(PlayersLeft).
% first player must show
% following players can either show or fold
% for all hands left divide pot amount best hand or hands


deal_hands(Deck, Players) ->
    lists:mapfoldl(fun(Player, Deck) ->
                           #player{ref=R} = Player,
                           [First,Second|Deck1] = Deck,
                           {{R, [First, Second]}, Deck1} end,
                   Deck, Players).



postflop_order(AllPlayers, PlayersStillInHand) ->
    RefsStillInHand = lists:map(fun(P) -> P#player.ref end, PlayersStillInHand),
    OrderedPlayers = utils:rotate_about(fun(P) -> P#player.is_button end, AllPlayers),
    lists:filter(fun(#player{ref=R}) -> lists:member(R, RefsStillInHand) end, OrderedPlayers).




%% Poker logic todo:
%% 0. sidepots
%% 1. minimum bet / raise sizes
%% 2. straddles
