-record(player, {ref, name, current_bet=0, is_bb=false, is_sb=false, is_button=false}).

-record(card, {suit, rank}).

-record(classified_hand, {hole_cards, full_hand, classification, kicker}).
