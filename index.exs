defmodule Poker do
  @moduledoc """
  Best hand ranking:
  (1) Royal Flush -> A, K, Q, J, 10, all the same suit
  (2) Straight flush -> Five cards in a sequence, all in the same suit
  (3) Four of a kind -> All four cards in the same rank.
  (4) Full house -> Three of a king with a pair (i.e. 10 10 10 9 9 10s over 9s; beats 10 10 10 9 9)
  (5) Flush -> Any five cards of the same suit, but not in a sequence
  (6) Straight -> Five cards in a sequence, but not of the same suit
  (7) Three of a Kind -> Three cards of the same rank
  (8) Two pair -> Two different pairs
  (9) Pair -> Two cards of the same rank
  (10) High Card -> When you haven't made any of the hands above, the highest card plays
  """

  # Create guards for determining if a certain hand was possibly made?
  # Guard that checks if all cards have the same suit (handles (1), (3), and (5))
  #  -> Could possibly be a Royal Flush, so next check (inside the multiclause function)
  #     would need to see if A, K, Q, J, 10 are in hand, or if there's any other sequence
  #     of cards, in which case it'll be a Straight Flush, otherwise it's just a Flush.

  # Guard that checks whether there are four cards of the same rank (handles (3))
  # Guard that checks whether there are three cards of the same rank (handles (7))
  # Guard that checks whether there are two cards of the same rank (handles (9))
  # Guard that checks whether there are two pairs (double up on the previous guard?) (handles (8))
  # Guard which performs similar checks as would be done inside the multiclause function
  # that follows after the first Guard (above) has passed (handles (6))

  # Guard that checks for three cards of the same rank AND two cards of the same rank (used
  # in conjunction with each other; AND must appear above the two other multiclause functions)
  # to detect a Full House (handles case (4))

  @card_rankings %{
    "A" => 14,
    "K" => 13,
    "Q" => 12,
    "J" => 11,
    "10" => 10,
    "9" => 9,
    "8" => 8,
    "7" => 7,
    "6" => 6,
    "5" => 5,
    "4" => 4,
    "3" => 3,
    "2" => 2
   }

   @table_game_state %{
     "players" => [
       %{ "seat" => 1, "hand" => [{:heart, "10"}, {:club, "10"}] },
       %{ "seat" => 4, "hand" => [{:diamond, "9"}, {:spade, "10"}] }
     ],
     "table_cards" => [
       {:diamond, "10"},
       {:spade, "9"},
       {:club, "9"},
       {:club, "A"},
       {:club, "2"},
     ]
   }

   @ranks ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
   @suits [:heart, :spade, :diamond, :club]

   # Sequence in the Monadic sense
   def sequence(xs) do
     xs
     |> Enum.reduce([], fn ({:right, x}, acc) -> [{:right, x} | acc]
                           ({:left, _}, acc) -> acc end)
   end

   @doc """
   cards = [{:club, "10"}, {:club, "8"}, {:club, "9"}, {:club, "A"}, {:club, "2"}]
   make_removed_card_list(cards)

   RESULT:
   [
     { {:club, "10"}, [{:club, "8"}, {:club, "9"}, {:club, "A"}, {:club, "2"}] },
     { {:club, "8"}, [{:club, "10"}, {:club, "9"}, {:club, "A"}, {:club, "2"}] },
     { {:club, "9"}, [{:club, "10"}, {:club, "8"}, {:club, "A"}, {:club, "2"}] },
     { {:club, "A"}, [{:club, "10"}, {:club, "8"}, {:club, "9"}, {:club, "2"}] },
     { {:club, "2"}, [{:club, "10"}, {:club, "8"}, {:club, "9"}, {:club, "A"}] }
   ]
   """
   def make_removed_card_list(cards) do
     1..5
     |> Enum.map(fn _ -> cards end)
     |> Enum.with_index
     |> Enum.map(fn {[a | xs], 0} -> {a, xs}
                    {[a, b | xs], 1} -> {b, [a | xs]}
                    {[a, b, c | xs], 2} -> {c, [a, b | xs]}
                    {[a, b, c, d | xs], 3} -> {d, [a, b, c | xs]}
                    {[a, b, c, d | [e]], 4} -> {e, [a, b, c, d]} end)
   end


   def make_deck(), do: for rank <- @ranks, suit <- @suits, do: {suit, rank}
   def make_shuffled_deck(), do: make_deck() |> Enum.shuffle

   @doc """
     EXAMPLE USAGE:

     Poker.check_suits([
       {:diamond, "10"},
       {:spade, "9"},
       {:club, "9"},
       {:club, "A"},
       {:club, "2"}
     ]) -> :nothing

     Poker.check_suits([
       {:club, "10"},
       {:club, "8"},
       {:club, "9"},
       {:club, "A"},
       {:club, "2"}
     ]) -> { :ok, {:all_same_suit} }
   """
   def check_suits([{a, _}, {b, _}, {c, _}, {d, _}, {e, _}] = cards) do
     if length(cards) !== 5 do raise "Wrong number of cards passed to check_suits/1" end

     case a === b && b === c && c === d && d === e do
       true ->  {:right, "ALL_SAME_SUIT"}
       false -> {:left, "NOT_ALL_SAME_SUIT"}
     end
   end

   def check_kind(:full_house, {:three_of_a_kind, x}, {:one_pair, y}, cards), do: {:right, {:full_house, %{"primary" => x, "secondary" => y}, cards}}
   def check_kind(:full_house, _, _, _), do: raise "Invalid input to check_kind(:full_house, _, _, _)"

   @doc """
   hand = [ {:club, "10"}, {:spade, "10"}, {:diamond, "10"}, {:club, "9"}, {:heart, "9"} ]
   Poker.check_for_pairs(hand)

   RESULT... if further processing isn't done after the first map
   [
      right: {:three_of_a_kind, %{"card" => "10"},
      [club: "10", spade: "10", diamond: "10", club: "9", heart: "9"]},
      right: {:three_of_a_kind, %{"card" => "10"},
      [club: "10", spade: "10", diamond: "10", club: "9", heart: "9"]},
      right: {:three_of_a_kind, %{"card" => "10"},
      [club: "10", spade: "10", diamond: "10", club: "9", heart: "9"]},
      right: {:one_pair, %{"card" => "9"},
      [club: "10", spade: "10", diamond: "10", club: "9", heart: "9"]},
      right: {:one_pair, %{"card" => "9"},
      [club: "10", spade: "10", diamond: "10", club: "9", heart: "9"]}
   ]

   can remove duplicates this way:
   iex(11)> Enum.dedup(xs)
   [
     right: {:three_of_a_kind, %{"card" => "10"},
      [club: "10", spade: "10", diamond: "10", club: "9", heart: "9"]},
     right: {:one_pair, %{"card" => "9"},
      [club: "10", spade: "10", diamond: "10", club: "9", heart: "9"]}
   ]
   """
   def check_for_pairs(cards) do
     make_removed_card_list(cards)
     |> Enum.map(
         fn ({{_, x}, ys}) ->
           ys
           |> Enum.reduce({:left, "NO_PAIRS"}, fn ({_, y}, acc) ->
             case {x === y, acc} do
               {true, {:left, _}} ->
                 {:right, {:one_pair, %{"card" => x}, cards}}
               {true, {:right, {:one_pair, _, _}}} ->
                 {:right, {:three_of_a_kind, %{"card" => x}, cards}}
               {true, {:right, {:three_of_a_kind, _, _}}} ->
                 {:right, {:four_of_a_kind, %{"card" => x}, cards}}
               _ ->
                acc
             end
           end)
         end
       )
       |> Enum.dedup
     # {:right, meta_data, cards}
     # where meta_data stores information regarding the card which formed the four_of_a_kind,
     # but meta_data will serve a more crucial function when it comes to resolving which player
     # has a better flush || straight || full house, etc...
     # Because in these situations we'll also need to know the high card of the made hand,
     # or in the case of full house which cards over which, i.e. 10s over 9s, which will beat 9s over 10s.
   end

   # def check_kind(:three_of_a_kind, cards), do: {:right, {:three_of_a_kind, cards}}
   # def check_kind(:one_pair, cards), do: {:right, {:one_pair, cards}}
   # def check_kind(:two_pair, cards) do

   #   # {:right, {:two_pair, %{"high_pair" => x, "low_pair" => y} cards}}
   # end

   # Function clauses to check for a (Royal Flush, Straight Flush, or Flush)
   def determine_made_hand(cards), do: determine_made_hand(cards, check_suits(cards))
   def determine_made_hand(cards, {:right, "ALL_SAME_SUIT"}) do
     case MapSet.equal?(make_rank_sequence(["A", "K", "Q", "J", "10"]), make_rank_sequence(cards)) do
       true ->
         {:right, {:royal_flush, cards}}
       false ->
         cards_sequence = make_rank_sequence(cards)
         case Enum.find(make_rank_sequences(@ranks), false, fn sequence -> MapSet.equal?(sequence, cards_sequence) end) do
          false ->
            {:right, {:flush, cards}}
          # Enum.find returns the element that for which the conditional evaluates to true.
          _ ->
            {:right, {:straight_flush, cards}}
         end
     end
   end

   # Function clauses to check for a (Straight)

   def determine_made_hand(cards, {:left, "NOT_ALL_SAME_SUIT"}) do
     xs = cards |> check_for_pairs |> sequence

     case xs do
       [] ->
         determine_made_hand(cards, {:left, "NO_PAIRS"})
       [x | []] -> x
       [{:right, {:one_pair, %{"card" => x}, cards}}, {:right, {:one_pair, %{"card" => y}, _}} | []] ->
          {:right, {:two_pair, %{"high_pair" => if x > y do x else y end, "low_pair" => if x < y do x else y end}}, cards}
       [{:right, {:three_of_a_kind, %{"card" => x}, cards}}, {:right, {:one_pair, %{"card" => y}, _}} | []] ->
          check_kind(:full_house, {:three_of_a_kind, x}, {:one_pair, y}, cards)
       [{:right, {:one_pair, %{"card" => y}, cards}}, {:right, {:three_of_a_kind, %{"card" => x}, _}} | []] ->
          check_kind(:full_house, {:three_of_a_kind, x}, {:one_pair, y}, cards)
       _ -> raise "DEVELOPER ERROR: check_kind/2 generated an multiple invalid :right results for checking for :four_of_a_kind, :three_of_a_kind, and :one_pair"
     end
   end

   def determine_made_hand(cards, {:left, "NO_PAIRS"}) do
     cards_sequence = make_rank_sequence(cards)
     case Enum.find(make_rank_sequences(cards), false, fn sequence -> MapSet.equal?(sequence, cards_sequence) end) do
       false ->
        {:right, {:straight, cards}}
       # Enum.find returns the element that for which the conditional evaluates to true.
       _ ->
        determine_made_hand(cards, {:left, "NO_STRAIGHT"})
     end
   end

   def determine_made_hand(cards, {:left, "NO_STRAIGHT"}) do
     # TODO/LLO
     card = cards
     |> Enum.reduce(nil, fn ({_, rank} = card, acc) ->
       cond do
        acc === nil ->
          card
        Map.get(@card_rankings, rank) > Map.get(@card_rankings, acc |> Tuple.to_list |> Enum.at(1)) ->
          card
        true ->
          acc
       end
     end)

     {:right, {:high_card, %{"card" => card}, cards}}
   end

   @doc """
   EXAMPLE USAGE:
   ranks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
   Poker.make_rank_sequences(ranks)
   Result:
   [
     #MapSet<["10", "A", "J", "K", "Q"]>,
     #MapSet<["10", "9", "J", "K", "Q"]>,
     #MapSet<["10", "8", "9", "J", "Q"]>,
     #MapSet<["10", "7", "8", "9", "J"]>,
     #MapSet<["10", "6", "7", "8", "9"]>,
     #MapSet<["5", "6", "7", "8", "9"]>,
     #MapSet<["4", "5", "6", "7", "8"]>,
     #MapSet<["3", "4", "5", "6", "7"]>,
     #MapSet<["2", "3", "4", "5", "6"]>,
     #MapSet<["2", "3", "4", "5", "A"]>
   ]
   """
   def make_rank_sequences([x | ranks]), do: make_rank_sequences(ranks ++ [x], [make_rank_sequence([x | ranks])])
   def make_rank_sequences([x | ranks], ys) do
     sequence = make_rank_sequence([x | ranks])
     case MapSet.size(sequence) do
       5 -> make_rank_sequences(ranks, [sequence | ys])
       _ -> ys
     end
   end

   @doc """
   EXAMPLE USAGE (for the case when a hand is passed in as an arg):

   The purpose of this particular clause of the function is to facilitate
   the comparison of a proper rank sequence via the MapSet Elixir lib function
   MapSet.equal?(map_set1, map_set2), which returns true | false

   Poker.make_rank_sequence([
     {:club, "10"},
     {:club, "8"},
     {:club, "9"},
     {:club, "A"},
     {:club, "2"}
   ])

   Result:
   #MapSet<["10", "2", "8", "9", "A"]>
   """
   def make_rank_sequence([{_, a}, {_, b}, {_, c}, {_, d}, {_, e}]), do: [a, b, c, d, e] |> MapSet.new
   def make_rank_sequence(ranks), do: Enum.take(ranks, 5) |> MapSet.new

  #  @doc """
  #  1. Create all possible combinations of players_cards and table_cards.
  #  2. Map over the list of all the possible combinations, invoking a multiclause
  #     function which performs pattern matching on the cards
  #  """
  #  def determine_made_hands(%{
  #    "player_cards" => player_cards,
  #    "table_cards" => table_cards
  #  }) do

  #  end

   # def make_table(%{
   #   "deck" => deck,
   #   "n_players" => n_players
   # }) do


   #   %{
   #     "remaining_deck" => [],
   #     "table_game_state" => %{
   #       "players" => %{},
   #       "table_cards" => []
   #     }
   #   }
   # end

end

# Poker.make_shuffled_deck() |> IO.puts
# Poker.check_suits([
#   {:club, "10"},
#   {:club, "8"},
#   {:club, "9"},
#   {:club, "A"},
#   {:club, "2"}
# ]) |> IO.puts

# ranks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
# IO.puts("make_rank_sequences/1 result:")
# Poker.make_rank_sequences(ranks) |> IO.inspect

# Poker.make_rank_sequence([
#   {:club, "10"},
#   {:club, "8"},
#   {:club, "9"},
#   {:club, "A"},
#   {:club, "2"}
# ]) |> IO.inspect

################################
# TESTING determine_made_hand/1 (Thinking I should just add %{} even for hand results which don't require metadata)
################################

# (1) Royal Flush -> A, K, Q, J, 10, all the same suit
# hand = [ {:heart, "A"}, {:heart, "K"}, {:heart, "Q"}, {:heart, "J"}, {:heart, "10"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# => {:right, {:royal_flush, [heart: "A", heart: "K", heart: "Q", heart: "J", heart: "10"]}}

# (2) Straight flush -> Five cards in a sequence, all in the same suit
# hand = [ {:club, "A"}, {:club, "2"}, {:club, "3"}, {:club, "4"}, {:club, "5"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# => {:right, {:straight_flush, [club: "A", club: "2", club: "3", club: "4", club: "5"]}}

# (3) Four of a kind -> All four cards in the same rank.
# hand = [ {:club, "10"}, {:heart, "10"}, {:spade, "10"}, {:diamond, "10"}, {:club, "2"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# => {:right, {:four_of_a_kind, %{"card" => "10"}, [club: "10", heart: "10", spade: "10", diamond: "10", club: "2"]}}

# (4) Full house -> Three of a king with a pair (i.e. 10 10 10 9 9 10s over 9s; beats 10 10 10 9 9)
# hand = [ {:club, "10"}, {:heart, "10"}, {:spade, "10"}, {:diamond, "9"}, {:club, "9"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# => {:right, {:full_house, %{"primary" => "10", "secondary" => "9"}, [club: "10", heart: "10", spade: "10", diamond: "9", club: "9"]}}

# (5) Flush -> Any five cards of the same suit, but not in a sequence
# hand = [ {:club, "10"}, {:club, "8"}, {:club, "9"}, {:club, "A"}, {:club, "2"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# => {:right, {:flush, [club: "10", club: "8", club: "9", club: "A", club: "2"]}}

# (6) Straight -> Five cards in a sequence, but not of the same suit
# hand = [ {:club, "7"}, {:heart, "6"}, {:spade, "5"}, {:club, "4"}, {:diamond, "3"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# => {:right, {:straight, [club: "7", heart: "6", spade: "5", club: "4", diamond: "3"]}}

# (7) Three of a Kind -> Three cards of the same rank
# hand = [ {:club, "7"}, {:heart, "7"}, {:spade, "7"}, {:club, "4"}, {:diamond, "3"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# => {:right, {:three_of_a_kind, %{"card" => "7"}, [club: "7", heart: "7", spade: "7", club: "4", diamond: "3"]}}

# (8) Two pair -> Two different pairs
# hand = [ {:club, "7"}, {:heart, "7"}, {:spade, "8"}, {:club, "8"}, {:diamond, "3"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# {:right, {:two_pair, %{"high_pair" => "8", "low_pair" => "7"}}, [club: "7", heart: "7", spade: "8", club: "8", diamond: "3"]}

# (9) Pair -> Two cards of the same rank
# hand = [ {:club, "7"}, {:heart, "7"}, {:spade, "8"}, {:club, "2"}, {:diamond, "3"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# => {:right, {:one_pair, %{"card" => "7"}, [club: "7", heart: "7", spade: "8", club: "2", diamond: "3"]}}

# (10) High Card -> When you haven't made any of the hands above, the highest card plays
# hand = [ {:club, "A"}, {:heart, "7"}, {:spade, "8"}, {:club, "2"}, {:diamond, "3"} ]
# Poker.determine_made_hand(hand) |> IO.inspect
# {:right, {:high_card, %{"card" => {:club, "A"}}, [club: "A", heart: "7", spade: "8", club: "2", diamond: "3"]}}
