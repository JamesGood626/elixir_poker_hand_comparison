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

  def card_order(card), do: @card_rankings |> Map.get(card)

  @doc """
  make_six_pairs([club: "8", club: "9", club: "A", club: "2"])
  [
    [club: "8", club: "9"],
    [club: "9", club: "A"],
    [club: "8", club: "2"],
    [club: "8", club: "A"],
    [club: "A", club: "2"],
    [club: "9", club: "2"]
  ]
  """
  def make_six_pairs(xs, acc \\ [])
  def make_six_pairs([_x | []], acc), do: acc
  def make_six_pairs([x | ys], []) do
    acc = for y <- ys, do: [x] ++ [y]
    make_six_pairs(ys, acc)
  end
  def make_six_pairs([x | ys], acc) do
    concat_acc = for y <- ys, do: [x] ++ [y]
    make_six_pairs(ys, acc ++ concat_acc)
  end

  @doc """
  Input:
  [
    {{:club, "10"}, [club: "8", club: "9", club: "A", club: "2"]},
    {{:club, "8"}, [club: "10", club: "9", club: "A", club: "2"]},
    {{:club, "9"}, [club: "10", club: "8", club: "A", club: "2"]},
    {{:club, "A"}, [club: "10", club: "8", club: "9", club: "2"]},
    {{:club, "2"}, [club: "10", club: "8", club: "9", club: "A"]}
  ]

  Output:
  [
    [
      [club: "10", club: "8", club: "9"],
      [club: "10", club: "8", club: "A"],
      [club: "10", club: "8", club: "2"],
      [club: "10", club: "9", club: "A"],
      [club: "10", club: "9", club: "2"],
      [club: "10", club: "A", club: "2"]
    ],
    [
      [club: "8", club: "10", club: "9"],
      [club: "8", club: "10", club: "A"],
      [club: "8", club: "10", club: "2"],
      [club: "8", club: "9", club: "A"],
      [club: "8", club: "9", club: "2"],
      [club: "8", club: "A", club: "2"]
    ],
    [
      [club: "9", club: "10", club: "8"],
      [club: "9", club: "10", club: "A"],
      [club: "9", club: "10", club: "2"],
      [club: "9", club: "8", club: "A"],
      [club: "9", club: "8", club: "2"],
      [club: "9", club: "A", club: "2"]
    ],
    [
      [club: "A", club: "10", club: "8"],
      [club: "A", club: "10", club: "9"],
      [club: "A", club: "10", club: "2"],
      [club: "A", club: "8", club: "9"],
      [club: "A", club: "8", club: "2"],
      [club: "A", club: "9", club: "2"]
    ],
    [
      [club: "2", club: "10", club: "8"],
      [club: "2", club: "10", club: "9"],
      [club: "2", club: "10", club: "A"],
      [club: "2", club: "8", club: "9"],
      [club: "2", club: "8", club: "A"],
      [club: "2", club: "9", club: "A"]
    ]
  ]
  """
  def make_multiple_six_pairs(xs) do
    xs
    |> Enum.map(fn ({x, xs}) ->
      xs
      |> Poker.make_six_pairs
      |> Enum.map(fn y -> [x | y] end)
    end)
  end

  @doc """
  Input:
  [{:club, "10"}, {:club, "8"}, {:club, "9"}, {:club, "A"}, {:club, "2"}]

  Output:
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

  @doc """
  result (a list of length 10):
  (5*4)/(2*1) = 10 # with factorial cancelation.
  [
    [club: "10", club: "2", club: "8"],
    [club: "10", club: "2", club: "9"],
    [club: "10", club: "2", club: "A"],
    [club: "10", club: "8", club: "9"],
    [club: "10", club: "8", club: "A"],
    [club: "10", club: "9", club: "A"],
    [club: "2", club: "8", club: "9"],
    [club: "2", club: "8", club: "A"],
    [club: "2", club: "9", club: "A"],
    [club: "8", club: "9", club: "A"]
  ]
  """
  def five_choose_three_combinations(cards) do
    cards
    |> Poker.make_removed_card_list()
    |> Poker.make_multiple_six_pairs
    |> Enum.flat_map(fn x -> x end)
    |> Enum.map(fn x -> MapSet.new(x) end)
    |> MapSet.new
    |> MapSet.to_list
    |> Enum.map(fn x -> MapSet.to_list(x) end)
  end

  def make_table_card_permutations(4, cards) do
    cards
    |> make_removed_card_list
    |> Enum.filter(fn ({_, xs}) -> xs end)
  end

  @doc """
  5 choose 3 permutations should be 60
  5 choose 3 combinations should be 10
  """
  def make_table_card_permutations(3, cards) do
    cards
    |> make_removed_card_list
    |> Enum.map(fn {card, cards} ->
      xs = [card | cards |> Enum.take(2)]
      ys = [card | cards |> Enum.drop(1) |> Enum.take(2)]
      zs = [card | cards |> Enum.drop(2) |> Enum.take(2)]
      [xs, ys, zs]
    end)
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
    if length(cards) !== 5 do raise "DEVELOPER ERROR: Wrong number of cards passed to check_suits/1" end

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

  TODO/LLO:
  ...this function is pissing me off right now.

  Thinking about the problem differently...
  iex(14)> xs |> Enum.map(fn ({_, rank}) -> rank end)
  ["A", "10", "2", "A"]

  # WHERE the "A" || "10" || "2" inside the predicate is the card split from the remaining 4 cards.

  # The case where we have a :three_of_a_kind
  iex(16)> xs |> Enum.map(fn ({_, rank}) -> rank end) |> Enum.filter(fn x -> x === "A" end)
  ["A", "A"]

  # The case where we have a :four_of_a_kind
  iex(19)> ["A", "10", "A", "A"] |> Enum.filter(fn x -> x === "A" end)
  ["A", "A", "A"]

  # The case where we have a :one_pair (and if our overall function returns two one_pairs... then we make it a :two_pair)
  iex(17)> xs |> Enum.map(fn ({_, rank}) -> rank end) |> Enum.filter(fn x -> x === "10" end)
  ["10"]

  # The case where we have "NO_PAIRS"
  iex(19)> ["A", "10", "A"] |> Enum.filter(fn x -> x === "2" end)
  []
  """
  def check_for_pairs(cards) do
    result =
      make_removed_card_list(cards)
      |> Enum.map(
          fn ({{_, x}, ys}) ->
              ys
              |> Enum.map(fn ({_, rank}) -> rank end)
              |> Enum.filter(fn y -> y === x end)
              |> case do
                [] ->
                  {:left, "NO_PAIRS"}
                [_, _, _, _] ->
                  {:right, {:four_of_a_kind, %{"card" => x}, cards}}
                [_, _, _] ->
                  {:right, {:three_of_a_kind, %{"card" => x}, cards}}
                [_] ->
                  {:right, {:one_pair, %{"card" => x}, cards}}
              end
          end
        )
        |> sequence

    # NOTE (this is to remove duplicates in the case of :one_pairs being made):
    # [
    #   right: {:one_pair, %{"card" => "10"},
    #    [hearts: "7", spades: "2", club: "10", club: "2", club: "10"]},
    #   right: {:one_pair, %{"card" => "2"},
    #    [hearts: "7", spades: "2", club: "10", club: "2", club: "10"]},
    #   right: {:one_pair, %{"card" => "10"},
    #    [hearts: "7", spades: "2", club: "10", club: "2", club: "10"]},
    #   right: {:one_pair, %{"card" => "2"},
    #    [hearts: "7", spades: "2", club: "10", club: "2", club: "10"]}
    # ]
    if ((result |> length) === 4) do
     result |> Enum.take(2)
    else
      result
    end
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

   @doc """
   input = [club: "10", club: "8", club: "9", club: "A", club: "2"]
   find_high_card(input)
   output: {:club, "A"}
   """
   def find_high_card(cards) do
    cards
    |> Enum.reduce(nil,fn
      (card, nil) -> card
      ({_, card_val1} = card, {_, card_val2} = acc) ->
        if card_order(card_val1) > card_order(card_val2) do card else acc end
      end)
   end

   # Function clauses to check for a (Royal Flush, Straight Flush, or Flush)
   def determine_made_hand(cards), do: determine_made_hand(cards, check_suits(cards))
   def determine_made_hand(cards, {:right, "ALL_SAME_SUIT"}) do
     case MapSet.equal?(make_rank_sequence(["A", "K", "Q", "J", "10"]), make_rank_sequence(cards)) do
       true ->
         {:right, {:royal_flush, nil, cards}}
       false ->
         cards_sequence = make_rank_sequence(cards)
         {_, card} = find_high_card(cards)
         case Enum.find(make_rank_sequences(@ranks), false, fn sequence -> MapSet.equal?(sequence, cards_sequence) end) do
          false ->
            {:right, {:flush, %{"high_card" => card}, cards}}
          # Enum.find returns the element that for which the conditional evaluates to true.
          _ ->
            {:right, {:straight_flush, %{"high_card" => card}, cards}}
         end
     end
   end

   # Function clauses to check for a (Straight)
   x = [right: {:one_pair, %{"card" => "A"}, [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]}]
   y = [right: {:one_pair, %{"card" => "A"}, [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]}]
   z = [right: {:one_pair, %{"card" => "10"},  [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]}]

   def determine_made_hand(cards, {:left, "NOT_ALL_SAME_SUIT"}) do
     xs = cards |> check_for_pairs

     case xs do
       [] ->
         determine_made_hand(cards, {:left, "NO_PAIRS"})
       [{:right, {:one_pair, %{"card" => x}, cards}}, {:right, {:one_pair, %{"card" => y}, _}} | []] ->
        if x === y do
          {:right, {:one_pair, %{"card" => x}, cards}}
        else
          {:right, {:two_pair, %{"high_pair" => if x > y do x else y end, "low_pair" => if x < y do x else y end}, cards}}
        end
       [{:right, {:three_of_a_kind, %{"card" => x}, cards}}, {:right, {:one_pair, %{"card" => y}, _}} | []] ->
          check_kind(:full_house, {:three_of_a_kind, x}, {:one_pair, y}, cards)
       [{:right, {:one_pair, %{"card" => y}, cards}}, {:right, {:three_of_a_kind, %{"card" => x}, _}} | []] ->
          check_kind(:full_house, {:three_of_a_kind, x}, {:one_pair, y}, cards)
       [x | []] -> x
       _ -> raise "DEVELOPER ERROR: check_kind/2 generated an multiple invalid :right results for checking for :four_of_a_kind, :three_of_a_kind, and :one_pair"
     end
   end

   def determine_made_hand(cards, {:left, "NO_PAIRS"}) do
     cards_sequence = make_rank_sequence(cards)
     case Enum.find(make_rank_sequences(cards), false, fn sequence -> MapSet.equal?(sequence, cards_sequence) end) do
       false ->
        {_, card} = find_high_card(cards)
        {:right, {:straight, %{"high_card" => card}, cards}}
       # Enum.find returns the element that for which the conditional evaluates to true.
       _ ->
        determine_made_hand(cards, {:left, "NO_STRAIGHT"})
     end
   end

   def determine_made_hand(cards, {:left, "NO_STRAIGHT"}) do
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

     {_, card_val} = card
     {:right, {:high_card, %{"card" => card_val}, cards}}
   end


   def determine_stronger_hand({card1, hand1}, {card2, hand2}) do
    if card_order(card1) === card_order(card2) do "TIE" end

    if card_order(card1) > card_order(card2) do
      hand1
    else
      hand2
    end
   end

   # NOTE: For comparing Full Houses and two pairs
   def determine_stronger_hand({primary1, secondary1, hand1}, {primary2, secondary2, hand2}) do
    if card_order(primary1) === card_order(primary2) do
      if card_order(secondary1) === card_order(secondary2) do
        "TIE"
      else
        # i.e. Aces over 10s would beat out Aces over 9s
        if card_order(secondary1) > card_order(secondary2) do
          hand1
        else
          hand2
        end
      end
    end

    if card_order(primary1) > card_order(primary2) do
      hand1
    else
      hand2
    end
   end

  # (1) Royal Flush -> A, K, Q, J, 10, all the same suit
  def determine_stronger_hand({:royal_flush, _, _} = hand1, {:royal_flush, _, _} = hand2), do: "TIE"
  def determine_stronger_hand({:royal_flush, _, _} = winning_hand, _), do: winning_hand
  def determine_stronger_hand(_, {:royal_flush, _, _} = winning_hand), do: winning_hand


  # (2) Straight flush -> Five cards in a sequence, all in the same suit
  def determine_stronger_hand({:straight_flush, %{"high_card" => high_card1}, _} = hand1, {:straight_flush, %{"high_card" => high_card2}, _} = hand2) do
   determine_stronger_hand({high_card1, hand1}, {high_card2, hand2})
  end

  def determine_stronger_hand({:straight_flush, _, _} = winning_hand, _), do: winning_hand
  def determine_stronger_hand(_, {:straight_flush, _, _} = winning_hand), do: winning_hand

  # (3) Four of a kind -> All four cards in the same rank.
  def determine_stronger_hand({:four_of_a_kind, %{"card" => card1}, _} = hand1, {:four_of_a_kind, %{"card" => card2}, _} = hand2) do
    determine_stronger_hand({card1, hand1}, {card2, hand2})
  end

  def determine_stronger_hand({:four_of_a_kind, _, _} = winning_hand, _), do: winning_hand
  def determine_stronger_hand(_, {:four_of_a_kind, _, _} = winning_hand), do: winning_hand

  # (4) Full house -> Three of a king with a pair (i.e. 10 10 10 9 9 10s over 9s; beats 10 10 10 9 9)
  def determine_stronger_hand({:full_house, %{"primary" => primary1, "secondary" => secondary1}, _} = hand1, {:full_house, %{"primary" => primary2, "secondary" => secondary2}, _} = hand2) do
    determine_stronger_hand({primary1, secondary1, hand1}, {primary2, secondary2, hand2})
  end

  # (5) Flush -> Any five cards of the same suit, but not in a sequence
  def determine_stronger_hand({:flush, %{"high_card" => high_card1}, _} = hand1, {:flush, %{"high_card" => high_card2}, _} = hand2) do
    determine_stronger_hand({high_card1, hand1}, {high_card2, hand2})
  end

  # (6) Straight -> Five cards in a sequence, but not of the same suit
  def determine_stronger_hand({:straight, %{"high_card" => high_card1}, _} = hand1, {:straight, %{"high_card" => high_card2}, _} = hand2) do
    determine_stronger_hand({high_card1, hand1}, {high_card2, hand2})
  end

  def determine_stronger_hand({:straight, _, _} = winning_hand, _), do: winning_hand
  def determine_stronger_hand(_, {:straight, _, _} = winning_hand), do: winning_hand

  # (7) Three of a Kind -> Three cards of the same rank
  def determine_stronger_hand({:three_of_a_kind, %{"card" => card1}, _} = hand1, {:three_of_a_kind, %{"card" => card2}, _} = hand2) do
    determine_stronger_hand({card1, hand1}, {card2, hand2})
   end

  def determine_stronger_hand({:three_of_a_kind, _, _} = winning_hand, _), do: winning_hand
  def determine_stronger_hand(_, {:three_of_a_kind, _, _} = winning_hand), do: winning_hand

  # (8) Two pair -> Two different pairs
  def determine_stronger_hand({:two_pair, %{"high_pair" => high_pair1, "low_pair" => low_pair1}, _} = hand1, {:two_pair, %{"high_pair" => high_pair2, "low_pair" => low_pair2}, _} = hand2) do
    determine_stronger_hand({high_pair1, low_pair1, hand1}, {high_pair2, low_pair2, hand2})
  end

  def determine_stronger_hand({:two_pair, _, _} = winning_hand, _), do: winning_hand
  def determine_stronger_hand(_, {:two_pair, _, _} = winning_hand), do: winning_hand

  # (9) Pair -> Two cards of the same rank
  def determine_stronger_hand({:one_pair, %{"card" => card1}, _} = hand1, {:one_pair, %{"card" => card2}, _} = hand2) do
    determine_stronger_hand({card1, hand1}, {card2, hand2})
  end

  def determine_stronger_hand({:one_pair, _, _} = winning_hand, _), do: winning_hand
  def determine_stronger_hand(_, {:one_pair, _, _} = winning_hand), do: winning_hand

  # (10) High Card -> When you haven't made any of the hands above, the highest card plays
  def determine_stronger_hand({:high_card, %{"card" => card1}} = hand1, {:high_card, %{"card" => card2} = hand2}) do
    determine_stronger_hand({card1, hand1}, {card2, hand2})
  end

  @doc """
  To be used by the function (final_showdown) that runs reduce over a list of this structure:
  [
    {1, {:high_card, %{"card" => "A"}, [spades: "10", club: "8", club: "9", club: "A", club: "2"]}},
    {1, {:one_pair, %{"card" => "10"}, [spades: "10", club: "10", club: "8", club: "9", club: "A"]}},
    {1, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}, [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]}},
    {9, {:high_card, %{"card" => "A"}, [spades: "2", club: "10", club: "8", club: "9", club: "A"]}},
    {9, {:one_pair, %{"card" => "2"}, [spades: "2", club: "10", club: "8", club: "9", club: "2"]}}
  ]
  where {seat, hand}
  """
  def single_showdown({seat1, hand1} = pair1, {seat2, hand2} = pair2) do
    case determine_stronger_hand(hand1, hand2) do
      "TIE" -> [pair1, pair2]
      stronger_hand ->
        if stronger_hand === hand1 do
          pair1
        else
          pair2
        end
    end
  end

  @doc """
  input = [
    {1, {:high_card, %{"card" => "A"}, [spades: "10", club: "8", club: "9", club: "A", club: "2"]}},
    {1, {:one_pair, %{"card" => "10"}, [spades: "10", club: "10", club: "8", club: "9", club: "A"]}},
    {1, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}, [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]}},
    {9, {:high_card, %{"card" => "A"}, [spades: "2", club: "10", club: "8", club: "9", club: "A"]}},
    {9, {:one_pair, %{"card" => "2"}, [spades: "2", club: "10", club: "8", club: "9", club: "2"]}}
  ]
  """
  def final_showdown(all_player_made_hands) do
    all_player_made_hands
    |> Enum.reduce(nil, fn ({seat, current_hand}, acc) ->
      cond do
        acc === nil -> {seat, current_hand}
        # If acc is list that means we've had a "TIE" in a previous comparison
        is_list(acc) ->
          [{seat, prev_hand}, _] = acc
          result = single_showdown({seat, current_hand}, {seat, prev_hand})
          if result === {seat, prev_hand} do acc else result end
        true -> single_showdown({seat, current_hand}, acc)
      end
    end)
  end

  @doc """
  # ALMOST EFFING FINISHED!
  # Below is the data structure that you'll be using to determine which player has the strongest hand...
  [
    %{
      "made_hands" => %{
        "flush" => nil,
        "four_of_a_kind" => nil,
        "full_house" => nil,
        "high_card" => {:high_card, %{"card" => "A"}, [spades: "10", club: "8", club: "9", club: "A", club: "2"]},
        "one_pair" => {:one_pair, %{"card" => "10"}, [spades: "10", club: "10", club: "8", club: "9", club: "A"]},
        "royal_flush" => nil,
        "straight" => nil,
        "straight_flush" => nil,
        "three_of_a_kind" => nil,
        "two_pair" => {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}, [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]}
      },
      "seat" => 1
    },
    %{
      "made_hands" => %{
        "flush" => nil,
        "four_of_a_kind" => nil,
        "full_house" => nil,
        "high_card" => {:high_card, %{"card" => "A"}, [spades: "2", club: "10", club: "8", club: "9", club: "A"]},
        "one_pair" => {:one_pair, %{"card" => "2"}, [spades: "2", club: "10", club: "8", club: "9", club: "2"]},
        "royal_flush" => nil,
        "straight" => nil,
        "straight_flush" => nil,
        "three_of_a_kind" => nil,
        "two_pair" => nil
      },
      "seat" => 9
    }
  ]

  # 1. Iterate over the above data structure and transform it into:
  [
    {1, {:high_card, %{"card" => "A"}, [spades: "10", club: "8", club: "9", club: "A", club: "2"]}},
    {1, {:one_pair, %{"card" => "10"}, [spades: "10", club: "10", club: "8", club: "9", club: "A"]}},
    {1, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}, [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]}},
    {9, {:high_card, %{"card" => "A"}, [spades: "2", club: "10", club: "8", club: "9", club: "A"]}},
    {9, {:one_pair, %{"card" => "2"}, [spades: "2", club: "10", club: "8", club: "9", club: "2"]}}
  ]

  # 2. And determining the best hand of all the players just comes down to reducing over the list and storing the best {seat, hand} pair at each comparison
  #    in the acc value.
  """
  def convert_made_hands_for_showdown(all_player_hands) do
    all_player_hands
    |> Enum.map(fn %{"made_hands" => made_hands, "seat" => seat} ->
      made_hands
      |> Map.keys
      |> Enum.reduce([], fn (key, acc) ->
        hand = Map.get(made_hands, key)
        if hand === nil do acc else [{seat, hand} | acc] end
      end)
    end)
    # flattens a nested list ["a", ["c", "b"]] to ["a", "c", "b"]
    |> Enum.flat_map(fn x -> x end)
  end

  # (1) Royal Flush -> A, K, Q, J, 10, all the same suit
  # (2) Straight flush -> Five cards in a sequence, all in the same suit
  # (3) Four of a kind -> All four cards in the same rank.
  # (4) Full house -> Three of a king with a pair (i.e. 10 10 10 9 9 10s over 9s; beats 10 10 10 9 9)
  # (5) Flush -> Any five cards of the same suit, but not in a sequence
  # (6) Straight -> Five cards in a sequence, but not of the same suit
  # (7) Three of a Kind -> Three cards of the same rank
  # (8) Two pair -> Two different pairs
  # (9) Pair -> Two cards of the same rank
  # (10) High Card -> When you haven't made any of the hands above, the highest card plays

   @doc """

   input = [
     NOTE: The first two elements of the list are duplicates, want to remove them:
     {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}},
      [hearts: "A", spades: "10", club: "10", club: "8", club: "A"]},
     {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}},
      [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]}
   ]

   output = %{
     "royal_flush" => nil,
     "straight_flush" => nil,
     "four_of_a_kind" => nil,
     "full_house" => nil,
     "flush" => nil,
     "straight" => nil,
     "three_of_a_kind" => nil,
     "two_pair" => nil,
     "one_pair" => nil,
     "high_card" => nil
   }

   Two cases:
   1. In the event that the hands are duplicate, we just keep the pair that's currently stored on the final output map.
   2. In the event that the hands are the same type (i.e. :two_pair), we invoke a function with the function signature (:two_pair, hand1, hand2)
      That will determine which is the stronger hand and return it. <- (this function can be used in the final showdown hand comparison logic too...)

   """
   def filter_gratuitous_made_hands(made_hands) do
    x = %{
      "royal_flush" => nil,
      "straight_flush" => nil,
      "four_of_a_kind" => nil,
      "full_house" => nil,
      "flush" => nil,
      "straight" => nil,
      "three_of_a_kind" => nil,
      "two_pair" => nil,
      "one_pair" => nil,
      "high_card" => nil
    }

    made_hands
    |> Enum.reduce(x, fn ({:right, {hand_type, _, _} = hand}, acc) ->
      hand_type_str = Atom.to_string(hand_type)
      currently_stored_hand = Map.get(acc, hand_type_str)
      if currently_stored_hand === nil do
        Map.put(acc, hand_type_str, hand)
      else
        stronger_hand = determine_stronger_hand(hand, currently_stored_hand)
        # NOTE: This check is due to the fact that I overlooked returning the cards which make up the hand from the detemine_stronger_hand/2 function...
        cond do
          stronger_hand === hand -> acc
          stronger_hand === "TIE" -> acc
          true -> Map.put(acc, hand_type_str, hand)
        end
      end
    end)
   end


   @doc """
  Patterns for creating the array of possible_made_hands
  3 cards from table + 2 cards from player hand
  4 cards from table + 1 card from player hand
  5 cards from table (in the case where no other hands were made from the two
                      possible patterns above... AND there's a possible made hand
                      on the table that is ANYTHING OTHER than a High Card... i.e. Flush, Full House, etc..)
  Input:
  players = [
    %{"seat" => 1, "hand" => [{:hearts, "A"}, {:spades, "10"}]},
    %{"seat" => 9, "hand" => [{:hearts, "7"}, {:spades, "2"}]}
  ]
  table_cards = [club: "10", club: "8", club: "9", club: "A", club: "2"]

  Output:
  [
    %{
      "possible_made_hands" => [
        [hearts: "A", spades: "10", club: "10", club: "2", club: "8"],
        [hearts: "A", spades: "10", club: "10", club: "2", club: "9"],
        [hearts: "A", spades: "10", club: "10", club: "2", club: "A"],
        [hearts: "A", spades: "10", club: "10", club: "8", club: "9"],
        [hearts: "A", spades: "10", club: "10", club: "8", club: "A"],
        [hearts: "A", spades: "10", club: "10", club: "9", club: "A"],
        [hearts: "A", spades: "10", club: "2", club: "8", club: "9"],
        [hearts: "A", spades: "10", club: "2", club: "8", club: "A"],
        [hearts: "A", spades: "10", club: "2", club: "9", club: "A"],
        [hearts: "A", spades: "10", club: "8", club: "9", club: "A"],
        [hearts: "A", club: "8", club: "9", club: "A", club: "2"],
        [hearts: "A", club: "10", club: "9", club: "A", club: "2"],
        [hearts: "A", club: "10", club: "8", club: "A", club: "2"],
        [hearts: "A", club: "10", club: "8", club: "9", club: "2"],
        [hearts: "A", club: "10", club: "8", club: "9", club: "A"],
        [spades: "10", club: "8", club: "9", club: "A", club: "2"],
        [spades: "10", club: "10", club: "9", club: "A", club: "2"],
        [spades: "10", club: "10", club: "8", club: "A", club: "2"],
        [spades: "10", club: "10", club: "8", club: "9", club: "2"],
        [spades: "10", club: "10", club: "8", club: "9", club: "A"]
      ],
      "seat" => 1
    },
    %{
      "made_hands" => [
        [hearts: "7", spades: "2", club: "10", club: "2", club: "8"],
        [hearts: "7", spades: "2", club: "10", club: "2", club: "9"],
        [hearts: "7", spades: "2", club: "10", club: "2", club: "A"],
        [hearts: "7", spades: "2", club: "10", club: "8", club: "9"],
        [hearts: "7", spades: "2", club: "10", club: "8", club: "A"],
        [hearts: "7", spades: "2", club: "10", club: "9", club: "A"],
        [hearts: "7", spades: "2", club: "2", club: "8", club: "9"],
        [hearts: "7", spades: "2", club: "2", club: "8", club: "A"],
        [hearts: "7", spades: "2", club: "2", club: "9", club: "A"],
        [hearts: "7", spades: "2", club: "8", club: "9", club: "A"],
        [hearts: "7", club: "8", club: "9", club: "A", club: "2"],
        [hearts: "7", club: "10", club: "9", club: "A", club: "2"],
        [hearts: "7", club: "10", club: "8", club: "A", club: "2"],
        [hearts: "7", club: "10", club: "8", club: "9", club: "2"],
        [hearts: "7", club: "10", club: "8", club: "9", club: "A"],
        [spades: "2", club: "8", club: "9", club: "A", club: "2"],
        [spades: "2", club: "10", club: "9", club: "A", club: "2"],
        [spades: "2", club: "10", club: "8", club: "A", club: "2"],
        [spades: "2", club: "10", club: "8", club: "9", club: "2"],
        [spades: "2", club: "10", club: "8", club: "9", club: "A"]
      ],
      "seat" => 9
    }
  ]
  """
  def make_all_possible_player_hands(table_cards, players) do
    # 1. Map over every player... extracting the player's hand
    # 2. Inside of the Map (over each player, create an array that )
    players
    |> Enum.map(fn %{"seat" => seat, "hand" => hand} ->
      # 1. get all possible 3 card permutations from the table_cards, then append the player's hand
      # IMMEDIATE TODO: Finish the implementation for mark_table_card_permutations(3, _)
      three_card_permutations = five_choose_three_combinations(table_cards)
      xs = three_card_permutations |> Enum.map(fn xs -> [Enum.at(hand, 0) | [Enum.at(hand, 1) | xs]] end)

      # 2. get all possible 4 card permutations from the table_cards, then create two lists from that
      #    with one of the player's cards being appended to each table_card 4 permutations in a separate list.
      four_card_permutations = make_table_card_permutations(4, table_cards)
      ys = four_card_permutations |> Enum.map(fn ({_ ,xs}) -> [Enum.at(hand, 0) | xs] end)
      zs = four_card_permutations |> Enum.map(fn ({_ ,xs}) -> [Enum.at(hand, 1) | xs] end)
      %{"seat" => seat, "possible_made_hands" => xs ++ ys ++ zs}
    end)
  end

  @doc """
  Input:
  The output of make_all_possible_player_hands/2

  Output:

  """
  def determine_all_made_player_hands(xs) do
    xs
    |> Enum.map(fn %{"seat" => seat, "possible_made_hands" => possible_made_hands} ->
      IO.puts("********Seat #/#{seat} ***********************\n")
      made_hands =
        possible_made_hands
        |> Enum.map(fn cards ->
          # IO.puts("Input cards: ")
          # IO.inspect(cards)

          result = determine_made_hand(cards)

          # IO.puts("Result of determine_made_hand: ")
          # IO.inspect(result)
          # IO.puts("\n")
        end)
        |> filter_gratuitous_made_hands()
        |> IO.inspect
        # |> TODO: function which will eliminate multiple similar :one_pair/:two_pair/:three_of_a_kind/:four_of_a_kind
        #          if the cards that make them up are the same.


      IO.puts("********************************")
      %{"seat" => seat, "made_hands" => made_hands}
    end)
    |> convert_made_hands_for_showdown
  end

  # determine_made_hands input/output:
  # [hearts: "A", spades: "10", club: "10", club: "2", club: "8"] <- erroneous result (yields :two_pair, when it should just be pair)

  # ^^ So this is the primary issue:
  # 1. Should be yielding :one_pair
  # 2. If there's multiple :one_pair of the same type (due to the different permutations being generated),
  #    Then you want to just ignore the duplicates.

  # Gets it right in this case though:
  # input = [hearts: "A", spades: "10", club: "10", club: "2", club: "A"]
  # Result of determine_made_hand:
  # {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}},
  #   [hearts: "A", spades: "10", club: "10", club: "2", club: "A"]}

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

######################################################################
# Next task after determine_made_hand has been implemented
# Create a function that will take in the table cards and an array of players
# make_all_possible_player_hands(table_cards, %{ "seat" => Int, "hand" => [Card, Card] })

# And then return a structure like this:
# %{
#  "seat" => Int,
#  "possible_made_hands" => [All possible 5 card combinations using the 5 table cards + 2 player cards]
# }

# Patterns for creating the array of possible_made_hands
# 4 cards from table + 1 card from player hand
# 3 cards from table + 2 cards from player hand
# 5 cards from table (in the case where no other hands were made from the two
#                     possible patterns above... AND there's a possible made hand
#                     on the table that is ANYTHING OTHER than a High Card... i.e. Flush, FUll House, etc..)
######################################################################

xs = [club: "10", club: "8", club: "9", club: "A", club: "2"]
# Poker.make_table_card_permutations(3, xs) |> Enum.flat_map(fn x -> x end) |> IO.inspect
# |> IO.inspect()

# Poker.flatten()

# TODO/LLO: Need to determine how to get these duduped...
# There are five extra three card permutations than necessary... (just the nature of the beast)
# iex(43)> zs |> Enum.dedup
# [
#   [club: "2", club: "9", club: "A"],
#   [club: "2", club: "8", club: "9"],
#   [club: "2", club: "10", club: "8"],
#   [club: "A", club: "9", club: "2"],
#   [club: "A", club: "8", club: "9"],
#   [club: "A", club: "10", club: "8"],
#   [club: "9", club: "A", club: "2"],
#   [club: "9", club: "8", club: "A"],
#   [club: "9", club: "10", club: "8"],
#   [club: "8", club: "A", club: "2"],
#   [club: "8", club: "9", club: "A"],
#   [club: "8", club: "10", club: "9"],
#   [club: "10", club: "A", club: "2"],
#   [club: "10", club: "9", club: "A"],
#   [club: "10", club: "8", club: "9"]
# ]
# iex(44)> zs |> length
# 15
# iex(45)> x = [club: "8", club: "10", club: "9"]
# [club: "8", club: "10", club: "9"]
# iex(46)> y = [club: "10", club: "8", club: "9"]
# [club: "10", club: "8", club: "9"]
# iex(47)> x === y
# false
# iex(48)> x == y
# false

# SOLUTION:

# Map over every resulting permutation and do this
# iex(51)> [club: "10", club: "8", club: "9"] |> Enum.map(fn x -> x |> Tuple.to_list end)
# [[:club, "10"], [:club, "8"], [:club, "9"]]
# iex(55)> [[:club, "10"], [:club, "8"], [:club, "9"]] === [[:club, "10"], [:club, "8"], [:club, "9"]]
# true


#iex(57)> transform = fn x -> x |> Enum.map(fn x -> x |> Tuple.to_list end) |> Enum.dedup end
# #Function<44.97283095/1 in :erl_eval.expr/5>
# iex(58)> zs |> Enum.map(transform) |> Enum.dedup                                [
#   [[:club, "2"], [:club, "9"], [:club, "A"]],
#   [[:club, "2"], [:club, "8"], [:club, "9"]],
#   [[:club, "2"], [:club, "10"], [:club, "8"]],
#   [[:club, "A"], [:club, "9"], [:club, "2"]],
#   [[:club, "A"], [:club, "8"], [:club, "9"]],
#   [[:club, "A"], [:club, "10"], [:club, "8"]],
#   [[:club, "9"], [:club, "A"], [:club, "2"]],
#   [[:club, "9"], [:club, "8"], [:club, "A"]],
#   [[:club, "9"], [:club, "10"], [:club, "8"]],
#   [[:club, "8"], [:club, "A"], [:club, "2"]],
#   [[:club, "8"], [:club, "9"], [:club, "A"]],
#   [[:club, "8"], [:club, "10"], [:club, "9"]],
#   [[:club, "10"], [:club, "A"], [:club, "2"]],
#   [[:club, "10"], [:club, "9"], [:club, "A"]],
#   [[:club, "10"], [:club, "8"], [:club, "9"]]
# ]
# iex(59)> zs |> Enum.map(transform)

# where xs is the flattened list of Poker.make_table_card_permutations(3, _)
# xs
# |> Enum.map(transform)
# |> Enum.reduce(MapSet.new([]), fn (x, acc) -> acc |> MapSet.put(MapSet.new(x)) end)
# |> MapSet.to_list
# |> Enum.map(fn x -> x |> MapSet.to_list end)

# ^^ This is close, but will need to replace dedup with a custom implemented function.
# BUT it only works if the elements of the two respective lists are in the same order...
# x = [[:club, "10"], [:club, "8"], [:club, "9"]]
# y = [[:club, "9"], [:club, "10"], [:club, "8"]]
# iex(15)> x === [[:club, "10"], [:club, "8"], [:club, "9"]]
# true
# iex(16)> x === y
# false
# iex(17)> x
# [[:club, "10"], [:club, "8"], [:club, "9"]]
# iex(18)> y
# [[:club, "9"], [:club, "10"], [:club, "8"]]

# So, this will require that this representation of three ards be translated into another data structure which
# can more easily be tested for equality... and then transformed back into the representation it was before the equality test.

# Poker.make_six_pairs([club: "8", club: "9", club: "A", club: "2"]) |> IO.inspect
players = [
  %{"seat" => 1, "hand" => [{:hearts, "A"}, {:spades, "10"}]}, # Should only have.... a :two_pair "A" & "10" and high_card "A" as made_hands results...
  %{"seat" => 9, "hand" => [{:hearts, "7"}, {:spades, "2"}]} # Should only have.... High card "A" and one_pair "2" as made_hands results...
]
table_cards = [club: "10", club: "8", club: "9", club: "A", club: "2"]
Poker.make_all_possible_player_hands(table_cards, players) |> Poker.determine_all_made_player_hands |> IO.inspect |> Poker.final_showdown |> IO.inspect

# TODO/LLO (This is all effed up...)
# The Output:
# [
#   %{
#     "made_hands" => [
#       {:right, {:two_pair, %{"high_pair" => "10", "low_pair" => "10"}}, [hearts: "A", spades: "10", club: "10", club: "2", club: "8"]},
#       {:right, {:two_pair, %{"high_pair" => "10", "low_pair" => "10"}}, [hearts: "A", spades: "10", club: "10", club: "2", club: "9"]},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}}, [hearts: "A", spades: "10", club: "10", club: "2", club: "A"]},
#       {:right, {:two_pair, %{"high_pair" => "10", "low_pair" => "10"}}, [hearts: "A", spades: "10", club: "10", club: "8", club: "9"]},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}}, [hearts: "A", spades: "10", club: "10", club: "8", club: "A"]},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "10"}}, [hearts: "A", spades: "10", club: "10", club: "9", club: "A"]},
#       {:right, {:high_card, %{"card" => {:hearts, "A"}}, [hearts: "A", spades: "10", club: "2", club: "8", club: "9"]}},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "A"}}, [hearts: "A", spades: "10", club: "2", club: "8", club: "A"]},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "A"}}, [hearts: "A", spades: "10", club: "2", club: "9", club: "A"]},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "A"}}, [hearts: "A", spades: "10", club: "8", club: "9", club: "A"]},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "A"}}, [hearts: "A", club: "8", club: "9", club: "A", club: "2"]},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "A"}}, [hearts: "A", club: "10", club: "9", club: "A", club: "2"]},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "A"}}, [hearts: "A", club: "10", club: "8", club: "A", club: "2"]},
#       {:right, {:high_card, %{"card" => {:hearts, "A"}}, [hearts: "A", club: "10", club: "8", club: "9", club: "2"]}},
#       {:right, {:two_pair, %{"high_pair" => "A", "low_pair" => "A"}}, [hearts: "A", club: "10", club: "8", club: "9", club: "A"]},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [spades: "10", club: "8", club: "9", club: "A", club: "2"]}},
#       {:right, {:two_pair, %{"high_pair" => "10", "low_pair" => "10"}}, [spades: "10", club: "10", club: "9", club: "A", club: "2"]},
#       {:right, {:two_pair, %{"high_pair" => "10", "low_pair" => "10"}}, [spades: "10", club: "10", club: "8", club: "A", club: "2"]},
#       {:right, {:two_pair, %{"high_pair" => "10", "low_pair" => "10"}}, [spades: "10", club: "10", club: "8", club: "9", club: "2"]},
#       {:right, {:two_pair, %{"high_pair" => "10", "low_pair" => "10"}}, [spades: "10", club: "10", club: "8", club: "9", club: "A"]}
#     ],
#     "seat" => 1
#   },
#   %{
#     "made_hands" => [
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [hearts: "7", spades: "2", club: "10", club: "2", club: "8"]},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [hearts: "7", spades: "2", club: "10", club: "2", club: "9"]},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [hearts: "7", spades: "2", club: "10", club: "2", club: "A"]},
#       {:right, {:high_card, %{"card" => {:club, "10"}}, [hearts: "7", spades: "2", club: "10", club: "8", club: "9"]}},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [hearts: "7", spades: "2", club: "10", club: "8", club: "A"]}},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [hearts: "7", spades: "2", club: "10", club: "9", club: "A"]}},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [hearts: "7", spades: "2", club: "2", club: "8", club: "9"]},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [hearts: "7", spades: "2", club: "2", club: "8", club: "A"]},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [hearts: "7", spades: "2", club: "2", club: "9", club: "A"]},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [hearts: "7", spades: "2", club: "8", club: "9", club: "A"]}},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [hearts: "7", club: "8", club: "9", club: "A", club: "2"]}},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [hearts: "7", club: "10", club: "9", club: "A", club: "2"]}},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [hearts: "7", club: "10", club: "8", club: "A", club: "2"]}},
#       {:right, {:high_card, %{"card" => {:club, "10"}}, [hearts: "7", club: "10", club: "8", club: "9", club: "2"]}},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [hearts: "7", club: "10", club: "8", club: "9", club: "A"]}},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [spades: "2", club: "8", club: "9", club: "A", club: "2"]},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [spades: "2", club: "10", club: "9", club: "A", club: "2"]},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [spades: "2", club: "10", club: "8", club: "A", club: "2"]},
#       {:right, {:two_pair, %{"high_pair" => "2", "low_pair" => "2"}}, [spades: "2", club: "10", club: "8", club: "9", club: "2"]},
#       {:right, {:high_card, %{"card" => {:club, "A"}}, [spades: "2", club: "10", club: "8", club: "9", club: "A"]}}
#     ],
#     "seat" => 9
#   }
# ]
