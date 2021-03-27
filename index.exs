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

  def make_deck(), do: for rank <- @ranks, suit <- @suits, do: {suit, rank}
  def make_shuffled_deck(), do: make_deck() |> Enum.shuffle

  @doc """
    EXAMPLE USAGE:

    Poker.check_suits(5, [
      {:diamond, "10"},
      {:spade, "9"},
      {:club, "9"},
      {:club, "A"},
      {:club, "2"}
    ]) -> :nothing

    Poker.check_suits(5, [
      {:club, "10"},
      {:club, "8"},
      {:club, "9"},
      {:club, "A"},
      {:club, "2"}
    ]) -> { :ok, {:all_same_suit} }
  """
  def check_suits(5, [{a, _}, {b, _}, {c, _}, {d, _}, {e, _}] = hand) do
    case a === b && b === c && c === d && d === e do
      true ->  {:right, "ALL_SAME_SUIT"}
      false -> {:left, "NOT_ALL_SAME_SUIT"}
    end
  end

  def check_suits(5, _) do
    raise "Wrong number of cards passed to check_suits(5, _)"
  end

  # TODO: Add the card the matched in kind to the return result of the :right tuple
  def check_kind(4, cards) do
    {:left, "NOT_FOUR_OF_A_KIND"}
  end
  def check_kind(3, cards), do: {:right, {:three_of_a_kind, cards}}
  def check_kind(2, cards), do: {:right, {:one_pair, cards}}

  # Function clauses to check for a (Royal Flush, Straight Flush, or Flush)
  def determine_made_hand(cards), do: determine_made_hand(cards, check_suits(5, cards))
  def determine_made_hand(cards, {:right, "ALL_SAME_SUIT"}) do
    case MapSet.equal?(make_rank_sequence(["A", "K", "Q", "J", "10"]), make_rank_sequence(cards)) do
      true ->
        {:right, {:royal_flush, cards}}
      false ->
        cards_sequence = make_rank_sequence(cards)
        case Enum.find(make_rank_sequences(cards), false, fn sequence -> MapSet.equals?(sequence, cards_sequence) end) do
          true ->
            {:right, {:straight_flush, cards}}
          false ->
            {:right, {:flush, cards}}
        end
    end
  end

  # Function clauses to check for a (Straight)

  # Function clauses to check for a (Two Pair)

  # Function clauses to check for a (Four of a Kind, Three of a Kind, and One Pair)
  def determine_made_hand(cards, {:left, "NOT_ALL_SAME_SUIT"}) do
    xs = [check_kind(4, cards), check_kind(3, cards), check_kind(2, cards)]
    |> Enum.reduce([], fn ({:right, _}, acc) -> [x | acc]
                          ({:left, _}, acc) -> acc end)
    case length(xs) do
      1 -> Enum.at(xs, 0)
      _ -> raise "DEVELOPER ERROR: check_kind/2 generated multiple :right results for checking for :four_of_a_kind, :three_of_a_kind, and :one_pair"
    end

    # LLO: Working in here
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

  @doc """
  1. Create all possible combinations of players_cards and table_cards.
  2. Map over the list of all the possible combinations, invoking a multiclause
     function which performs pattern matching on the cards
  """
  def determine_made_hands(%{
    "player_cards" => player_cards,
    "table_cards" => table_cards
  }) do

  end

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
# Poker.check_suits(5, [
#   {:club, "10"},
#   {:club, "8"},
#   {:club, "9"},
#   {:club, "A"},
#   {:club, "2"}
# ]) |> IO.puts

# ranks = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
# IO.puts("make_rank_sequences/1 result:")
# Poker.make_rank_sequences(ranks) |> IO.inspect

Poker.make_rank_sequence([
  {:club, "10"},
  {:club, "8"},
  {:club, "9"},
  {:club, "A"},
  {:club, "2"}
]) |> IO.inspect
