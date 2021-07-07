--  File       : Proj2.hs
--  Author     : Ankita Dhar <addh@student.unimelb.edu.au> <Student#: 1154197>
--  Purpose    : Play Card Hand Guessing Game with a human opponent

-- This file implements functions that define an agent that plays Card guessing game based on feedback from the `Answerer`. 
-- 
-- In this game, we consider complete standard deck of western playing cards (without jokers).
-- Number of players are two, an Answerer and a Guesser, both having a complete deck of cards for themselves.
-- For this game, a set of cards chosen by the players is refered to as a 'hand'
-- Game is as follows:
-- An 'Answerer' selects a hand of his choice of cards from his deck of 52 Cards. The 'Guesser', the role of this agent, is to
-- guess the Answerer's hand, via repeated guesses, informed by feedback from the Answerer.
-- The feedback given by the Answerer informs the guesser about the following :-
-- 1. Number of exact matches of the card
-- 2. Number of lower ranked cards, compared to the lowest card of Guesser's hand, in Answerer's hand
-- 3. Number of cards having exact ranks in the Guesser's hand as that of the Answerer's hand
-- 4. Number of higher ranked cards, compared to the highest card of Guesser's hand, in Answerer's hand
-- 5. Number of cards having exact suits in the Guesser's hand as that of the Answerer's hand
-- 
-- This agent solves the puzzle by generating all possible hands of the size given as input
-- and then prunes the list based on feedback such that size of possible hands reduces drastically 
-- and thus Answerer's hand can be guessed in minimum number of attempts.

module Proj2 (feedback, initialGuess, nextGuess, GameState) where

import Data.List ( (\\), delete, group, intersect, sort, sortBy )
import Data.Ord ( comparing )
import Card ( Card(Card), Rank, Suit ) 
-- Card is predefined data type comprising of Rank and Suit data type.
-- A Rank indicates rank of a card and are either of the following (mentioned in ascending order)
-- R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, Jack, Queen, King, Ace.
-- A Suit indicates suit of a card and are either of the following
-- Club, Diamond, Heart, Spade.

-- | GameState is simply a list of remaining possible hands
type GameState = [[Card]]

-- | Generates an initial guess, and an initial game state, that is all the other possible hands other than guessed hand
--   The initial guess is chosen as equally distant ranked cards and having different suits.
--   Input : n - size of the hand (Answerer's hand)
--   Output: initguess - initial guess (list of Card, of size n)
--         : initstate - initial state
initialGuess :: Int -> ([Card],GameState)

initialGuess n = 
    let
        initguess = initHelper n 0 >>= \(r,s) -> [(ranks)!!(r-1)] >>= \card_rank -> [Card ((suits)!!(s`mod`4)) card_rank]
        initstate = allhands \\ [initguess]
    in
        (initguess, initstate)
    where 
        ranks = [minBound..maxBound]::[Rank]
        suits = [minBound..maxBound]::[Suit]
        deck = [Card suit rank | suit <- suits, rank <- ranks]
        allhands = uniqueCombos n deck

-- | Generates feedback for a given Answerer's hand and given Guesser's hand. The feedback is as follows:
--      1. Number of exact matches of the card
--      2. Number of lower ranked cards, compared to the lowest card of Guesser's hand, in Answerer's hand
--      3. Number of cards having exact ranks in the Guesser's hand as that of the Answerer's hand
--      4. Number of higher ranked cards, compared to the highest card of Guesser's hand, in Answerer's hand
--      5. Number of cards having exact suits in the Guesser's hand as that of the Answerer's handi
--      Input : ans_hand - Answerer's hand
--            : guess_hand - Guesser's hand
--      Output: set of five integer values corresponding to above mentioned five points.
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)

feedback ans_hand guess_hand = 
    let
        ans_hand_ranks = extractRank ans_hand
        guess_hand_ranks = extractRank guess_hand
        ans_hand_suits = extractSuit ans_hand
        guess_hand_suits = extractSuit guess_hand
        lower_ranks = if lowest_rank == (minBound::Rank) then [] else [(minBound::Rank)..(prev lowest_rank)]::[Rank]
        higher_ranks = if highest_rank == (maxBound::Rank) then [] else [(next highest_rank)..(maxBound::Rank)]::[Rank]
    in
        (length $ intersect ans_hand guess_hand
        , length $ intersect ans_hand_ranks lower_ranks
        , cardMatchCount ans_hand_ranks guess_hand_ranks 0
        , length $ intersect ans_hand_ranks higher_ranks
        , cardMatchCount ans_hand_suits guess_hand_suits 0)
    where
        lowest_rank = findLowestRank guess_hand (maxBound::Rank)
        highest_rank = findHighestRank guess_hand (minBound::Rank)

-- | Generates the best next guess based on the feedback.
--   The best results can be had by carefully choosing a guess that is likely to leave you the smallest remaining 
--   list of possible answers. To begin with only those hands are selected which will give same feedback with the last guessed hand,
--   had they been the Answerer's hand.
--   Input : old_hand - previously guessed hand
--         : old_state - previous game state
--         : last_feedback - feedback to the old_hand
--   Output: new_hand - best guessed hand from remaining set of hands in game state
--         : new_state - new state after guessing the new_hand
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)

nextGuess (old_hand, old_state) last_feedback =
    let
        new_hand = bestGuess state'
        new_state = state' \\ [new_hand]
    in
        (new_hand, new_state)
    where
        state' = [ hand | hand <- old_state, isGoodHand hand old_hand last_feedback ]
                                

-- helper functions --

-- | next function returns the next enum value of a given enum value in the enumerated list
next :: (Eq a, Enum a, Bounded a) => a -> a

next e | e == maxBound = minBound
       | otherwise = succ e

-- | prev function returns the previous enum value of a given enum value in the enumerated list
prev :: (Eq a, Enum a, Bounded a) => a -> a

prev e | e == minBound = maxBound
       | otherwise = pred e

-- | findLowestRank function returns the lowest rank of a hand
findLowestRank :: [Card] -> Rank -> Rank

findLowestRank [] lowest_rank = lowest_rank
findLowestRank ((Card _ rank):hand_tail) lowest_rank
    | rank < lowest_rank = findLowestRank hand_tail rank
    | otherwise = findLowestRank hand_tail lowest_rank

-- | findHighestRank function returns the highest rank of a hand
findHighestRank :: [Card] -> Rank -> Rank

findHighestRank [] highest_rank = highest_rank
findHighestRank ((Card _ rank):hand_tail) highest_rank
    | rank > highest_rank = findHighestRank hand_tail rank
    | otherwise = findHighestRank hand_tail highest_rank

-- | extractRank extracts the ranks of a given hand
extractRank :: [Card] -> [Rank]

extractRank [] = []
extractRank ((Card _ rank):hand_tail) = [rank] ++ extractRank hand_tail

-- | extractSuit extracts the suits of a given hand
extractSuit :: [Card] -> [Suit]

extractSuit [] = []
extractSuit ((Card suit _):hand_tail) = [suit] ++ extractSuit hand_tail

-- | cardMatchCount returns the number of exact matches in the two given lists of hands or hand ranks or hand suits.
cardMatchCount :: (Eq a) => [a] -> [a] -> Int -> Int

cardMatchCount [] _ count = count
cardMatchCount (x:xs) ys count = if x `elem` ys
                 then cardMatchCount xs (delete x ys) (count+1)
                 else cardMatchCount xs ys count

-- next guess helper function --

-- | bestGuess is a helper function, which helps in taking the next best guess
--   For each remaining possible answer, the average number of possible answers (Answerer's hand) is computed that will be 
--   left if the agent guesses it.
bestGuess :: GameState -> [Card]

bestGuess possible_hands =
    let
        best_guess = sortBy (comparing snd) possible_ans
    in
        fst $ head best_guess
    where
        possible_ans = [(ans_hand, avg_rem_hands)
                       | ans_hand <- possible_hands
                       , let state' = possible_hands \\ [ans_hand]
                       , let avg_rem_hands = avgRemHands ans_hand state']

-- | avgRemHands calculates average number of possible remaining hands (potential candidate hands for answer) for a given guess-hand by 
--   creating groups of hands, grouped by feedback given the guess-hand is not the answer. The expected number of remaining possible 
--   answers for that guess is the average of the sizes of these groups, weighted by the sizes of the groups. 
--   That is, it is the sum of the squares of the group sizes divided by the sum of the group sizes.
avgRemHands :: [Card] -> GameState -> Double

avgRemHands possible_ans state =
    let
        sum_sq_grp_size = fromIntegral $ sum [ (size^2) | size <- grp_size ]
        sum_grp_size = fromIntegral $ sum grp_size
    in
        sum_sq_grp_size / sum_grp_size
    where
        guess_feedbacks = [ans | next_guess <- state, let ans = feedback next_guess possible_ans]
        grouped_feedbacks = group $ sort guess_feedbacks
        grp_size = groupSizeList grouped_feedbacks


-- | groupSizeList just creates a list of sizes of grouped elements
groupSizeList :: Foldable t => [t a] -> [Int]

groupSizeList [] = []
groupSizeList (group_head:group_tail) = [length group_head] ++ groupSizeList group_tail

-- | isGoodHand returns True if a card has the same counts for each of the parameters of feedback as that of the old guessed hand.
--   otherwise False
isGoodHand :: [Card] -> [Card] -> (Int,Int,Int,Int,Int) -> Bool

isGoodHand hand oldhand (card_match_count, low_rank_count, rank_match_count, high_rank_count, suit_match_count) =
    let
        hand_ranks = extractRank hand
        oldhand_ranks = extractRank oldhand
        hand_suits = extractSuit hand
        oldhand_suits = extractSuit oldhand
        lower_ranks = if lowest_rank == (minBound::Rank) then [] else [(minBound::Rank)..(prev lowest_rank)]::[Rank]
        higher_ranks = if highest_rank == (maxBound::Rank) then [] else [(next highest_rank)..(maxBound::Rank)]::[Rank]
    in
        card_match_count == (length $ intersect hand oldhand) &&
        rank_match_count == cardMatchCount hand_ranks oldhand_ranks 0 &&
        suit_match_count == cardMatchCount hand_suits oldhand_suits 0 &&
        low_rank_count == (length $ intersect hand_ranks lower_ranks) &&
        high_rank_count == (length $ intersect hand_ranks higher_ranks)
    where
        lowest_rank = findLowestRank oldhand (maxBound::Rank)
        highest_rank = findHighestRank oldhand (minBound::Rank)

-- initial guess helper --

-- | initHelper function helps in deciding the initial guess for the agent. for an n card answer,
--   the agent chooses ranks that are about 13/(n+1) ranks apart.
initHelper :: Int -> Int -> [(Int, Int)]

initHelper n i
    | i < n = [(((13 `div` (n+1)) + (i * (13 `div` n))),i)] ++ initHelper n (i+1)
    | n < 0 = error "Please select a positive number of cards"
    | otherwise = []

-- | uniqueCombos function forms a combination of all possible cards to create all possible hands of a given size n.
uniqueCombos :: Int -> [Card] -> GameState

uniqueCombos 0 _ = [[]]
uniqueCombos _ [] = []
uniqueCombos n (top_card : rest_deck)
    | n > 0 = map (top_card :) (uniqueCombos (n - 1) rest_deck) ++ uniqueCombos n rest_deck
