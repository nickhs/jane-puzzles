module GetOutVote where

-- https://www.janestreet.com/puzzles/get-out-the-vote/
-- https://www.janestreet.com/puzzles/solutions/june-2016-solution/

import Data.List (sortBy, sortOn, findIndex)
import Data.Either (fromRight)

type Candidate = String

type VotingOutcome = [(Candidate, Int)]
type DelegateOutcome = [(Candidate, Int)]

distributeDelegates :: Int -> VotingOutcome -> DelegateOutcome
distributeDelegates delegateCount votes = do
    let totalVotes = sum $ map snd votes
    let delegates voteCount = fromIntegral delegateCount * (fromIntegral voteCount / fromIntegral totalVotes)
    let delegates' voteCount = floor (delegates voteCount)
    let remainder voteCount = delegates voteCount - fromIntegral (delegates' voteCount)
    let r = sortBy (\(_, _, a) (_, _, b) -> b `compare` a)
            $ map (\(cand, voteCount) ->
                (cand, delegates' voteCount, remainder voteCount)) votes
    let remainingDelegates = delegateCount - sum (map (\(_, x, _) -> x) r)
    let delegateMask = replicate remainingDelegates 1 ++ repeat 0
    let r2 = zipWith (\(cand, delCount, _) moreDels ->
                        (cand, delCount + moreDels)) r delegateMask
    sortOn fst r2

votingOutcomes :: Int -> [VotingOutcome]
votingOutcomes votes = map (\(h, l, m) -> [("Harry", h), ("Larry", l), ("Mary", m)]) validVotes
    where
        validVotes = filter (\(a, b, c) -> a + b + c == votes) allVotes
        allVotes = [(a, b, c) | a <- [0..votes], b <- [0..votes], c <- [0..votes]]

findMatchingVotes :: DelegateOutcome -> Int -> [(VotingOutcome, DelegateOutcome)]
findMatchingVotes delegates voteCount = do
    let vOutcomes = votingOutcomes voteCount
    let delegateCount = sum $ map snd delegates
    let delegateOutcomes = zip vOutcomes $ map (distributeDelegates delegateCount) vOutcomes
    filter (\(votes, dgates) -> dgates == delegates) delegateOutcomes

addVotes :: Candidate -> Int -> VotingOutcome -> Either VotingOutcome VotingOutcome
addVotes cand votes state = case findIndex (\(c, _) -> c == cand) state of
                                Nothing -> Left state
                                Just idx -> Right $ do
                                    let count = snd (state !! idx)
                                    let (before, after) = splitAt idx state
                                    before ++ [(cand, count + votes)] ++ tail after

solve :: [(DelegateOutcome, VotingOutcome)]
solve = filter (\(_, [("Harry", n), _, _]) -> n == 1) newDistributions
    where
        firstKnown = [("Harry", 2), ("Larry", 2), ("Mary", 3)]
        votesMatchingFirst = map fst $ findMatchingVotes firstKnown 102
        newPotentialVotes = map (fromRight (error "wat") . addVotes "Harry" 1) votesMatchingFirst
        newDistributions = zip newPotentialVotes $ map (distributeDelegates 8) newPotentialVotes
