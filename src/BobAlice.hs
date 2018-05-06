module BobAlice where

-- https://www.janestreet.com/puzzles/what-about-bob/
-- https://www.janestreet.com/puzzles/solutions/february-2017-solution/

import           Debug.Trace

data GameState = GameState {
    gamePrevious :: [(Persons, Int)],
    gameTally    :: Int,
    gameDesired  :: Int,
    gameTurns    :: Int
} deriving (Show)

data Persons = Bob | Alice deriving (Show)

initialGame :: Int -> GameState
initialGame desired = GameState {
    gamePrevious = [],
    gameTally = 0,
    gameTurns = 0,
    gameDesired = desired
}

whoWon :: GameState -> Persons
whoWon state = case gameTurns state `mod` 2 of
    -- if game ends in an odd number, alice won
    1 -> Alice
    -- if game ends in an even number, bob won
    0 -> Bob

swapPersons :: Persons -> Persons
swapPersons Alice = Bob
swapPersons Bob = Alice

gameLoop :: GameState -> [GameState]
gameLoop state
    -- shouldn't happen, logic error
    | gameTally state > gameDesired state = error "wat"
    -- yay we won!
    | gameTally state == gameDesired state = [state]
    -- we can't make any choices, we lose :(
    | null options' = [state]
    -- we've lost :/
    | null winningStates = [head newStates]
    -- recurse
    | otherwise = winningStates
    where
        options = case gamePrevious state of
            ((_, num):_) -> filter (/= num) [1..9]
            []           -> [1..9]
        options' = filter (\x -> (x + gameTally state) <= gameDesired state) options
        newStates = concatMap (gameLoop . mkState) options'
        mkState choice = GameState {
            gamePrevious = (swapPersons $ whoWon state, choice) : gamePrevious state,
            gameTally = gameTally state + choice,
            gameTurns = gameTurns state + 1,
            gameDesired = gameDesired state
        }

        -- FIXME(nickhs): do I want the shortest winning states?
        -- find all states that have me (even or odd) as the winner
        isItMe = case gameTurns state `mod` 2 of
            -- I'm even, I win if the final state is odd
            0 -> \num -> num `mod` 2 == 1
            -- I'm odd, I win if the final state is even
            1 -> \num -> num `mod` 2 == 0

        winningStates = filter (isItMe . gameTurns) newStates

findNBobWins :: Int -> [GameState]
findNBobWins count = go 1 []
    where
        go :: Int -> [GameState] -> [GameState]
        go num won = do
            let r = head $ gameLoop (initialGame num)
            let newWon = case whoWon r of
                    Bob -> r : won
                    Alice -> won

            if length newWon == count
            then newWon
            else go (num + 1) newWon
