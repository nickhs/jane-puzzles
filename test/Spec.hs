import Test.Hspec (shouldBe, it, describe, hspec, xdescribe, xit)

import qualified BobAlice as BA
import qualified WellWell as Well
import qualified GetOutVote as GO

import Data.Array ((!))
import Data.List (foldl', delete)
import Data.Foldable (foldlM)

main :: IO ()
main = hspec $ do
    describe "BobAlice" $ do
        xit "works" $ do
            let r = BA.findNBobWins 3
            map BA.gameTally r `shouldBe` [32, 22, 11]

    describe "GetOutVote" $ do
        it "distributes votes" $ do
            let votes = [("Harry", 15), ("Larry", 30), ("Mary", 57)]
            let dels = GO.distributeDelegates 7 votes
            dels `shouldBe` [("Harry", 1), ("Larry", 2), ("Mary", 4)]

        it "works" $ do
            let expected = [("Harry", 21), ("Larry", 34), ("Mary", 48)]
            fst (head GO.solve) `shouldBe` expected

    describe "WellWell" $ do
        it "prints" $ do
            let s = Well.showGrid Well.initialGrid
            putStrLn s

        it "works" $ do
            done <- foldlM (\prev iter -> do
                let r = Well.drip prev
                putStrLn $ "At iter " ++ show iter ++ "\n" ++ (Well.diffCharWise (Well.showGrid prev) (Well.showGrid r))
                return r) Well.initialGrid [0..359]

            True `shouldBe` True
