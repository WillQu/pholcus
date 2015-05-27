module Main where

import Board
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Validate basic board operations" $ do
        it "should add pieces to the board" $ do
           getPiece (Square E R4) (addPiece (Square E R4) (Piece King White) emptyBoard)
            `shouldBe` Just (Piece King White)
