module Main where

import Board
import Test.Hspec

main :: IO ()
main = hspec $
    describe "Basic board operations" $ do
        let boardWithPiece = addPiece (Square E R4) (Piece King White) emptyBoard
        it "should add pieces to the board" $
           getPiece (Square E R4) boardWithPiece
            `shouldBe` Just (Piece King White)
        it "should remove pieces from the board" $
            getPiece (Square E R4) (removePiece (Square E R4) boardWithPiece)
             `shouldBe` Nothing
        it "should move pieces on the board" $ do
            let movedBoard = movePiece (Square E R4) (Square F R8) boardWithPiece
            getPiece (Square E R4) movedBoard `shouldBe` Nothing
            getPiece (Square F R8) movedBoard `shouldBe` Just (Piece King White)
            
