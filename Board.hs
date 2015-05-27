module Board
    ( Player(..)
    , PieceType (..)
    , File (..)
    , Rank (..)
    , Square (..)
    , Piece (..)
    , Board
    , addPiece
    , removePiece
    , movePiece
    , emptyBoard
    , getPiece
    )
where

import Data.Map (Map)
import qualified Data.Map as Map

-- | Datatype for players
data Player = White | Black
    deriving (Show, Eq)

-- | The type for a piece (independent from the color)
data PieceType = King
    deriving (Show, Eq)

-- | Files of the board
data File = A | B | C | D | E | F | G | H
    deriving (Show, Eq, Ord)

-- | Ranks of the board
data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
    deriving (Show, Eq, Ord)

-- | Datatype for a square of the board
data Square = Square { file :: File, rank :: Rank}
    deriving (Show, Eq, Ord)

-- | Datatype for pieces
data Piece = Piece { value :: PieceType, player :: Player }
    deriving (Show, Eq)

-- | The chess board
data Board = Board (Map Square Piece)
    deriving (Show)

-- | Add a piece to the board. Replaces any existing piece on the square.
addPiece :: Square -> Piece -> Board -> Board
addPiece square piece (Board boardMap) = Board $ Map.insert square piece boardMap

-- | Remove a piece from the board, or return the same board if there is no
-- piece on the square.
removePiece :: Square -> Board -> Board
removePiece square (Board boardMap) = Board $ Map.delete square boardMap

-- | Move a piece from a square to another
movePiece
    :: Square -- ^ source square
    -> Square -- ^ destination
    -> Board
    -> Board
movePiece source dest board@(Board boardMap) =
    let
        piece = Map.lookup source boardMap
        doMove p = addPiece dest p . removePiece source $ board
    in
       maybe board doMove piece 

-- | Get a piece from the given square
getPiece :: Square -> Board -> Maybe Piece
getPiece s (Board m) = Map.lookup s m

-- | Empty board
emptyBoard :: Board
emptyBoard = Board Map.empty
