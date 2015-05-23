module Board
    ( Player(..)
    , PieceType (..)
    , Column (..)
    , Row (..)
    , Square (..)
    )
where

-- | Datatype for players
data Player = White | Black

-- | The type for a piece (independent from the color)
data PieceType = King

-- | Columns of the board
data Column = A | B | C | D | E | F | G | H

-- | Rows of the board
data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8

-- | Datatype for a square of the board
data Square = Square { column :: Column, row :: Row}
