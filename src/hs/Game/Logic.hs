{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Logic
    ( newGame
    , foldGameStatus
    , Game(..)
    , GameStatus
    ) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad (msum)

import qualified Data.Map as Map

import Game.Types
import Game.Move (Move, movePos)

data Game turn = Game Board (GameStatus turn)

data GameStatus (turn :: Piece) where
    Turn  :: ProcessMove turn -> GameStatus turn
    Draw  :: GameStatus a
    Win   :: Piece -> GameStatus a

type ProcessMove turn = Move turn -> Maybe (Game (Other turn))

foldGameStatus
    :: (ProcessMove turn -> r)
    -> r
    -> (Piece -> r)
    -> GameStatus turn
    -> r
foldGameStatus handleTurn handleDraw handleWin s = case s of
    Turn f -> handleTurn f
    Draw   -> handleDraw
    Win p  -> handleWin p


maybeIf :: Bool -> a -> Maybe a
maybeIf p a
    | p         = Just a
    | otherwise = Nothing

newGame :: Game X
newGame = Game newBoard $ Turn $ makeMove newBoard where

makeMove :: CyclicPiece turn => Board -> Move turn -> Maybe (Game (Other turn))
makeMove (Board mp) move
    | pos `Map.member` mp = Nothing
    | c < 1 || c > 3 || r < 1 || r > 3 = Nothing
    | otherwise = Game board' <$> (gameOver <|> nextTurn)
    where
        assoc = (pos, piece)
        piece = reifyPiece move
        pos   = movePos move
        (Coord c, Coord r) = pos

        board'   = Board mp'
        mp'      = uncurry Map.insert assoc mp

        nextTurn = Just $ Turn $ makeMove board'
        gameOver = victory <|> draw
        draw     = maybeIf (Map.size mp' == 9) Draw
        victory  = fullRow <|> fullColumn <|> fullDiagonal

        fullRow      = msum [match (1, row) (1, 0) | row <- [1..3]]
        fullColumn   = msum [match (col, 1) (0, 1) | col <- [1..3]]
        fullDiagonal = match (1, 1) (1, 1) <|> match (3, 1) (-1, 1)

        match (sx, sy) (dx, dy) = go (3 :: Int) sx sy where
            go _ x y | lookup' (x, y) /= Just piece = Nothing
            go 1 _ _ = Just $ Win piece
            go n x y = go (n-1) (x+dx) (y+dy)

        lookup' (x,y) = Map.lookup (Coord x, Coord y) mp'

newBoard :: Board
newBoard = Board $ Map.empty
