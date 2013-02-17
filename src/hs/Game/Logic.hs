{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Game.Logic (newGame) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad (msum, void, join, when)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Concurrent (forkIO)

import qualified Data.Map as Map

import Game.Types

type CyclicMove turn =
    ( Other (Other turn) ~ turn
    , MoveAssoc turn
    , MoveAssoc (Other turn)
    )

maybeIf :: Bool -> a -> Maybe a
maybeIf p a
    | p         = Just a
    | otherwise = Nothing

newGame :: Game X
newGame = Game newBoard $ Turn $ makeMove newBoard where

makeMove :: CyclicMove turn => Board -> Move turn -> Maybe (Game (Other turn))
makeMove (Board mp) move
    | pos `Map.member` mp = Nothing
    | x < 1 || x > 3 || y < 1 || y > 3 = Nothing
    | otherwise = Game board' <$> (gameOver <|> nextTurn)
    where
        assoc@(pos, piece) = moveAssoc move
        (Coord x, Coord y) = pos

        board'   = Board mp'
        mp'      = uncurry Map.insert assoc mp

        nextTurn = Just $ Turn $ makeMove board'
        gameOver = victory <|> draw
        draw     = maybeIf (Map.size mp' == 9) Draw
        victory  = fullRow <|> fullColumn <|> fullDiagonal

        fullRow      = msum [match (1, r) (1, 0) | r <- [1..3]]
        fullColumn   = msum [match (c, 1) (0, 1) | c <- [1..3]]
        fullDiagonal = match (1, 1) (1, 1) <|> match (3, 1) (-1, 1)

        match (sx, sy) (dx, dy) = go (3 :: Int) sx sy where
            go _ x y | lookup' (x, y) /= Just piece = Nothing
            go 1 _ _ = Just $ Win piece
            go n x y = go (n-1) (x+dx) (y+dy)

        lookup' (x,y) = Map.lookup (Coord x, Coord y) mp'

newBoard :: Board
newBoard = Board $ Map.empty
