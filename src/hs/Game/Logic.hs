{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module Game.Logic
    ( newGame
    , foldGameStatus
    , Game(..)
    , GameStatus
    ) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad (msum)

import Game.Move  (Move, movePos)
import Game.Piece
import Game.Board

data Game (turn :: Piece) = Game Board (GameStatus turn)

type ProcessMove turn = Move turn -> Maybe (Game (Other turn))

data GameStatus (turn :: Piece) where
    Turn  :: ProcessMove turn -> GameStatus turn
    Draw  :: GameStatus a
    Win   :: Piece -> GameStatus a


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
newGame = Game newBoard $ Turn $ makeMove newBoard

type CyclicTurn turn =
    ( ReifyPiece turn
    , ReifyPiece (Other turn)
    , Other (Other turn) ~ turn
    )


makeMove :: CyclicTurn turn => Board -> ProcessMove turn
makeMove board move
    | Nothing <- board ! pos = Game board' <$> (gameOver <|> nextTurn)
    | otherwise              = Nothing
    where
        piece  = reifyPiece move
        pos    = movePos move
        board' = putPiece piece pos board

        nextTurn = Just $ Turn $ makeMove board'
        gameOver = victory <|> draw
        draw     = maybeIf (isFull board') Draw
        victory  = msum $ map check lanes

        check lane = maybeIf allMatch $ Win piece where
            allMatch = all (\p -> board' ! p == Just piece) lane

