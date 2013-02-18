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
makeMove b move
    | Nothing <- b ! pos = Game b' <$> (gameOver <|> nextTurn)
    | otherwise          = Nothing
    where
        piece = reifyPiece move
        pos   = movePos move
        b'    = putPiece piece pos b

        nextTurn = Just $ Turn $ makeMove b'
        gameOver = victory <|> draw
        draw     = maybeIf (isFull b') Draw
        victory  = msum [check l | l <- lanes]

        check lane = maybeIf allMatch $ Win piece where
            allMatch = all (\p -> b' ! p == Just piece) lane
