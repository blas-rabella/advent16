{-# LANGUAGE OverloadedStrings #-}

module Parser (Operation(Rect,Rot), Ax(X, Y), parseLines) where


import Data.Attoparsec.Text
import Data.Text
import Control.Applicative hiding(many)
data Operation = Rect Int Int | Rot Ax Int Int deriving (Show, Read, Eq)

data Ax = X | Y deriving (Show, Eq, Read)

parseRect :: Parser Operation
parseRect = do
    _ <- "rect "
    x <- decimal
    _ <- "x"
    y <- decimal
    return $ Rect x y

parsX = do
    x <- "column x"
    return X

parsY = do
    y <- "row y"
    return Y

parseAxis :: Parser Ax
parseAxis = do
    parsX <|> parsY

parseRot = do
    _ <- "rotate "
    ax <- parseAxis
    _ <- "="
    n <- decimal
    _ <- " by "
    m <- decimal
    return $ Rot ax n m

parseInstruction = do
    parseRot <|> parseRect

parseLines :: [Text] -> Either String [Operation]
parseLines = mapM (parseOnly parseInstruction)
