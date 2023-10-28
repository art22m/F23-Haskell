{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Text.Read (readMaybe)

-- Murashko Artem BS20-02

main :: IO ()
main = putStrLn "Hello, World!"


-- | 1 Interactive test | --

-- 1.1 Simple tests

-- | A question of some type.
data Question = 
    -- «true/false» question requires user to answer if a given statement is true or false
    TrueFalseQuestion String | 
    -- «single choice» question presents a few options and requires user to pick exactly one;
    SingleChoice String [String] | 
    -- «ordering» question presents a few items and requires user to order them
    Ordering String [String] | 
    -- «multiple choice» question presents a few options and requires user to pick some;
    MultipleChoice String [String] |
    -- «list» question requires user to provide a list of all solutions (e.g. separated by a comma, order does not matter);
    ListSolutions String [String]

-- | An answer to some question.
data Answer = 
    Single String |
    Multiple [String] |
    Invalid

-- | A test with some questions.
data Test = Test [(Question, Answer)]

-- | User answers to test questions (possibly in progress).
data UserTest = UserTest [(Question, Answer)]

-- | Present a question to the user and collect their answer. 
runQuestion :: Question -> IO Answer
runQuestion (TrueFalseQuestion q) = do
    putStrLn q
    Single <$> getLine
runQuestion (SingleChoice q choices) = do
    putStrLn q
    mapM_ putStrLn (zipWith (\i c -> show i ++ ") " ++ c) [1..] choices)
    Single <$> getLine
runQuestion (Ordering q choices) = do
    mapM_ putStrLn (zipWith (\i c -> show i ++ ") " ++ c) [1..] choices)
    res <- getLine
    if length (words res) /= length choices then do
        putStrLn "Invalid."
        runQuestion (Ordering q choices)
    else return (Multiple (words res))
runQuestion (MultipleChoice q choices) = do
    mapM_ putStrLn (zipWith (\i c -> show i ++ ") " ++ c) [1..] choices)
    res <- getLine
    if length (words res) == 0 then do
        putStrLn "Invalid."
        runQuestion (MultipleChoice q choices)
    else return (Multiple (words res))
runQuestion (ListSolutions q choices) = do
    mapM_ putStrLn (zipWith (\i c -> show i ++ ") " ++ c) [1..] choices)
    res <- getLine
    if length (words res) == 0 then do
        putStrLn "Invalid."
        runQuestion (ListSolutions q choices)
    else return (Multiple (words res))

-- | Run a test and collect all user answers.
runTest :: Test -> IO UserTest
runTest (Test []) = return (UserTest [])
runTest (Test ((q,_):qs)) = do
  answer <- runQuestion q
  rest <- runTest (Test qs)
  return (UserTest ((q, answer):getList rest))
  where
    getList :: UserTest -> [(Question, Answer)]
    getList (UserTest arr) = arr