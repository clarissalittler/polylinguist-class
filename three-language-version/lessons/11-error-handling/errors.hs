-- Lesson 11: Error Handling - Haskell Examples
-- Haskell uses Maybe, Either, and Exceptions for error handling

module Main where

import Control.Exception
import Data.Maybe (fromMaybe, catMaybes)
import System.IO
import Control.Monad (when)

-- =============================================================================
-- MAYBE TYPE: The Simplest Error Handling
-- =============================================================================

-- Safe division returns Nothing for division by zero
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

-- Safe head returns Nothing for empty list
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- Safe list indexing
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
    | n < 0     = Nothing
    | otherwise = safeIndex xs (n - 1)

-- Using Maybe values
processResult :: Maybe Int -> Int
processResult Nothing  = 0           -- Default value
processResult (Just n) = n * 2

-- =============================================================================
-- CHAINING MAYBE WITH do-NOTATION
-- =============================================================================

-- Compute: sqrt(x/y) safely
safeSqrt :: Double -> Maybe Double
safeSqrt x
    | x < 0     = Nothing
    | otherwise = Just (sqrt x)

safeCompute :: Double -> Double -> Maybe Double
safeCompute x y = do
    result <- safeDiv x y      -- Might fail
    safeSqrt result            -- Might also fail

-- Equivalent using >>=
safeCompute' :: Double -> Double -> Maybe Double
safeCompute' x y = safeDiv x y >>= safeSqrt

-- =============================================================================
-- EITHER TYPE: Errors with Information
-- =============================================================================

data MathError = DivByZero
               | NegativeSqrt
               | Overflow
               deriving (Show, Eq)

safeDivE :: Double -> Double -> Either MathError Double
safeDivE _ 0 = Left DivByZero
safeDivE x y = Right (x / y)

safeSqrtE :: Double -> Either MathError Double
safeSqrtE x
    | x < 0     = Left NegativeSqrt
    | otherwise = Right (sqrt x)

-- Chain Either computations
safeComputeE :: Double -> Double -> Either MathError Double
safeComputeE x y = do
    result <- safeDivE x y
    safeSqrtE result

-- Handle Either results
handleResult :: Either MathError Double -> String
handleResult (Left DivByZero)    = "Error: Division by zero"
handleResult (Left NegativeSqrt) = "Error: Cannot take sqrt of negative"
handleResult (Left Overflow)     = "Error: Numeric overflow"
handleResult (Right value)       = "Success: " ++ show value

-- =============================================================================
-- CUSTOM ERROR TYPES
-- =============================================================================

data ValidationError = EmptyInput
                     | TooShort Int Int  -- actual, minimum
                     | TooLong Int Int   -- actual, maximum
                     | InvalidChar Char
                     deriving (Show, Eq)

validateUsername :: String -> Either ValidationError String
validateUsername "" = Left EmptyInput
validateUsername s
    | length s < 3  = Left (TooShort (length s) 3)
    | length s > 20 = Left (TooLong (length s) 20)
    | not (all isValidChar s) = Left (InvalidChar (head $ filter (not . isValidChar) s))
    | otherwise = Right s
  where
    isValidChar c = c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

-- =============================================================================
-- COMBINING MULTIPLE VALIDATIONS
-- =============================================================================

data UserInput = UserInput
    { username :: String
    , email :: String
    , age :: Int
    } deriving Show

validateEmail :: String -> Either String String
validateEmail e
    | '@' `notElem` e = Left "Email must contain @"
    | otherwise = Right e

validateAge :: Int -> Either String Int
validateAge a
    | a < 0     = Left "Age cannot be negative"
    | a > 150   = Left "Age seems unrealistic"
    | otherwise = Right a

validateAll :: String -> String -> Int -> Either String UserInput
validateAll u e a = do
    validU <- case validateUsername u of
                Left err -> Left (show err)
                Right v -> Right v
    validE <- validateEmail e
    validA <- validateAge a
    return $ UserInput validU validE validA

-- =============================================================================
-- THE EXCEPTION SYSTEM (IO)
-- =============================================================================

-- Note: Exceptions in Haskell are for IO and truly exceptional situations
-- Pure code should use Maybe/Either

readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left e  -> return $ Left (show e)
        Right c -> return $ Right c

-- Using bracket for cleanup
withFile' :: FilePath -> (Handle -> IO a) -> IO a
withFile' path action = bracket
    (openFile path ReadMode)  -- acquire resource
    hClose                     -- release resource (always runs)
    action                     -- use resource

-- =============================================================================
-- PARTIAL FUNCTIONS AND TOTAL FUNCTIONS
-- =============================================================================

-- Partial function (can crash!)
unsafeHead :: [a] -> a
unsafeHead (x:_) = x
unsafeHead [] = error "empty list"  -- BAD!

-- Total function (always returns a valid result)
totalHead :: a -> [a] -> a
totalHead def []    = def
totalHead _   (x:_) = x

-- Using NonEmpty for guaranteed non-empty lists
data NonEmpty a = a :| [a] deriving Show

neHead :: NonEmpty a -> a
neHead (x :| _) = x  -- Always safe!

-- =============================================================================
-- MONADIC ERROR HANDLING
-- =============================================================================

-- MonadFail for pattern match failures in do notation
lookupUser :: Int -> Maybe String
lookupUser 1 = Just "Alice"
lookupUser 2 = Just "Bob"
lookupUser _ = Nothing

getUserAge :: String -> Maybe Int
getUserAge "Alice" = Just 30
getUserAge "Bob"   = Just 25
getUserAge _       = Nothing

-- Chain operations that might fail
getAgeById :: Int -> Maybe Int
getAgeById uid = do
    name <- lookupUser uid
    age <- getUserAge name
    return age

-- =============================================================================
-- APPLICATIVE ERROR COLLECTION
-- =============================================================================

-- Collect all errors instead of stopping at first
data Validation e a = Failure [e] | Success a
    deriving (Show, Eq)

instance Functor (Validation e) where
    fmap _ (Failure es) = Failure es
    fmap f (Success a)  = Success (f a)

instance Applicative (Validation e) where
    pure = Success
    Failure e1 <*> Failure e2 = Failure (e1 ++ e2)
    Failure e  <*> _          = Failure e
    _          <*> Failure e  = Failure e
    Success f  <*> Success a  = Success (f a)

validateName :: String -> Validation String String
validateName "" = Failure ["Name cannot be empty"]
validateName n  = Success n

validateAge' :: Int -> Validation String Int
validateAge' a
    | a < 0  = Failure ["Age cannot be negative"]
    | a > 150 = Failure ["Age too high"]
    | otherwise = Success a

-- This collects ALL errors!
validatePerson :: String -> Int -> Validation String (String, Int)
validatePerson name age = (,) <$> validateName name <*> validateAge' age

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    putStrLn "=== Maybe ==="
    putStrLn $ "safeDiv 10 2 = " ++ show (safeDiv 10 2)
    putStrLn $ "safeDiv 10 0 = " ++ show (safeDiv 10 0)
    putStrLn $ "safeHead [1,2,3] = " ++ show (safeHead [1,2,3 :: Int])
    putStrLn $ "safeHead [] = " ++ show (safeHead ([] :: [Int]))

    putStrLn "\n=== Chaining Maybe ==="
    putStrLn $ "safeCompute 8 2 = " ++ show (safeCompute 8 2)
    putStrLn $ "safeCompute 8 0 = " ++ show (safeCompute 8 0)
    putStrLn $ "safeCompute (-8) 2 = " ++ show (safeCompute (-8) 2)

    putStrLn "\n=== Either ==="
    putStrLn $ handleResult (safeComputeE 8 2)
    putStrLn $ handleResult (safeComputeE 8 0)
    putStrLn $ handleResult (safeComputeE (-8) 2)

    putStrLn "\n=== Validation ==="
    putStrLn $ "validateUsername \"alice123\" = " ++ show (validateUsername "alice123")
    putStrLn $ "validateUsername \"ab\" = " ++ show (validateUsername "ab")
    putStrLn $ "validateUsername \"hello@world\" = " ++ show (validateUsername "hello@world")

    putStrLn "\n=== Combined Validation ==="
    putStrLn $ show $ validateAll "alice" "alice@example.com" 25
    putStrLn $ show $ validateAll "" "invalid" (-5)

    putStrLn "\n=== Monadic Chaining ==="
    putStrLn $ "getAgeById 1 = " ++ show (getAgeById 1)
    putStrLn $ "getAgeById 99 = " ++ show (getAgeById 99)

    putStrLn "\n=== Applicative Validation (collects errors) ==="
    putStrLn $ "validatePerson \"Alice\" 25 = " ++ show (validatePerson "Alice" 25)
    putStrLn $ "validatePerson \"\" (-5) = " ++ show (validatePerson "" (-5))

    putStrLn "\n=== File IO ==="
    result <- readFileSafe "nonexistent.txt"
    putStrLn $ "readFileSafe \"nonexistent.txt\" = " ++ show result
