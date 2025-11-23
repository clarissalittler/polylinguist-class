{-
Lesson 7: Object-Oriented Programming in Haskell
(Or: How Functional Programming Achieves OOP Goals)

This file demonstrates how Haskell achieves OOP-like features without traditional OOP:
- Data encapsulation via modules
- Polymorphism via type classes
- "Inheritance" via type class extension
- Abstraction via type classes and ADTs
- Composition (natural in FP)

Haskell's Approach:
- Algebraic Data Types (ADTs) instead of classes
- Type classes instead of interfaces/inheritance
- Pure functions instead of methods
- Immutability by default
- Module system for encapsulation
- Parametric polymorphism (generics) built-in

Key Insight: Haskell achieves the GOALS of OOP (abstraction, polymorphism,
code reuse) using different MECHANISMS (type classes, ADTs, composition).
-}

{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.List (intercalate)
import Data.IORef
import Control.Monad (forM_)
import qualified Data.Map as Map

-- ============================================================================
-- PART 1: DATA TYPES (Instead of Classes)
-- ============================================================================

-- Simple data type (like a class with fields)
data Person = Person {
    personName :: String,
    personAge :: Int
} deriving (Show, Eq)  -- Auto-derive type class instances

-- Smart constructor for validation
createPerson :: String -> Int -> Maybe Person
createPerson name age
    | null name = Nothing
    | age < 0 || age > 150 = Nothing
    | otherwise = Just (Person name age)

-- "Methods" are just functions on the data type
introduce :: Person -> String
introduce (Person name age) =
    "Hi, I'm " ++ name ++ ", " ++ show age ++ " years old"

haveBirthday :: Person -> Person
haveBirthday person = person { personAge = personAge person + 1 }

isAdult :: Person -> Bool
isAdult (Person _ age) = age >= 18

-- Pattern matching (like method overloading)
greet :: Person -> String
greet (Person name age)
    | age < 18 = "Hello, young " ++ name
    | age < 65 = "Hello, " ++ name
    | otherwise = "Hello, wise " ++ name

-- Custom Ord instance (like operator overloading)
instance Ord Person where
    compare (Person _ age1) (Person _ age2) = compare age1 age2

-- ============================================================================
-- PART 2: ENCAPSULATION VIA MODULES
-- ============================================================================

-- In a real module, you'd export only what's needed:
{-
module BankAccount (
    BankAccount,        -- Export type but not constructor
    createAccount,
    deposit,
    withdraw,
    getBalance,
    getTransactionHistory
) where
-}

data Transaction = Transaction {
    transType :: String,
    transAmount :: Double,
    balanceAfter :: Double
} deriving (Show, Eq)

data BankAccount = BankAccount {
    accountNumber :: String,
    accountBalance :: Double,
    transactions :: [Transaction]
} deriving (Show, Eq)

-- Smart constructor (factory method)
createAccount :: String -> Double -> BankAccount
createAccount accNum initialBalance =
    BankAccount accNum initialBalance []

-- Pure functions return new accounts (immutable)
deposit :: Double -> BankAccount -> Maybe BankAccount
deposit amount account
    | amount > 0 = Just newAccount
    | otherwise = Nothing
  where
    newBalance = accountBalance account + amount
    newTrans = Transaction "deposit" amount newBalance
    newAccount = account {
        accountBalance = newBalance,
        transactions = transactions account ++ [newTrans]
    }

withdraw :: Double -> BankAccount -> Maybe BankAccount
withdraw amount account
    | amount > 0 && amount <= accountBalance account = Just newAccount
    | otherwise = Nothing
  where
    newBalance = accountBalance account - amount
    newTrans = Transaction "withdrawal" amount newBalance
    newAccount = account {
        accountBalance = newBalance,
        transactions = transactions account ++ [newTrans]
    }

getBalance :: BankAccount -> Double
getBalance = accountBalance

getTransactionHistory :: BankAccount -> [Transaction]
getTransactionHistory = transactions

-- ============================================================================
-- PART 3: TYPE CLASSES (Instead of Inheritance/Interfaces)
-- ============================================================================

-- Type class defines behavior (like an interface)
class Describable a where
    describe :: a -> String

-- Type class with default implementation
class Describable a => Detailed a where
    detailedDescription :: a -> String
    detailedDescription x = "Detailed: " ++ describe x

-- Animal "hierarchy" using algebraic data types
data Animal
    = Dog { dogName :: String, dogAge :: Int, breed :: String }
    | Cat { catName :: String, catAge :: Int }
    | Duck { duckName :: String, duckAge :: Int }
    deriving (Show, Eq)

-- Type class for animal behavior (like interface)
class Speakable a where
    speak :: a -> String
    getName :: a -> String
    getAge :: a -> Int

-- Implement for Animal type
instance Speakable Animal where
    speak (Dog name _ _) = name ++ " says Woof!"
    speak (Cat name _) = name ++ " says Meow!"
    speak (Duck name _) = name ++ " says Quack!"

    getName (Dog name _ _) = name
    getName (Cat name _) = name
    getName (Duck name _) = name

    getAge (Dog _ age _) = age
    getAge (Cat _ age) = age
    getAge (Duck _ age) = age

instance Describable Animal where
    describe animal = getName animal ++ " is a " ++ show (getAge animal) ++
                     " year old " ++ animalType animal
      where
        animalType (Dog _ _ _) = "dog"
        animalType (Cat _ _) = "cat"
        animalType (Duck _ _) = "duck"

-- Specialized behavior for specific animals
dogFetch :: Animal -> Maybe String
dogFetch (Dog name _ _) = Just $ name ++ " is fetching the ball!"
dogFetch _ = Nothing

catClimb :: Animal -> Maybe String
catClimb (Cat name _) = Just $ name ++ " is climbing a tree!"
catClimb _ = Nothing

-- ============================================================================
-- PART 4: POLYMORPHISM VIA TYPE CLASSES
-- ============================================================================

-- Ad-hoc polymorphism: function works for any Speakable type
makeItSpeak :: Speakable a => a -> IO ()
makeItSpeak thing = putStrLn (speak thing)

-- Parametric polymorphism: function works for ANY type
identity :: a -> a
identity x = x

-- Type class for multiple capabilities (like multiple inheritance)
class Flyable a where
    fly :: a -> String
    land :: a -> String

class Swimmable a where
    swim :: a -> String
    dive :: a -> String

-- Duck implements both (like multiple inheritance)
instance Flyable Animal where
    fly (Duck name _) = name ++ " is flying through the air"
    fly animal = getName animal ++ " cannot fly!"

    land (Duck name _) = name ++ " is landing"
    land animal = getName animal ++ " cannot land (not flying)!"

instance Swimmable Animal where
    swim (Duck name _) = name ++ " is swimming in water"
    swim animal = getName animal ++ " is swimming"

    dive (Duck name _) = name ++ " is diving deep"
    dive animal = getName animal ++ " is diving"

-- Function requiring multiple type class constraints
performAllActions :: (Flyable a, Swimmable a, Speakable a) => a -> [String]
performAllActions thing = [
    speak thing,
    fly thing,
    swim thing
    ]

-- ============================================================================
-- PART 5: ABSTRACTION VIA TYPE CLASSES
-- ============================================================================

-- Abstract "interface" for shapes
class Shape a where
    area :: a -> Double
    perimeter :: a -> Double
    shapeColor :: a -> String

    -- Default implementation
    shapeDescribe :: a -> String
    shapeDescribe shape =
        "A " ++ shapeColor shape ++ " shape with area " ++
        show (area shape)

-- Concrete implementations
data Circle = Circle {
    circleColor :: String,
    radius :: Double
} deriving (Show, Eq)

data Rectangle = Rectangle {
    rectColor :: String,
    width :: Double,
    height :: Double
} deriving (Show, Eq)

instance Shape Circle where
    area (Circle _ r) = pi * r * r
    perimeter (Circle _ r) = 2 * pi * r
    shapeColor = circleColor

instance Shape Rectangle where
    area (Rectangle _ w h) = w * h
    perimeter (Rectangle _ w h) = 2 * (w + h)
    shapeColor = rectColor

-- Polymorphic function using Shape constraint
printShapeInfo :: Shape a => a -> IO ()
printShapeInfo shape = putStrLn (shapeDescribe shape)

-- ============================================================================
-- PART 6: "OPERATOR OVERLOADING" VIA TYPE CLASSES
-- ============================================================================

data Vector2D = Vector2D {
    vecX :: Double,
    vecY :: Double
} deriving (Eq)

-- Implement Num type class for arithmetic operators
instance Num Vector2D where
    (Vector2D x1 y1) + (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
    (Vector2D x1 y1) - (Vector2D x2 y2) = Vector2D (x1 - x2) (y1 - y2)
    (Vector2D x1 y1) * (Vector2D x2 y2) = Vector2D (x1 * x2) (y1 * y2)  -- Element-wise
    abs (Vector2D x y) = Vector2D (abs x) (abs y)
    signum (Vector2D x y) = Vector2D (signum x) (signum y)
    fromInteger n = Vector2D (fromInteger n) (fromInteger n)

instance Show Vector2D where
    show (Vector2D x y) = "Vector2D(" ++ show x ++ ", " ++ show y ++ ")"

-- Scalar multiplication (separate function since not in Num)
scaleVector :: Double -> Vector2D -> Vector2D
scaleVector s (Vector2D x y) = Vector2D (s * x) (s * y)

magnitude :: Vector2D -> Double
magnitude (Vector2D x y) = sqrt (x * x + y * y)

-- ============================================================================
-- PART 7: COMPOSITION (Natural in Functional Programming)
-- ============================================================================

data Engine = Engine {
    horsepower :: Int,
    fuelType :: String
} deriving (Show, Eq)

startEngine :: Engine -> String
startEngine (Engine hp fuel) =
    "Engine starting... " ++ show hp ++ " HP " ++ fuel ++ " engine roaring!"

data Vehicle = Vehicle {
    brand :: String,
    engine :: Maybe Engine
} deriving (Show, Eq)

startVehicle :: Vehicle -> String
startVehicle (Vehicle brand (Just eng)) =
    brand ++ ": " ++ startEngine eng
startVehicle (Vehicle brand Nothing) =
    brand ++ ": No engine - use pedal power!"

-- Composition is natural - just nest data structures
data Wheel = Wheel { wheelDiameter :: Int } deriving (Show, Eq)

data CompleteVehicle = CompleteVehicle {
    cvBrand :: String,
    cvEngine :: Maybe Engine,
    cvWheels :: [Wheel]
} deriving (Show, Eq)

-- ============================================================================
-- PART 8: VALIDATION WITH SMART CONSTRUCTORS
-- ============================================================================

data Temperature = Temperature { celsius :: Double } deriving (Eq)

-- Smart constructor with validation
mkTemperature :: Double -> Maybe Temperature
mkTemperature c
    | c < -273.15 = Nothing  -- Below absolute zero
    | otherwise = Just (Temperature c)

toFahrenheit :: Temperature -> Double
toFahrenheit (Temperature c) = c * 9/5 + 32

fromFahrenheit :: Double -> Maybe Temperature
fromFahrenheit f = mkTemperature ((f - 32) * 5/9)

toKelvin :: Temperature -> Double
toKelvin (Temperature c) = c + 273.15

fromKelvin :: Double -> Maybe Temperature
fromKelvin k = mkTemperature (k - 273.15)

instance Show Temperature where
    show (Temperature c) = show c ++ "°C"

-- ============================================================================
-- PART 9: DESIGN PATTERNS IN FUNCTIONAL STYLE
-- ============================================================================

-- Factory Pattern - just a function!
createAnimal :: String -> String -> Int -> Maybe Animal
createAnimal "dog" name age = Just (Dog name age "Unknown")
createAnimal "cat" name age = Just (Cat name age)
createAnimal "duck" name age = Just (Duck name age)
createAnimal _ _ _ = Nothing

-- Observer Pattern using callbacks
type Observer a = a -> IO ()

data Subject a = Subject {
    subjectState :: IORef a,
    observers :: IORef [Observer a]
}

createSubject :: a -> IO (Subject a)
createSubject initialState = do
    stateRef <- newIORef initialState
    obsRef <- newIORef []
    return $ Subject stateRef obsRef

attachObserver :: Subject a -> Observer a -> IO ()
attachObserver subject observer = do
    modifyIORef (observers subject) (observer :)

notifyObservers :: Subject a -> IO ()
notifyObservers subject = do
    state <- readIORef (subjectState subject)
    obs <- readIORef (observers subject)
    mapM_ (\f -> f state) obs

setSubjectState :: Subject a -> a -> IO ()
setSubjectState subject newState = do
    writeIORef (subjectState subject) newState
    notifyObservers subject

-- Singleton Pattern - use top-level definition
{- In a module:
logger :: Logger
logger = Logger []  -- Single instance

Or use IORef for mutable singleton
-}

-- Strategy Pattern - just pass functions!
type SortStrategy a = [a] -> [a]

sortData :: SortStrategy a -> [a] -> [a]
sortData strategy = strategy

-- Different strategies are just different functions
bubbleSort :: Ord a => [a] -> [a]
bubbleSort = undefined  -- Implementation omitted

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where
    smaller = filter (<= x) xs
    larger = filter (> x) xs

-- ============================================================================
-- PART 10: TYPE CLASS HIERARCHIES (Like Inheritance)
-- ============================================================================

-- Base type class
class Identifiable a where
    getId :: a -> String

-- Extended type class (requires Identifiable)
class Identifiable a => Persistable a where
    save :: a -> IO ()
    load :: String -> IO (Maybe a)

-- Another extension
class Identifiable a => Comparable a where
    compareById :: a -> a -> Ordering
    compareById a b = compare (getId a) (getId b)

-- Example type implementing the hierarchy
data User = User {
    userId :: String,
    userName :: String
} deriving (Show, Eq)

instance Identifiable User where
    getId = userId

instance Persistable User where
    save user = putStrLn $ "Saving user: " ++ getId user
    load uid = do
        putStrLn $ "Loading user: " ++ uid
        return Nothing  -- Simplified

instance Comparable User where
    -- Uses default implementation from type class

-- ============================================================================
-- DEMONSTRATION AND TESTING
-- ============================================================================

main :: IO () = do
    putStrLn $ replicate 70 '='
    putStrLn "LESSON 7: OBJECT-ORIENTED PROGRAMMING IN HASKELL"
    putStrLn "(Achieving OOP Goals with Functional Programming)"
    putStrLn $ replicate 70 '='

    -- Part 1: Data Types
    putStrLn "\n--- PART 1: DATA TYPES (Instead of Classes) ---"
    let alice = Person "Alice" 30
    let bob = Person "Bob" 25
    putStrLn $ introduce alice
    putStrLn $ "Is adult? " ++ show (isAdult alice)
    putStrLn $ "String representation: " ++ show alice
    putStrLn $ "Alice < Bob? " ++ show (alice < bob)
    putStrLn $ greet alice

    -- Part 2: Encapsulation
    putStrLn "\n--- PART 2: ENCAPSULATION VIA MODULES ---"
    let account = createAccount "ACC001" 1000
    let account1 = case deposit 500 account of
                       Just acc -> acc
                       Nothing -> account
    let account2 = case withdraw 200 account1 of
                       Just acc -> acc
                       Nothing -> account1
    putStrLn $ "Balance: $" ++ show (getBalance account2)
    putStrLn $ "Transactions: " ++ show (length $ getTransactionHistory account2)

    -- Part 3: Type Classes
    putStrLn "\n--- PART 3: TYPE CLASSES (Instead of Inheritance) ---"
    let dog = Dog "Buddy" 3 "Golden Retriever"
    let cat = Cat "Whiskers" 2
    putStrLn $ describe dog
    putStrLn $ speak dog
    case dogFetch dog of
        Just msg -> putStrLn msg
        Nothing -> return ()
    putStrLn $ speak cat
    case catClimb cat of
        Just msg -> putStrLn msg
        Nothing -> return ()

    -- Part 4: Polymorphism
    putStrLn "\n--- PART 4: POLYMORPHISM VIA TYPE CLASSES ---"
    let duck = Duck "Donald" 1
    makeItSpeak dog
    makeItSpeak cat
    makeItSpeak duck

    putStrLn "\nMultiple capabilities (like multiple inheritance):"
    putStrLn $ fly duck
    putStrLn $ swim duck

    -- Part 5: Abstraction
    putStrLn "\n--- PART 5: ABSTRACTION VIA TYPE CLASSES ---"
    let circle = Circle "red" 5
    let rect = Rectangle "blue" 4 6
    printShapeInfo circle
    printShapeInfo rect

    -- Part 6: Operator Overloading
    putStrLn "\n--- PART 6: OPERATOR OVERLOADING VIA TYPE CLASSES ---"
    let v1 = Vector2D 3 4
    let v2 = Vector2D 1 2
    let v3 = v1 + v2
    let v4 = scaleVector 2 v1
    putStrLn $ "v1 = " ++ show v1
    putStrLn $ "v2 = " ++ show v2
    putStrLn $ "v1 + v2 = " ++ show v3
    putStrLn $ "2 * v1 = " ++ show v4
    putStrLn $ "|v1| = " ++ show (magnitude v1)

    -- Part 7: Composition
    putStrLn "\n--- PART 7: COMPOSITION (Natural in FP) ---"
    let engine = Engine 200 "gasoline"
    let car = Vehicle "Toyota" (Just engine)
    let bicycle = Vehicle "Schwinn" Nothing
    putStrLn $ startVehicle car
    putStrLn $ startVehicle bicycle

    -- Part 8: Validation
    putStrLn "\n--- PART 8: VALIDATION WITH SMART CONSTRUCTORS ---"
    case mkTemperature 0 of
        Just temp -> do
            putStrLn $ "0°C = " ++ show (toFahrenheit temp) ++ "°F = " ++
                      show (toKelvin temp) ++ "K"
            case fromFahrenheit 212 of
                Just temp2 -> putStrLn $ "212°F = " ++ show temp2
                Nothing -> putStrLn "Invalid temperature"
        Nothing -> putStrLn "Invalid temperature"

    -- Part 9: Design Patterns
    putStrLn "\n--- PART 9: DESIGN PATTERNS IN FUNCTIONAL STYLE ---"

    -- Factory
    case createAnimal "dog" "Max" 2 of
        Just animal -> putStrLn $ "Factory created: " ++ speak animal
        Nothing -> putStrLn "Failed to create animal"

    -- Observer
    putStrLn "\nObserver Pattern:"
    subject <- createSubject (0 :: Int)
    let observer1 state = putStrLn $ "Observer 1: State is now " ++ show state
    let observer2 state = putStrLn $ "Observer 2: State is now " ++ show state
    attachObserver subject observer1
    attachObserver subject observer2
    setSubjectState subject 42

    -- Strategy
    putStrLn "\nStrategy Pattern:"
    let numbers = [3, 1, 4, 1, 5, 9, 2, 6]
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "QuickSort: " ++ show (sortData quickSort numbers)

    -- Part 10: Type Class Hierarchies
    putStrLn "\n--- PART 10: TYPE CLASS HIERARCHIES ---"
    let user = User "user123" "Alice"
    putStrLn $ "User ID: " ++ getId user
    save user

    putStrLn $ "\n" ++ replicate 70 '='
    putStrLn "Haskell OOP demonstration complete!"
    putStrLn "Key insight: Haskell achieves OOP goals using different mechanisms:"
    putStrLn "  - Type classes instead of inheritance"
    putStrLn "  - ADTs instead of classes"
    putStrLn "  - Pure functions instead of methods"
    putStrLn "  - Composition over inheritance (naturally!)"
    putStrLn $ replicate 70 '='
