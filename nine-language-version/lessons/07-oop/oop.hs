-- Lesson 7: Object-Oriented Programming in Haskell
--
-- Haskell is a pure functional language and doesn't have traditional OOP.
-- However, it has concepts that achieve similar goals:
-- - Algebraic Data Types (ADTs) for data modeling
-- - Type Classes for polymorphism (different from OOP classes!)
-- - Records for bundling data
-- - Pattern matching for behavior
--
-- This demonstrates functional alternatives to OOP patterns.

{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as Map
import Data.IORef

-- ====================
-- 1. Records (like simple classes)
-- ====================

data Person = Person
    { personName :: String
    , personAge :: Int
    } deriving (Show)

-- "Constructor" is just a function
makePerson :: String -> Int -> Person
makePerson name age = Person { personName = name, personAge = age }

-- "Methods" are functions that take the record
introduce :: Person -> String
introduce p = "Hi, I'm " ++ personName p ++ ", " ++ show (personAge p) ++ " years old"

haveBirthday :: Person -> Person
haveBirthday p = p { personAge = personAge p + 1 }

-- ====================
-- 2. Type Classes (polymorphism, not OOP classes!)
-- ====================

-- Type class defines an interface
class Animal a where
    speak :: a -> String
    sleep :: a -> String
    sleep _ = "Zzz..."  -- Default implementation

-- Algebraic Data Types for different animals
data Dog = Dog
    { dogName :: String
    , dogBreed :: String
    } deriving (Show)

data Cat = Cat
    { catName :: String
    , catIndoor :: Bool
    } deriving (Show)

-- Implement the Animal type class for Dog
instance Animal Dog where
    speak d = dogName d ++ " says Woof!"
    sleep d = dogName d ++ " is sleeping... Zzz"

-- Implement the Animal type class for Cat
instance Animal Cat where
    speak c = catName c ++ " says Meow!"
    sleep c = catName c ++ " is sleeping... Zzz"

-- Additional Dog-specific behavior
fetch :: Dog -> String
fetch d = dogName d ++ " is fetching the ball!"

-- Additional Cat-specific behavior
scratch :: Cat -> String
scratch c = catName c ++ " is scratching the furniture!"

-- ====================
-- 3. Sum Types for Polymorphism
-- ====================

-- Sum type can hold any kind of animal
data AnimalType = DogAnimal Dog | CatAnimal Cat

-- Pattern matching for polymorphic behavior
animalSpeak :: AnimalType -> String
animalSpeak (DogAnimal d) = speak d
animalSpeak (CatAnimal c) = speak c

-- ====================
-- 4. Abstract Data Types with Type Classes (like abstract classes/interfaces)
-- ====================

class Shape a where
    area :: a -> Double
    perimeter :: a -> Double

    -- Default implementation using other methods
    describe :: a -> String
    describe s = shapeName s ++ ": area=" ++ show (roundTo 2 (area s))
                 ++ ", perimeter=" ++ show (roundTo 2 (perimeter s))

    shapeName :: a -> String

-- Helper function
roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n)) / 10^n

-- Concrete implementations
data Circle = Circle { circleRadius :: Double } deriving (Show)

instance Shape Circle where
    area c = pi * r * r where r = circleRadius c
    perimeter c = 2 * pi * circleRadius c
    shapeName _ = "Circle"

data Rectangle = Rectangle
    { rectWidth :: Double
    , rectHeight :: Double
    } deriving (Show)

instance Shape Rectangle where
    area r = rectWidth r * rectHeight r
    perimeter r = 2 * (rectWidth r + rectHeight r)
    shapeName _ = "Rectangle"

data Triangle = Triangle
    { triSideA :: Double
    , triSideB :: Double
    , triSideC :: Double
    } deriving (Show)

instance Shape Triangle where
    area t = sqrt (s * (s - a) * (s - b) * (s - c))
        where
            a = triSideA t
            b = triSideB t
            c = triSideC t
            s = perimeter t / 2

    perimeter t = triSideA t + triSideB t + triSideC t
    shapeName _ = "Triangle"

-- Existential type for storing different shapes together
data AnyShape = forall a. Shape a => AnyShape a

instance Shape AnyShape where
    area (AnyShape s) = area s
    perimeter (AnyShape s) = perimeter s
    shapeName (AnyShape s) = shapeName s

-- ====================
-- 5. Encapsulation with Abstract Data Types
-- ====================

-- Only export the type and specific functions, not the constructor
-- This provides encapsulation (in a real module)

data BankAccount = BankAccount
    { accountNumber :: String
    , balance :: Double
    , transactions :: [String]
    } deriving (Show)

newAccount :: String -> Double -> BankAccount
newAccount accNum initialBalance =
    BankAccount accNum initialBalance []

deposit :: Double -> BankAccount -> Maybe BankAccount
deposit amount acc
    | amount > 0 = Just acc
        { balance = balance acc + amount
        , transactions = transactions acc ++ ["Deposit: +$" ++ show amount]
        }
    | otherwise = Nothing

withdraw :: Double -> BankAccount -> Maybe BankAccount
withdraw amount acc
    | amount > 0 && amount <= balance acc = Just acc
        { balance = balance acc - amount
        , transactions = transactions acc ++ ["Withdrawal: -$" ++ show amount]
        }
    | otherwise = Nothing

getBalance :: BankAccount -> Double
getBalance = balance

getTransactions :: BankAccount -> [String]
getTransactions = transactions

-- ====================
-- 6. "Smart Constructors" and Phantom Types
-- ====================

data Temperature = Temperature { celsius :: Double } deriving (Show)

fromCelsius :: Double -> Temperature
fromCelsius c = Temperature c

fromFahrenheit :: Double -> Temperature
fromFahrenheit f = Temperature ((f - 32) * 5 / 9)

fromKelvin :: Double -> Temperature
fromKelvin k = Temperature (k - 273.15)

isFreezing :: Temperature -> Bool
isFreezing t = celsius t <= 0

toFahrenheit :: Temperature -> Double
toFahrenheit t = celsius t * 9 / 5 + 32

toKelvin :: Temperature -> Double
toKelvin t = celsius t + 273.15

-- ====================
-- 7. Composition (preferred in FP!)
-- ====================

data Engine = Engine
    { horsepower :: Int
    , running :: Bool
    } deriving (Show)

newEngine :: Int -> Engine
newEngine hp = Engine hp False

startEngine :: Engine -> Engine
startEngine e = e { running = True }

stopEngine :: Engine -> Engine
stopEngine e = e { running = False }

data Car = Car
    { brand :: String
    , model :: String
    , engine :: Engine  -- Composition!
    } deriving (Show)

newCar :: String -> String -> Int -> Car
newCar b m hp = Car b m (newEngine hp)

startCar :: Car -> (String, Car)
startCar c =
    let newEngine = startEngine (engine c)
        msg = brand c ++ " " ++ model c ++ ": Engine starting... "
              ++ show (horsepower newEngine) ++ "hp engine now running"
    in (msg, c { engine = newEngine })

stopCar :: Car -> (String, Car)
stopCar c =
    let newEngine = stopEngine (engine c)
        msg = brand c ++ " " ++ model c ++ ": Engine stopped"
    in (msg, c { engine = newEngine })

-- ====================
-- 8. Type Classes for Operator-like Behavior
-- ====================

data Point = Point
    { pointX :: Double
    , pointY :: Double
    } deriving (Show, Eq)

-- Point is an instance of Num, so we can use +, -, etc.
instance Num Point where
    Point x1 y1 + Point x2 y2 = Point (x1 + x2) (y1 + y2)
    Point x1 y1 - Point x2 y2 = Point (x1 - x2) (y1 - y2)
    Point x1 y1 * Point x2 y2 = Point (x1 * x2) (y1 * y2)  -- Element-wise
    abs (Point x y) = Point (abs x) (abs y)
    signum (Point x y) = Point (signum x) (signum y)
    fromInteger n = Point (fromInteger n) (fromInteger n)

distanceFromOrigin :: Point -> Double
distanceFromOrigin (Point x y) = sqrt (x * x + y * y)

-- ====================
-- 9. Multiple Type Class Constraints (like multiple interfaces)
-- ====================

class Flyable a where
    fly :: a -> String

class Swimmable a where
    swim :: a -> String

data Duck = Duck { duckName :: String } deriving (Show)

instance Animal Duck where
    speak d = duckName d ++ " says Quack!"

instance Flyable Duck where
    fly d = duckName d ++ " is flying through the air"

instance Swimmable Duck where
    swim d = duckName d ++ " is swimming in water"

-- Function requiring multiple constraints
showDuckAbilities :: (Animal a, Flyable a, Swimmable a) => a -> IO ()
showDuckAbilities d = do
    putStrLn $ "   " ++ speak d
    putStrLn $ "   " ++ fly d
    putStrLn $ "   " ++ swim d

-- ====================
-- 10. Design Patterns in Functional Style
-- ====================

-- Factory Pattern (just a function!)
createAnimal :: String -> String -> AnimalType
createAnimal "dog" name = DogAnimal (Dog name "Mixed")
createAnimal "cat" name = CatAnimal (Cat name True)
createAnimal _ name = DogAnimal (Dog name "Unknown")

-- Builder Pattern (using function composition)
data PersonBuilder = PersonBuilder
    { builderName :: Maybe String
    , builderAge :: Maybe Int
    }

emptyBuilder :: PersonBuilder
emptyBuilder = PersonBuilder Nothing Nothing

setName :: String -> PersonBuilder -> PersonBuilder
setName n pb = pb { builderName = Just n }

setAge :: Int -> PersonBuilder -> PersonBuilder
setAge a pb = pb { builderAge = Just a }

buildPerson :: PersonBuilder -> Maybe Person
buildPerson pb = do
    n <- builderName pb
    a <- builderAge pb
    return $ Person n a

-- ====================
-- Main Function
-- ====================

main :: IO ()
main = do
    putStrLn "=== Object-Oriented Programming in Haskell ===\n"

    -- 1. Basic record
    putStrLn "1. Basic Record (like a simple class):"
    let alice = makePerson "Alice" 30
    putStrLn $ "   " ++ introduce alice
    let alice' = haveBirthday alice
    putStrLn $ "   After birthday: age = " ++ show (personAge alice')

    -- 2. Type classes and polymorphism
    putStrLn "\n2. Type Classes and Polymorphism:"
    let buddy = Dog "Buddy" "Golden Retriever"
    let whiskers = Cat "Whiskers" True
    let maxDog = Dog "Max" "German Shepherd"

    putStrLn $ "   " ++ speak buddy
    putStrLn $ "   " ++ speak whiskers
    putStrLn $ "   " ++ speak maxDog

    putStrLn $ "   " ++ fetch buddy
    putStrLn $ "   " ++ scratch whiskers

    -- 3. Polymorphism with sum types
    putStrLn "\n   Using sum types for polymorphism:"
    let animals = [DogAnimal buddy, CatAnimal whiskers, DogAnimal maxDog]
    mapM_ (\a -> putStrLn $ "   " ++ animalSpeak a) animals

    -- 4. Shapes
    putStrLn "\n3. Shapes (Type Classes):"
    let circle = Circle 5.0
    let rectangle = Rectangle 4.0 6.0
    let triangle = Triangle 3.0 4.0 5.0

    putStrLn $ "   " ++ describe circle
    putStrLn $ "   " ++ describe rectangle
    putStrLn $ "   " ++ describe triangle

    -- 5. Encapsulation
    putStrLn "\n4. Encapsulation (Bank Account):"
    let account = newAccount "ACC001" 1000.0
    putStrLn $ "   Initial balance: $" ++ show (getBalance account)
    case deposit 500.0 account of
        Just acc1 -> do
            putStrLn $ "   After deposit: $" ++ show (getBalance acc1)
            case withdraw 200.0 acc1 of
                Just acc2 -> do
                    putStrLn $ "   After withdrawal: $" ++ show (getBalance acc2)
                    putStrLn $ "   Transactions: " ++ show (getTransactions acc2)
                Nothing -> putStrLn "   Withdrawal failed"
        Nothing -> putStrLn "   Deposit failed"

    -- 6. Smart constructors
    putStrLn "\n5. Smart Constructors (Temperature):"
    let temp1 = fromCelsius 0
    let temp2 = fromFahrenheit 32
    let temp3 = fromKelvin 273.15

    putStrLn $ "   0°C = " ++ show (roundTo 1 (toFahrenheit temp1)) ++ "°F"
    putStrLn $ "   32°F = " ++ show (roundTo 1 (celsius temp2)) ++ "°C"
    putStrLn $ "   273.15K = " ++ show (roundTo 1 (celsius temp3)) ++ "°C"
    putStrLn $ "   Is 0°C freezing? " ++ show (isFreezing temp1)

    -- 7. Composition
    putStrLn "\n6. Composition:"
    let car = newCar "Toyota" "Camry" 200
    let (startMsg, car') = startCar car
    putStrLn $ "   " ++ startMsg
    let (stopMsg, car'') = stopCar car'
    putStrLn $ "   " ++ stopMsg

    -- 8. Operator overloading with type classes
    putStrLn "\n7. Operator Overloading (Type Classes):"
    let p1 = Point 3 4
    let p2 = Point 1 2
    putStrLn $ "   p1 = " ++ show p1
    putStrLn $ "   p2 = " ++ show p2
    putStrLn $ "   p1 + p2 = " ++ show (p1 + p2)
    putStrLn $ "   p1 - p2 = " ++ show (p1 - p2)
    putStrLn $ "   p1 == p2? " ++ show (p1 == p2)
    putStrLn $ "   p1 distance from origin: " ++ show (roundTo 2 (distanceFromOrigin p1))

    -- 9. Multiple type class constraints
    putStrLn "\n8. Multiple Type Class Constraints:"
    let duck = Duck "Donald"
    showDuckAbilities duck

    -- 10. Factory pattern
    putStrLn "\n9. Factory Pattern:"
    let dog = createAnimal "dog" "Rover"
    let cat = createAnimal "cat" "Mittens"
    putStrLn $ "   " ++ animalSpeak dog
    putStrLn $ "   " ++ animalSpeak cat

    -- 11. Builder pattern
    putStrLn "\n10. Builder Pattern:"
    case buildPerson $ setAge 25 $ setName "Bob" emptyBuilder of
        Just bob -> putStrLn $ "   " ++ introduce bob
        Nothing -> putStrLn "   Failed to build person"

    putStrLn "\n11. Key Takeaway:"
    putStrLn "   Haskell doesn't have OOP, but achieves similar goals through:"
    putStrLn "   - Type classes for polymorphism"
    putStrLn "   - ADTs for modeling data"
    putStrLn "   - Immutability and pure functions"
    putStrLn "   - Composition over inheritance"
