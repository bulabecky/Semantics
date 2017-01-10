-- comments
{-
multi line comments
-}

import Data.List
import System.IO

-- Int -2^63 2^63
maxInt = maxBound :: Int
minInt = minBound :: Int

-- Integer (can be as big as your memory can hold)
--Float
-- Double 
bigFloat = 3.99999999999 + 0.00000000005

-- Bool True False
-- Char '
-- Tuple (store lists made of many data points)

-- you can declare perminat value of a variable
always5 :: Int
always5 = 5

sumOfNums = sum [ 1..1000]
addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

modEx = mod 5 4 --called a prefix operator
modEx2 = 5 `mod` 4 -- infix operator as in is inbetween the two numbers

negNumEx = 5 +(-4)

{-sqrt :: Floating a => a -> a 
-- works with float numbers then recieve a value then pop put a value -}

num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9) -- converting int to floating point

-- Built in Math Functions
piVal = pi 
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

-- also sin, cos, tan, asin, atan, acos, sinh, tanh, cosh, asinh, acosh

trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

{- (+) :: Num a => a -> a -> a 
recieve two parameters and return one value 

truncate :: (Integral b, RealFrac a) => a -> b
-}

-- lists in haskell are singlarly linked

primeNumbers = [3, 5, 7, 11]

morePrimes = primeNumbers ++ [13,17,19,23,29]

favNums = 2: 7 : 21 : 66 :[] -- way to combine numbers into a list

multiList = [[3,5,7],[11,13,17]]
morePrimes2 = 2: morePrimes
lenPrime = length morePrimes2

revPrime = reverse morePrimes2
isListEmpty = null morePrimes2

secondPrime = morePrimes2 !! 1
firstPrime = head morePrimes2
lastPrime = last morePrimes2

primeInit = init morePrimes2
first3Primes = take 3 morePrimes2
removedPrimes = drop 3 morePrimes2

is7InList = 7 `elem` morePrimes2

maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2

newList = [2,3,5]
prodPrimes = product newList

zeroToTen = [0..10]

evenList = [2, 4 .. 20]
letterList = ['A','C'..'Z']

inifinPow10 = [10,20] -- haskell is lazy so doesnt work till stated how far to go up to

many2s = take 10 (repeat 2)

many3s = replicate 10 3

cycleList = take 10 (cycle [1,2,3,4,5])

listTimes2 = [x * 2| x <- [1..10]]
listTimes3 = [x * 3| x <- [1..10], x * 3 <=50]

divisBy9N13 = [x | x <-[1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [9,1,8,3,4,7,6]

sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10] 
listBiggerThan5 = filter (>5) morePrimes

evensUpTo20 = takeWhile (<=20) [2,4..] --see how infinite lists work

--foldl or foldr applys operation on each item of a list
multOfList = foldl (*) 1 [2,3,4,5] -- foldl - l stands for multiplying from the left
multOfList2 = foldr (*) 1 [2,3,4,5] -- foldr - r stands for multiplying from the right

pow3List = [3^n | n <- [1..10]]
multTable = [[x * y| y<- [1..10]]| x <-[1..10]]

--Tuples 

randTuple = (1, "Random Tuple")

bobSmith = ("Bob Smith", 52) -- tuple pair

bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["Rebecca", "Manus", "Emma"]
address =["Clonamany", "convoy", "What day is it?"] 

namesNaddress = zip names address -- creates tuple pairs

{-main = do 
putStrLn "What is your name?"
name <- getLine
putStrLn ("Hello " ++ name)-}
	
addMe :: Int -> Int -> Int

-- functionName param1 param2 = operations (returned value)
addMe x y = x + y

sumMe x y = x + y 

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String

whatAge 16 = "you can drive"
whatAge 18 = "you can vote"
whatAge x = "Nothing"

-- recursion 

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

productFact n = product [1..n]

isOdd :: Int -> Bool -- using guards
isOdd n 
	| n `mod` 2 == 0 = False
	| otherwise = True
	
isEven n = n `mod` 2 == 0

whatGrade :: Int -> String
whatGrade age
	| (age >= 5) && (age <= 6) = "Kindergarten"
	| (age > 6) && (age <= 10) = "Elementary"
	| (age > 10) && (age <= 14) = "middle school"
	| (age > 14) && (age <= 18) = "high school"
	|otherwise = "Go to college"


batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
	| avg <= 0.200 = "terrible batting average"
	| avg <= 0.250 = "Average palyer"
	| avg <= 0.280 = "You are doing pretty good"
	| otherwise = "you are a superstar"
	where avg = hits / atBats
	
getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x	
getListItems (x:y:[]) = "you list contains "++ show x ++" and " ++ show y
getListItems (x:xs) ="The first Itenm is " ++ show x ++ " and the rest are " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

-- higher order functions

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]
multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

-- [1,2,3,4,5] : remeber x = 1, xs = [2,3,4,5]
-- [2,3,4,5]: x = 2, xs =[3,4,5]  etc

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3
num3Times4 = doMult times4

getAddFunc :: Int -> (Int->Int)
getAddFunc x y = x + y
adds3 = getAddFunc 3

fourPlus3 = adds3 4

threePlusList = map adds3 [1,2,3,4,5]

dbl1To10 = map (\x -> x*2) [1..10] -- \ is lambda

-- /= is not equal to, < , > , <=, >=, ==
-- && || not 

doubleEvenNumber y = 
	if (y `mod` 2 /= 0)
	then y 
	else y *2
	
getClass :: Int -> String 
getClass n = case n of
	5 -> "go to kindergarten"
	6 -> "go to elementary school"
	_ -> "go away"

	
{-module SampFunctions (getClass, doubleEvenNumber) where	

import SampFunctions -}

data BaseballPlayer = Pitcher
					|Catcher
					|Infielder
					|Outfielder
				deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfielder = True
barryInOf = print(barryBonds Outfielder)


data Customer = Customer String String Double
	deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main st" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b	

data RPS = Rock | Paper | Scissors
shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats Rock"
shoot Rock Scissors = "Rock beats Scissors"
shoot Scissors Paper = "Scissors beats Paper"
shoot Scissors Rock = "Scissors loses to Rock"
shoot Paper Scissors = "Paper loses to Scissors"
shoot Rock Paper = "Rock beats Paper"
shoot _ _ = "Error"


data Shape = Circle Float Float Float | Rectangle Float Float Float Float
	deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r^2
area (Rectangle x y xz yz) = (abs $ xz - x)* (abs  $ yz - y)

sumValue = putStrLn (show (1+2))
sumValue2 = putStrLn . show $ 1 + 2


areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 10 100 20

-- Show is a type class

data Employee = Employee { name :: String,
							position :: String,
							idNum :: Int } deriving (Eq, Show)
samSmith = Employee {name = "Sam Smith", position = "Manager", idNum = 1000}
pamMark = Employee {name = "Pam Mark", position = "Sales", idNum = 1001}

isSamPam = samSmith == pamMark 

samSmithData = show samSmith


data ShirtSize = S | M | L

instance Eq ShirtSize where
	S == S = True
	M == M = True
	L == L = True
	_ == _ = False
	
instance Show ShirtSize where
	show S = "Small"
	show M = "Medium"
	show L = "Large"
	
smallAvail = S `elem` [S,M,L]

theSize = show S


class MyEq a where	
	areEqual :: a-> a-> Bool

instance MyEq ShirtSize where	
		areEqual S S = True
		areEqual M M = True
		areEqual L L = True
		areEqual _ _ = False
		
newSize = areEqual M M

sayHello = do 
	putStrLn "What is your name?"
	name <- getLine
	putStrLn $ "Hello " ++ name

	
writeToFile = do 
	theFile <- openFile "test.txt" WriteMode
	hPutStrLn theFile ("Random line of text")
	hClose theFile
	
readFromFile = do 
	theFile2 <- openFile "test.txt" ReadMode
	contents <- hGetContents theFile2
	putStr contents
	hClose theFile2
	
fib = 1 : 1 : [a + b | (a,b) <-zip fib (tail fib)]
-- 1, 1, 2, 3, 5, 8,... etc
-- zip fib (tail fib) using recursion and using zip to create tuple pairs

--  start with [1,1], then go to zip  fib (tail fib) - where fib = 1 and tail fib = 1 and pass it to a + b | (a,b)
-- so then we have [1,1,2] then look at fib = 1 and tail fib = 2  getting you [1,1,2,3] and so on 

fib300 =  fib !! 300

 


	
