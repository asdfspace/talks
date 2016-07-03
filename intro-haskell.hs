--------------------------
-- Types
--------------------------

int = 1

float :: Float
float = 2.0

double :: Double
double = 3.0

integer :: Integer
integer = 4 -- can be aribtraily sized

charList :: [Char]
charList = ['H', 'e', 'l', 'l', 'o', 'W', 'o', 'r', 'l', 'd']

string :: String
string = "HelloWorld"

bool :: Bool
bool = charList == string -- A string is simply a list of characters!


data Vec = Vec Int Int

instance Eq Vec where
	(==) (Vec x y) (Vec x' y') = x==x'
	(/=) (Vec x y) (Vec x' y') = x/=x'

------------------------------
-- Functions
------------------------------

doubleMe x = x + x

doubleUs :: Int -> Int -> Int
doubleUs x y = x * 2 + y * 2

-- void swap(int *x, int *y) {
--     int tmp = *x;
--     *x = *y;
--     *y = tmp;
-- }

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


curry = max 4 5
curry' = (max 4) 5


compose :: Ord a => [a] -> a
compose = head . reverse . reverse . reverse

------------------------------
-- Guards
------------------------------
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise                 = "You're a whale, congratulations!"

------------------------------
-- List comprehension
------------------------------

lc1 = [x*2.0 | x <- [1.0..10.0]]

lc2 = [x*2 | x <- [1..10], x*2 >= 12]

lc3 = [ x*y | x <- [2,5,10], y <- [8,10,11], y < 11 ]

-----------------------------
-- Recursion
------------------------------

-- http://stackoverflow.com/questions/28550361/insertion-sort-in-haskell/35529542#35529542
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert $ insertionSort xs
    where insert [] = [x]
          insert (y:ys)
              | x < y = x : y : ys
              | otherwise = y : insert ys

------------------------------
-- Higher Order
------------------------------

ho1 = map (+3) [1,5,3,1,6]

ho2 = filter (>3) [1,5,3,2,1,6,4,3,2,1]

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

------------------------------
-- Functor
------------------------------

divide :: Int -> Int -> Maybe a
divide x 0 = Nothing
divide x y = Just (x/y)

--instance Functor [] where
--  fmap _ [] = []
--  fmap g (x:xs) = g x : fmap xs
--
--instance Functor Maybe where
--  fmap _ Nothing =
--  fmap g (Just a) =

------------------------------
-- Monoid
------------------------------

--instance Monoid [a] where
--    mappend =
--    mempty =

------------------------------
-- Monad
------------------------------

-- IO Monad

iomonad :: IO ()
iomonad = do
   putStrLn "What is your name?"
   name <- getLine
   putStrLn ("Welcome, " ++ name ++ "!")

iomonad' = putStrLn "What is your name?" >>= (\_ -> getLine) >>= (\name -> putStrLn ("Welcome, " ++ name ++ "!"))

-- State Monad

type Stack = [Int]

pop :: Stack -> (Int,Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((),Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((),newStack1) = push 3 stack
    (a ,newStack2) = pop newStack1
    in pop newStack2

------------------------------
-- Lazy
------------------------------

complexFunc i = do
  let n = i^10000000
  return 1
