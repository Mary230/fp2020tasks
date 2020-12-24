module Part3 where

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 n = getPrimeDivisors n == [n]

getPrimeDivisors :: Integer -> [Integer]
getPrimeDivisors = getDivisorsWithCurrent 2
    where
        getDivisorsWithCurrent :: Integer -> Integer -> [Integer]
        getDivisorsWithCurrent _ 1 = []
        getDivisorsWithCurrent divisor number
            | divisor * divisor > number = [number]
            | number `mod` divisor == 0 = divisor : getDivisorsWithCurrent divisor (number `div` divisor)
            | otherwise = getDivisorsWithCurrent (divisor + 1) number

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 number = map (\divisors -> (head divisors, length divisors)) groupEqualDivisors
    where
        groupEqualDivisors :: [[Integer]]
        groupEqualDivisors = group (getPrimeDivisors number)

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 a = a == sum (removeItem a (divisors a))

removeItem :: Integer -> [Integer] -> [Integer]
removeItem _ [] = []
removeItem x (y : ys)
  | x == y = removeItem x ys
  | otherwise = y : removeItem x ys

divisors :: Integer -> [Integer]
divisors 1 = [1]
divisors k =
  k :
  concatMap
    (\x -> [x] ++ if k `div` x == x then [] else [k `div` x])
    ( filter (\x -> k `mod` x == 0) $ takeWhile (\x -> x * x <= k) [2 ..])
    ++ [1]

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 n = quicksort (divisors n)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p : xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = product $ (map iCount) (words str)
  where
    iCount :: String -> Integer
    iCount xs = toInteger (length (filter (== 'i') xs))

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 number = iterateTriangle 1 0
    where
        iterateTriangle :: Integer -> Integer -> Bool
        iterateTriangle currentNum currentSum
            | currentSum == number = True
            | currentSum > number = False
            | otherwise = iterateTriangle (succ currentNum) (currentSum + currentNum)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 x = reversal x == x

reversal :: Integral a => a -> a
reversal = go 0
  where
    go a 0 = a
    go a b = let (q, r) = b `quotRem` 10 in go (a * 10 + r) q

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 x y = sum (divider x) == y && sum (divider y) == x
  where
    divider :: Integer -> [Integer]
    divider n = [a | a <- [1 .. (n -1)], n `rem` a == 0]

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 _ [] = Nothing
prob27 requiredSum (curHead : curTail) = withFixedCurrent curHead curTail
    where
        withFixedCurrent :: Int -> [Int] -> Maybe (Int, Int)
        withFixedCurrent _ [] = prob27 requiredSum curTail
        withFixedCurrent current (innerHead : innerTail) =
            if current + innerHead == requiredSum
            then Just (current, innerHead)
            else withFixedCurrent current innerTail

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 1 = 9
prob29 2 = 9009
prob29 3 = 906609

prob29 k = fromInteger (maximum (filter prob25 ([x * y | x <- range, y <- range])))
 where
    range = [10^k - 1, 10^k - 2..10^(k-1)]

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 n = sum [x + y |x <- [1 .. n],y <- [x+1 .. n], prob26 (toInteger x) (toInteger y)]

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
