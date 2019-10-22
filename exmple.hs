
module Example where    

    
myLast :: [a] -> a
myLast [] = error "no last for empty List"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "error"
-- myButLast [x] = error "error"
myButLast [x] = myButLast []
myButLast [x, y] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a 
elementAt xs n 
    | n > length xs = error "index must > length of input list"
    | n == 1 = head xs
    | otherwise = elementAt (tail xs) (n-1)
 

myLength :: [a] -> Int
myLength [] = 0
myLength xs = (+) 1 $ myLength $ tail xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = let rev = myReverse xs in xs == rev

compress :: Eq a => [a] -> [a]   -- 这里使用group函数会很方便
compress [x] = [x]
compress [] = []
compress (x:xs) = let com = compress xs in if head xs == x 
    then com 
    else x : com 


-- pack :: Eq a => [a] -> [[a]]
-- pack = group

--手写一个group呢
pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' allx = let x = head allx; foo = (x==) in takeWhile foo allx : pack' (dropWhile foo allx)

-- main :: IO ()
-- main = do
--     print $ myLast [1]
--     print $ myLast [2, 4..10]
--     print $ myLast []
--     return ()

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack'

data ItemList a = Single a | Double a Int  -- must be upper case 
    deriving (Show)

itemLength :: Eq a => [a] -> ItemList a   
itemLength x 
    | l > 1 = Double (head x) l
    | otherwise = Single $ head x
    where l = length x

encodeModified :: Eq a => [a] -> [ItemList a]
encodeModified = map itemLength . pack'

decode :: Eq a => [(Int, a)] -> [a]
decode = concatMap $ uncurry replicate

replicateItem :: Eq a => ItemList a -> [a]
replicateItem (Single x) = [x]
replicateItem (Double x n) = replicate n x

decodeModified :: Eq a => [ItemList a] -> [a]
decodeModified = concatMap replicateItem

dupli :: [a] -> [a]
dupli xs = xs >>= (\x -> [x, x])

repli :: [a] -> Int -> [a]
repli xs n = xs >>= replicate n

dropEvery :: [a] -> Int -> [a]
dropEvery xs n
    | length xs >= n = init (fst split) ++ dropEvery (snd split) n
    | otherwise = xs
    where split = splitAt n xs

type T2 a = (a, a)

wrapT2 :: a -> T2 [a] -> T2 [a]
wrapT2 n (x, y) = (n : x, y)

split :: [a] -> Int -> T2 [a]
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) n = wrapT2 x (split xs $ n - 1) 
    -- | length xs > n = 

removeAt :: Int -> [a] -> [a]  -- 假定没有非法输入
removeAt 1 xs = tail xs
removeAt n (x:xs) = x : removeAt (n - 1) xs 

removeAtWith :: Int -> [a] -> (a, [a])
removeAtWith 1 (x:xs) = (x, xs)
removeAtWith n (x:xs) = (\y -> x : y) <$> removeAtWith (n-1) xs


sortFn :: Ord b => [a] -> (a -> b) -> [a]
sortFn [] _ = []
sortFn all@(x:xs) f = sortFn (filter (\y -> f y < f x) xs) f ++ [x] ++ sortFn (filter (\y -> f y >= f x) xs) f

combination :: [a] -> [[a]]
combination [] = [[]]
combination xs = let comHelper n xs = map (\x -> n : x) $ combination xs in concatMap (uncurry comHelper) $ removeAtWith <$> [1..length xs] <*> [xs]

combinationBy :: Int -> [a] -> [[a]]
combinationBy 0 xs = [[]]
combinationBy num xs = let comHelper n xs = map (\x -> n : x) $ combinationBy (num-1) xs in concatMap (uncurry comHelper) $ removeAtWith <$> [1..length xs] <*> [xs]
