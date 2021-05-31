add10toall :: [Int] -> [Int]
add10toall x = [x+10 | x <- x]

multN :: Int -> [Int] -> [Int]
multN n lista = [x*n | x <- lista] 

multN' :: Int -> [Int] -> [Int]
multN' n lista = map ((\x y -> x*y)n) lista

applyExpr :: [Int] -> [Int]
applyExpr lista = [3*x+2 | x <- lista]

applyExpr' :: [Int] -> [Int]
applyExpr' lista = map (\x -> 3*x+2) lista

addSuffix :: String -> [String] -> [String]
addSuffix string lista = [x++string | x <- lista]

selectgt5 :: [Int] -> [Int]
selectgt5 lista = [x | x <- lista, x > 5]

sumOdds :: [Int] -> Int
sumOdds lista = sum([x | x <- lista, odd x])

sumOdds' :: [Int] -> Int
sumOdds' lista = sum$filter odd lista

selectExpr :: [Int] -> [Int]
selectExpr lista = [x | x <- lista, even x && 20 <= x && 50 >= x]

countShorts :: [String] -> Int
countShorts lista = length([x | x <- lista, length x < 5])

calcExpr :: [Float] -> [Float]
calcExpr lista = [x^2/2 | x <- lista, x > 10]

trSpaces :: String -> String
trSpaces lista = [if x == ' ' then '-' else x| x <- lista]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd tupla = [snd x | x <- tupla]

dotProd :: [Int] -> [Int] -> Int
dotProd lista1 lista2 = sum [fst x * snd x | x <- zip lista1 lista2]