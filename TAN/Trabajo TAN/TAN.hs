import Data.Numbers.Primes
import Data.Matrix
import Data.List

-- Introducimos una lista de n números que nos sirve como sucesión numérica.
-- En este caso, vamos a tomar la lista de los 10^24 primeros primos
suc :: [Int]
suc = take (10^24) primes

-- Construimos la función que nos dé la lista en la siguiente etapa
fun :: [Int] -> [Int]
fun xs = zipWith (\x y -> abs (x-y)) xs (tail xs)

maxim n = [2]++[2^(s-1)+1 | s<-[2..n]]

cambiomaria v = v
-- Lista infinita de listas infinitas de la pirámide de Gilbreath. Para otros triángulos cambia primes por la lista
gilbreath_seq :: [[Int]]
gilbreath_seq = iterate fun suc

-- Comprueba la conjetura de Gilbreath para hasta n primos
conjetura :: Int -> Bool
conjetura n = all (==1) . take n . tail . map head $ gilbreath_seq

triangulo :: Int -> [[Int]]
triangulo n = [take (n-i) xs | (i,xs) <- zip [0..] (take n gilbreath_seq)]

dibujaTriangulo :: Int -> IO ()
dibujaTriangulo n = mapM_ putStrLn $ map (unwords . map show) a
    where a = take n (triangulo n)

-- Vamosos a construir el árbol de [2,3]

primero xs = [s | s<-[a+2..b], (iterate fun (xs ++ [s]))!! (length xs) == [1]]
         where a = last xs
               b = (a+1)*(a+1)
prim xs = [s | s<-[3..b], (iterate fun (xs ++ [s]))!! (length xs) == [1]]
         where a = last xs
               b = (a+1)*(a+1)

conet xs = (primero xs, length $ primero xs, length $ prim xs)

menor [] [] = True
menor (x:xs) (y:ys) = x <= y && menor xs ys



qsubn 2 = [2,3]
qsubn 56 = [2,3,5,9,11,21,21,27,27,33,41,51,55,57,53,63,67,89,85,95,105,97,109,105,121,129,137,129,135,137,155,217,189,205,191,213,201,207,213,211,213,231,221,255,241,263,245,287,319,299,297,313,299,303,311,331]
qsubn n = qsubn (n-1) ++  [last (primero $  (take (n-1) primes))]

difer n = zipWith (-) (qsubn n) (take n primes) 

primg = [if isPrime (p-2) then 1 else 0 | p<-primes]
conj2 n = [if x == 0 then -1 else xs!!y - xs!!(y+1)| (x,y)<-(zip ([0]++primg) [0..n])]
    where xs = qsubn n
