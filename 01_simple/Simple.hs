-- author: Leander Schulz

-- vim shortcuts:
-- set number
-- set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
module Simple
where

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0
fib     :: Integer -> Integer
--fib x = undefined
--fib 0 = 0
--fib 1 = 1
--fib x = (fib (x-1)) + (fib (x-2))

fib x
    | x == 0 = 0
    | x == 1 = 1
    | x >= 2 = (fib (x-1)) + (fib (x-2))
    | otherwise = error "Not allowed for negative values"

-- Definieren Sie eine Funktion fib zur Berechung der Fibonacci-Zahlen
-- ab 0 mit linearer Laufzeit
fib2    :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 x = fib3 0 0 1 x

-- params(counter, last, last2, end)
fib3 :: Integer -> Integer -> Integer -> Integer -> Integer
fib3 c x y z
    | c <  z = fib3 (c+1) (x+y) (x) z
    | c == z = x
    | c >  z = error "Something went wrong"

-- Definieren Sie eine Funktion c (für Collatz), die berechnet
-- wie viele Rekursionsschritte benötigt werden, um
-- eine natürliche Zahl n >= 1 auf 1 zu
-- reduzieren.
--
-- Folgende Reduktionsregel sind dabei anzuwenden: Wenn n gerade ist,
-- so wird n halbiert, wenn n ungerade ist, so wird n verdreifacht und um
-- 1 erhöht.
c       :: Integer -> Integer
c x = ccount x 0

ccount :: Integer -> Integer -> Integer
ccount a n 
    | a < 1        = error "Collatz value lower than 1"
    | a == 1       = n
    | mod a 2 == 0 = ccount (div a 2) (n+1)
    | otherwise    = ccount (3*a+1) (n+1)
    

-- Definieren Sie ein endrekursive Variante von c
c1      :: Integer -> Integer
c1 x 
    | x < 1     = error "Lowest allowed value is 1"
    | otherwise = c1h x 0

c1h :: Integer -> Integer -> Integer
c1h x n = 
    if x == 1 
        then n
        else if x > 1
            then c1h next (n+1)
            else error "LOL" 
    where next = if x `mod` 2 == 0 
            then x `div` 2 
            else 3*x+1


-- Definieren Sie eine Funktion cmax, die für ein
-- Intervall von Zahlen das Maximum der
-- Collatz-Funktion berechnet. Nutzen Sie die
-- vordefinierten Funkt min und max.
cmax    :: Integer -> Integer -> Integer
cmax lb ub 
    | lb == ub  = c ub
    | otherwise = max (c lb) (cmax (lb+1) ub)


-- Definieren Sie eine Funktion imax, die für ein
-- Intervall von Zahlen das Maximum einer
-- ganzzahligen Funktion berechnet. Formulieren
-- Sie die obige Funktion cmax so um, dass sie mit imax arbeitet.
imax    :: (Integer -> Integer) -> Integer -> Integer -> Integer
imax f lb ub
    | lb == ub  = f ub
    | otherwise = max (f lb) (imax f (lb+1) ub)


cmax1   :: Integer -> Integer -> Integer
cmax1
    = imax c1

-- Entwickeln Sie eine Funktion,
-- die die Position und den Wert bestimmt, an der
-- das Maximum angenommen wird.
-- Versuchen Sie, eine endrekursive Lösung zu finden
-- (mit einer lokalen Hilfsfunktion).
imax2   :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
imax2 f lb ub = undefined
    
-- ----------------------------------------
