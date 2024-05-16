module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

factorial :: Number -> Number
factorial 0 = 1 --caso base
factorial n = n * factorial (n-1) --caso recursivo

largo :: [a] -> Number
largo [] = 0
largo (x:xs) = 1 + largo xs

ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

cabeza :: [a] -> a
cabeza (x:xs) = x

segundo :: [a] -> a
segundo (x:xs) = cabeza xs

segundo' :: [a] -> a
segundo' (x:y:ys) = y

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False --variable anonima
any' condicion (x:xs) = condicion x 
                      || any' condicion xs

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' condicion (x:xs) = condicion x 
                        && all' condicion xs

data Persona = UnaPersona { nombre :: String,
                           peso :: Number} deriving Show
data Rosquilla = UnaRosquilla { gramos :: Number } deriving Show

homero :: Persona 
homero = UnaPersona "Homero" 130000

rosquilla1 = UnaRosquilla 300
rosquilla2 = UnaRosquilla 100
rosquilla3 = UnaRosquilla 50
rosquilla4 = UnaRosquilla 1000

comerUna :: Persona -> Rosquilla -> Persona
comerUna persona rosquilla = 
      persona {peso = peso persona * 0.9999999999 
      + gramos rosquilla}

panzada :: Persona -> [Rosquilla] -> Persona
panzada persona [] = persona
panzada persona (rosquilla:rosquillas) = 
          panzada (comerUna persona rosquilla) rosquillas

panzada' persona [] = persona
panzada' persona (r:rs) =
              comerUna (panzada' persona rs) r

panzada'' persona rosquillas = foldl comerUna persona rosquillas
panzada''' persona rosquillas = foldr (flip comerUna) persona rosquillas


--foldl :: (a -> b -> a) -> a -> [b] -> a
--foldr :: (b -> a -> a) -> a -> [b] -> a


maximum' :: Ord a => [a] -> a
maximum' (x:xs) = foldl max x xs

maximum'' lista = foldl1 max lista

-- foldl foldl1 foldr foldr1

--foldl1 :: (a -> a -> a) -> [a] -> a

enterosDesde :: Number -> [Number]
enterosDesde x = x : enterosDesde (x + 1)

--EAGER EVALUATION - LAZY EVALUATION
