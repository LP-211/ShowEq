module EjemploSE where

data Time = TF Int Int | TW Int Int Bool -- deriving (Show, Eq) -- deriving (Eq)

-- TF: formato de 24 horas
-- TW: formato de 12 horas, T si es am, F si es pm

instance Show Time where
  show a = showTime a

showTime :: Time -> String
showTime (TF h m)   = show h ++ " : " ++ show m ++ " HRS"
showTime (TW h m b) = show h ++ " : " ++ show m ++ if b then "PM" else "AM"

(Nodo n (Nodo m ...) (Nodo l ...))

tf1 = TF 15 24

tf2 = TF 10 09

tw1 = TW 03 24 False

tw2 = TW 10 09 False

data Persona = Edad Int | Nombre String  deriving (Show) -- deriving (Show, Eq)

instance Eq Persona where
  a == b = eqPersona a b

eqPersona :: Persona -> Persona -> Bool
eqPersona (Edad n) (Edad m)       = n == m
eqPersona (Nombre n) (Nombre m)   = n == m
eqPersona (Nombre n) (Edad m)     = n == "Carlos" && m == 32
eqPersona a@(Edad n) b@(Nombre m) = eqPersona b a

p1 = Edad 90

p2 = Edad 32

p3 = Nombre "Carlos"

p4 = Nombre "Sofia"
