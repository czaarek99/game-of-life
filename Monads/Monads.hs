module Monads where

import Data.Char

data Person = Person { firstName :: String
                     , lastName :: String
                     , phoneNumber :: PhoneNumber
                     , age :: Int
                     } deriving  (Show, Eq)



newtype PhoneNumber = PhoneNumber String deriving (Show, Eq)


readPerson :: String -> Either String Person
readPerson str = do
  (a,b,c,d) <- readFourSplit str
  fn <- validName a
  sn <- validName b
  ph <- validPhone c
  i <- validAge d
  return $ Person fn sn ph i



readPersonAlt str = do
    (a,b,c,d) <- readFourSplit str
    Person <$> validName a
           <*> validName b
           <*> validPhone c
           <*> validAge d

readFourSplit :: String -> Either String (String, String, String, String)
readFourSplit str =
  case words str of
    [] -> Left "Cannot split empty string into three"
    [_] -> Left "Well there's threeelement to few"
    [_,_] -> Left "We are missing two argument here brah"
    [_,_,_] -> Left "1 + 1 + 1 == 3 not 4 dummy"
    (a:b:c:d:_) -> Right (a,b,c,d)

validName :: String -> Either String String
validName string =
  if any (not . isAlpha) string
  then Left "Names should only cotain letters brah"
  else Right string

validPhone :: String -> Either String PhoneNumber
validPhone string =
  if any (not . isNumber) string
  then Left "Names should only cotain letters brah"
  else Right (PhoneNumber string)



validAge :: String -> Either String Int
validAge string =
  if any (not . isNumber) string
  then Left "Names should only cotain letters brah"
  else Right (read string)


-------------- LISTS


listShtuff :: [a] -> [(a,a)]
listShtuff [] = []
listShtuff inpt = do
  a <- inpt
  b <- inpt
  pure (a,b)



------------ APLLICATIVES
