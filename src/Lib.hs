module Lib where

import Network.HTTP
import Data.List.Split
import Data.List
import Control.Monad

rootURI = "http://www.results.manabadi.co.in/SSC2008.aspx?htno="

data Person = Person { hallTicketNumber :: Integer
                     , personName :: String
                     , fatherName :: String
                     } deriving (Show, Eq)
              
showHallTicket :: Integer -> String              
showHallTicket num
  | len < 7 = replicate (7-len) '0' ++ numString
  | len == 7 = numString
  | otherwise = error "Hall ticket number can't be more than 7 digits!"
  where numString = show num
        len = length numString

showPerson :: Person -> String
showPerson (Person hallTicket person father) = intercalate ", " [show hallTicket, person, father]
        
getPerson :: Integer -> IO (Maybe Person)
getPerson num = do respBody <- simpleHTTP (getRequest $ rootURI ++ showHallTicket num) >>= getResponseBody
                   return $ parsePerson respBody
                   
parsePerson :: String -> Maybe Person
parsePerson respBody
  | null respBody = Nothing
  | otherwise = Just $ Person hallTicket name father
  where splitted = splitOn "|" respBody
        hallTicket = read (head splitted) :: Integer
        name = head . tail $ splitted
        father = head . tail . tail $ splitted 

        
