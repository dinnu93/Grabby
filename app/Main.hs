module Main where

import Lib
import Control.Monad

main :: IO ()
main = forM_ [100000..1999999] $ \n -> do p <- getPerson n
                                          let path = "/Users/dinnu93/Desktop/person.csv"
                                          (\person ->
                                             (if null person 
                                               then print n
                                               else appendFile path . ("\n"++) $ person)) . maybe "" showPerson $ p
                                            
                                      
