module Main where

import qualified MyLib (someFunc)
import Veld.Primitives
import Veld.Table
import Veld.Veldon
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    MyLib.someFunc

    let num = VeldNumber 42.0
    let str = VeldString "Hello, World!"
    let table = VeldTable (Map.fromList [("key1", VeldonValue num), ("key2", VeldonValue str)])

    -- Print the table
    putStrLn (show_veldon table)

    -- Add a new value to the table
    let updated_table = add_to_table "key3" (VeldonValue (VeldNumber 3.14)) table
    putStrLn (show_veldon updated_table)
