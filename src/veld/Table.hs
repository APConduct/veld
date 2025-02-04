{-# LANGUAGE GADTs #-}
module Veld.Table where
import qualified Data.Map as Map
import Veld.Veldon

-- Define the Table type
data VeldTable = VeldTable (Map.Map String VeldonInstance)

data VeldonInstance = forall a. Veldon a => VeldonValue a

instance Veldon VeldonInstance where
    show_veldon (VeldonValue a) = show_veldon a

instance Veldon VeldTable where
    show_veldon (VeldTable m) = "{" ++ show_map m ++ "}"
        where
            show_map map = concat [key ++ ": " ++ show_veldon value ++ ", " | (key, value) <- Map.toList map]
