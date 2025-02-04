module Veld.Primitives where

import Veld.Veldon

data VeldNumber = VeldNumber Double
data VeldString = VeldString String

instance Veldon VeldNumber where
    show_veldon (VeldNumber n) = show n

instance Veldon VeldString where
    show_veldon (VeldString s) = s
