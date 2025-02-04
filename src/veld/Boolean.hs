module Veld.Boolean where
import Veld.Veldon

data VeldBoolean = VeldBoolean Bool

instance Veldon VeldBoolean where
    show_veldon (VeldBoolean b) = show b
