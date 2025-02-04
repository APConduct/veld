module Veld.Veldon where
-- Define the Veldon type class
class Veldon a where
    show_veldon :: a -> String
