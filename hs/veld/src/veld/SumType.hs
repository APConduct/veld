{-# LANGUAGE GADTs #-}

module Veld.SumType where
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

newtype Table = Table (M.Map VeldValue VeldValue)

instance Show Table where
    show (Table t) = "{" ++ show (M.toList t) ++ "}"

instance Eq Table where
    Table a == Table b = a == b

instance Ord Table where
    compare (Table a) (Table b) = compare a b

--Define all the types that a Veld value can be
data VeldValue where
    VInt :: Int -> VeldValue
    VDouble :: Double -> VeldValue
    VFloat :: Float -> VeldValue
    VChar :: Char -> VeldValue
    VString :: String -> VeldValue
    VBool :: Bool -> VeldValue
    VTable :: Table -> VeldValue
    VList :: [VeldValue] -> VeldValue
    VNil :: VeldValue

instance Show VeldValue where
    show (VInt i) = show i
    show (VDouble d) = show d
    show (VFloat f) = show f
    show (VChar c) = show c
    show (VList l) = show l
    show (VString s) = "\"" ++ s ++ "\""
    show (VBool b) = show b
    show (VTable t) = "<table>"
    show VNil = "nil"

-- Add Eq instance
instance Eq VeldValue where
    VInt a == VInt b = a == b
    VDouble a == VDouble b = a == b
    VFloat a == VFloat b = a == b
    VChar a == VChar b = a == b
    VString a == VString b = a == b
    VBool a == VBool b = a == b
    VTable a == VTable b = a == b
    VList a == VList b = a == b
    VNil == VNil = True
    _ == _ = False

-- Add Ord instance
instance Ord VeldValue where
    compare VNil VNil = EQ
    compare VNil _ = LT
    compare _ VNil = GT
    compare (VInt a) (VInt b) = compare a b
    compare (VDouble a) (VDouble b) = compare a b
    compare (VFloat a) (VFloat b) = compare a b
    compare (VChar a) (VChar b) = compare a b
    compare (VString a) (VString b) = compare a b
    compare (VBool a) (VBool b) = compare a b
    -- compare (VTable _) (VTable _) = EQ -- Simple comparison for tables for now
    compare (VTable a) (VTable b) = compare a b
    compare (VList a) (VList b) = compare a b
    -- Compare different constructors based on their ordering
    compare a b = compare (tag a) (tag b)
        where
            tag VNil = 0
            tag (VInt _) = 1
            tag (VDouble _) = 2
            tag (VFloat _) = 3
            tag (VChar _) = 4
            tag (VString _) = 5
            tag (VBool _) = 6
            tag (VTable _) = 7
            tag (VList _) = 8

normalize_key :: VeldValue -> VeldValue
normalize_key (VFloat f) =
    if fromIntegral (floor f) == f
        then VInt (floor f)
        else VFloat f
normalize_key v = v

empty_table :: Table
empty_table = Table M.empty

insert :: VeldValue -> VeldValue -> Table -> Table
insert key val (Table t) = Table (M.insert key val t)

lookup_table :: VeldValue -> Table -> VeldValue
lookup_table key (Table t) = fromMaybe VNil (M.lookup key t)
