{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- Uses multi-param type classes
-- Copied from Data.ByteString
--
module QuickCheckUtils where

import Test.QuickCheck.Batch
import Test.QuickCheck
import Text.Show.Functions

import Control.Monad        ( liftM, liftM2 )
import qualified Control.Exception as E
import Data.Char
import Data.List
import Data.Word
import Data.Int
import Debug.Trace
import System.Random
import System.IO
import System.IO.Unsafe

import qualified Data.CompactString as C
import qualified Data.ByteString    as B
import Data.CompactString.Encodings

-- Enable this to get verbose test output. Including the actual tests.
debug = False

mytest :: Testable a => a -> Int -> IO ()
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO ()
mycheck config a =
  do rnd <- newStdGen
     mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()
mytests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK," ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps =
  do putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"


------------------------------------------------------------------------
-- Debugging

-- | Do a test on a property with 1 argument, return the first argument that fails
firstFail :: Arbitrary a => (a -> Bool) -> Int -> IO (Maybe a)
firstFail f n =
  do rnd <- newStdGen
     firstFail' defaultConfig{configMaxTest=n} arbitrary f rnd 0

firstFail' :: Config -> Gen a -> (a -> Bool) -> StdGen -> Int -> IO (Maybe a)
firstFail' config gen test rnd0 ntest
 | ntest == configMaxTest config = return Nothing
 | otherwise = case test input of
           False -> do
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests"
                    )
             hFlush stdout
             return (Just input)
           _ ->
             firstFail' config gen test rnd1 (ntest + 1)
   where
      input       = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

-- | Do a test, comparing two functions, return the first argument and result that lead to inequality
firstNeq :: (Eq b, Arbitrary a) => (a -> b) -> (a -> b) -> Int -> IO (Maybe (a, b, b))
firstNeq f1 f2 n =
  do rnd <- newStdGen
     firstNeq' defaultConfig{configMaxTest=n} arbitrary f1 f2 rnd 0

firstNeq' :: Eq b => Config -> Gen a -> (a -> b) -> (a -> b) -> StdGen -> Int -> IO (Maybe (a, b, b))
firstNeq' config gen t1 t2 rnd0 ntest
 | ntest == configMaxTest config = return Nothing
 | otherwise = if o1 /= o2 then do
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests"
                    )
             hFlush stdout
             return (Just (input, o1, o2))
           else
             firstNeq' config gen t1 t2 rnd1 (ntest + 1)
   where
      input       = generate (configSize config ntest) rnd2 gen
      o1          = t1 input
      o2          = t2 input
      (rnd1,rnd2) = split rnd0

------------------------------------------------------------------------
-- Types

type C = C.CompactString
type B = B.ByteString
type S = String

------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary Char where
    arbitrary = liftM toChar $ choose (0,maxToChar)
    coarbitrary c = variant (ord c `rem` 4)

maxToChar = 0x10FFFF-0x800
toChar c
    | c >= 0xD800 = chr (c + 0x800) -- skip surrogate pairs
    | otherwise   = chr c

instance Arbitrary Word8 where
    arbitrary = liftM fromIntegral $ choose (0::Int,255)
    coarbitrary c = variant (fromIntegral c `rem` 4)

instance C.Encoding a => Arbitrary (C a) where
    arbitrary = liftM C.pack arbitrary
    coarbitrary = coarbitrary . C.unpack

instance Arbitrary B where
    arbitrary = liftM B.pack arbitrary
    coarbitrary = coarbitrary . B.unpack

------------------------------------------------------------------------
-- (from ByteString)
--
-- The Model class connects a type and its model type, via a conversion
-- function. 
--
--
class Model a b where
    model :: a -> b  -- get the abstract vale from a concrete value

instance C.Encoding a => Model (C a) [Char] where model = C.unpack

-- Types are trivially modeled by themselves
instance Model Bool  Bool         where model = id
instance Model Int   Int          where model = id
instance Model Char  Char         where model = id
instance Model Ordering Ordering  where model = id

-- Type constructors are modeled recursively
instance Model a b => Model [a]       [b]       where  model = fmap model
instance Model a b => Model (Maybe a) (Maybe b) where  model = fmap model
instance Model a b => Model (x -> a)  (x -> b)  where  model = fmap model
instance (Model a b, Model c d) => Model (a,c) (b,d)  where  model (a,c) = (model a, model c)

instance Functor ((->) r) where
    fmap = (.)

instance Monad ((->) r) where
    return = const
    f >>= k = \ r -> k (f r) r

instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

------------------------------------------------------------------------
--
-- These comparison functions handle wrapping and equality.
--
-- A single class for these would be nice, but note that they differe in
-- the number of arguments, and those argument types, so we'd need HList
-- tricks. See here: http://okmij.org/ftp/Haskell/vararg-fn.lhs
--

eq0 f g = 
    model f             == g
eq1 f g = \a         ->
    model (f a)         == g (model a)
eq2 f g = \a b       ->
    model (f a b)       == g (model a) (model b)
eq3 f g = \a b c     ->
    model (f a b c)     == g (model a) (model b) (model c)
eq4 f g = \a b c d   ->
    model (f a b c d)   == g (model a) (model b) (model c) (model d)
eq5 f g = \a b c d e ->
    model (f a b c d e) == g (model a) (model b) (model c) (model d) (model e)

eq2b :: C.Encoding a
     => ((Char -> C a) -> C a -> C a)
     -> ((Char -> S) -> S -> S)
     -> (Char -> C a) -> C a -> Bool
eq2b f g = \a b       ->
    --model (f a b)       == g (model a) (model b)
    let ma = model a
        mb = model b
        mfab = model (f a b)
        gmab = g ma mb
    in if mfab == gmab then True
       else trace ("f " ++ show  a ++ " " ++ show  b ++ " = " ++ show mfab)
          $ trace ("g " ++ show ma ++ " " ++ show mb ++ " = " ++ show gmab)
          $ trace (show $ model (f a b) == g (model a) (model b))
          -- $ trace (unlines $ map (show . f a) (model b))
          -- $ trace (unlines $ map (show . (model a::Char->S)) (C.unpack b))
          $ False

--
-- And for functions that take non-null input
--
eqnotnull1 f g = \x     -> (not (isNull x)) ==> eq1 f g x
eqnotnull2 f g = \x y   -> (not (isNull y)) ==> eq2 f g x y
eqnotnull3 f g = \x y z -> (not (isNull z)) ==> eq3 f g x y z

class                    IsNull t     where isNull :: t -> Bool
instance                 IsNull S     where isNull = null
instance C.Encoding a => IsNull (C a) where isNull = C.null


------------------------------------------------------------------------
-- Exception catching

withCatch :: a -> Maybe a
withCatch a = unsafePerformIO $
        (E.evaluate a >>= return . Just)
          `E.catch`
        (\(E.ErrorCall _) -> return Nothing)

withCatch1 f x   = withCatch (f x)
withCatch2 f x y = withCatch (f x y)

--
-- Eq for functions that can throw exceptions
--
eqexcept1 f g = eq1 (withCatch1 f) (withCatch1 g)
eqexcept2 f g = eq2 (withCatch2 f) (withCatch2 g)
