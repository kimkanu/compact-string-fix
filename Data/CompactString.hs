{-# OPTIONS_GHC -cpp -fno-warn-orphans #-}
-- |
-- Module      : Data.CompactString
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : untested
-- 
-- A time and space-efficient implementation of strings using
-- packed Word8 arrays, suitable for high performance use, both in terms
-- of large data quantities, or high speed requirements.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.CompactString as C
--
-- Internally, CompactStrings are encoded 'ByteString's.
--
module Data.CompactString (
        
        -- * The @CompactString@ type
        Encoding,
        CompactString,          -- abstract, instances: Eq, Ord, Show, Monoid
        
        -- * Introducing and eliminating 'CompactString's
        empty,                  -- :: CompactString
        singleton,              -- :: Char   -> CompactString
        pack,                   -- :: String -> CompactString
        unpack,                 -- :: CompactString -> String
        
        -- * Basic interface
        cons,                   -- :: Char -> CompactString -> CompactString
        snoc,                   -- :: CompactString -> Char -> CompactString
        append,                 -- :: CompactString -> CompactString -> CompactString
        head,                   -- :: CompactString -> Char
        last,                   -- :: CompactString -> Char
        tail,                   -- :: CompactString -> CompactString
        init,                   -- :: CompactString -> CompactString
        headView,               -- :: CompactString -> Maybe (Char, CompactString)
        lastView,               -- :: CompactString -> Maybe (CompactString, Char)
        null,                   -- :: CompactString -> Bool
        length,                 -- :: CompactString -> Int
        
        -- * Transforming 'CompactString's
        map,                    -- :: (Char -> Char) -> CompactString -> CompactString
        reverse,                -- :: CompactString -> CompactString
        intersperse,            -- :: Char -> CompactString -> CompactString
        intercalate,            -- :: CompactString -> [CompactString] -> CompactString
        transpose,              -- :: [CompactString] -> [CompactString]
        
        -- * Reducing 'CompactString's (folds)
        foldl,                  -- :: (a -> Char -> a) -> a -> CompactString -> a
        foldl',                 -- :: (a -> Char -> a) -> a -> CompactString -> a
        foldl1,                 -- :: (Char -> Char -> Char) -> CompactString -> Char
        foldl1',                -- :: (Char -> Char -> Char) -> CompactString -> Char
        
        foldr,                  -- :: (Char -> a -> a) -> a -> CompactString -> a
        foldr',                 -- :: (Char -> a -> a) -> a -> CompactString -> a
        foldr1,                 -- :: (Char -> Char -> Char) -> CompactString -> Char
        foldr1',                -- :: (Char -> Char -> Char) -> CompactString -> Char
        
        -- ** Special folds
        concat,                 -- :: [CompactString] -> CompactString
        concatMap,              -- :: (Char -> CompactString) -> CompactString -> CompactString
        any,                    -- :: (Char -> Bool) -> CompactString -> Bool
        all,                    -- :: (Char -> Bool) -> CompactString -> Bool
        maximum,                -- :: CompactString -> Char
        minimum,                -- :: CompactString -> Char
        
        -- * Building CompactStrings
        -- ** Scans
        scanl,                  -- :: (Char -> Char -> Char) -> Char -> CompactString -> CompactString
        scanl1,                 -- :: (Char -> Char -> Char) ->         CompactString -> CompactString
        scanr,                  -- :: (Char -> Char -> Char) -> Char -> CompactString -> CompactString
        scanr1,                 -- :: (Char -> Char -> Char) ->         CompactString -> CompactString
        
        -- ** Accumulating maps
        mapAccumL,              -- :: (acc -> Char -> (acc, Char)) -> acc -> CompactString -> (acc, CompactString)
        mapAccumR,              -- :: (acc -> Char -> (acc, Char)) -> acc -> CompactString -> (acc, CompactString)
        mapIndexed,             -- :: (Int -> Char -> Char) -> CompactString -> CompactString
        
        -- ** Unfolding CompactStrings
        replicate,              -- :: Int -> Char -> CompactString
        unfoldr,                -- :: (a -> Maybe (Char, a)) -> a -> CompactString
        unfoldrN,               -- :: Int -> (a -> Maybe (Char, a)) -> a -> (CompactString, Maybe a)
        
        -- * Substrings
        
        -- ** Breaking strings
        take,                   -- :: Int -> CompactString -> CompactString
        drop,                   -- :: Int -> CompactString -> CompactString
        splitAt,                -- :: Int -> CompactString -> (CompactString, CompactString)
        takeWhile,              -- :: (Char -> Bool) -> CompactString -> CompactString
        dropWhile,              -- :: (Char -> Bool) -> CompactString -> CompactString
        span,                   -- :: (Char -> Bool) -> CompactString -> (CompactString, CompactString)
        spanEnd,                -- :: (Char -> Bool) -> CompactString -> (CompactString, CompactString)
        break,                  -- :: (Char -> Bool) -> CompactString -> (CompactString, CompactString)
        breakEnd,               -- :: (Char -> Bool) -> CompactString -> (CompactString, CompactString)
        group,                  -- :: CompactString -> [CompactString]
        groupBy,                -- :: (Char -> Char -> Bool) -> CompactString -> [CompactString]
        inits,                  -- :: CompactString -> [CompactString]
        tails,                  -- :: CompactString -> [CompactString]
        
        -- ** Breaking into many substrings
        split,                  -- :: Char -> CompactString -> [CompactString]
        splitWith,              -- :: (Char -> Bool) -> CompactString -> [CompactString]
        
        -- ** Breaking into lines and words
        lines,                  -- :: CompactString -> [CompactString]
        words,                  -- :: CompactString -> [CompactString]
        unlines,                -- :: [CompactString] -> CompactString
        unwords,                -- :: CompactString -> [CompactString]
        
        -- * Predicates
        isPrefixOf,             -- :: CompactString -> CompactString -> Bool
        isSuffixOf,             -- :: CompactString -> CompactString -> Bool
        isInfixOf,              -- :: CompactString -> CompactString -> Bool
        
        -- ** Search for arbitrary substrings
        findSubstring,          -- :: CompactString -> CompactString -> Maybe Int
        findSubstrings,         -- :: CompactString -> CompactString -> [Int]
        
        -- * Searching CompactStrings
        
        -- ** Searching by equality
        elem,                   -- :: Char -> CompactString -> Bool
        notElem,                -- :: Char -> CompactString -> Bool
        
        -- ** Searching with a predicate
        find,                   -- :: (Char -> Bool) -> CompactString -> Maybe Char
        filter,                 -- :: (Char -> Bool) -> CompactString -> CompactString
        partition,              -- :: (Char -> Bool) -> CompactString -> (CompactString, CompactString)
        
        -- * Indexing CompactStrings
        index,                  -- :: CompactString -> Int -> Char
        elemIndex,              -- :: Char -> CompactString -> Maybe Int
        elemIndices,            -- :: Char -> CompactString -> [Int]
        elemIndexEnd,           -- :: Char -> CompactString -> Maybe Int
        findIndex,              -- :: (Char -> Bool) -> CompactString -> Maybe Int
        findIndexEnd,           -- :: (Char -> Bool) -> CompactString -> Maybe Int
        findIndices,            -- :: (Char -> Bool) -> CompactString -> [Int]
        count,                  -- :: Char -> CompactString -> Int
        
        -- * Zipping and unzipping CompactStrings
        zip,                    -- :: CompactString -> CompactString -> [(Char,Char)]
        zipWith,                -- :: (Char -> Char -> c) -> CompactString -> CompactString -> [c]
        zipWith',               -- :: (Char -> Char -> Char) -> CompactString -> CompactString -> CompactString
        unzip,                  -- :: [(Char,Char)] -> (CompactString,CompactString)
        
        -- * Ordered CompactStrings
        sort,                   -- :: CompactString -> CompactString
        compare',               -- :: CompactString a -> CompactString b -> Ordering
        
        -- * Encoding
        toByteString,           -- :: Encoding a => CompactString a -> ByteString
        fromByteString,         -- :: Encoding a => ByteString -> m (CompactString a)
        fromByteString_,        -- :: Encoding a => ByteString -> CompactString a
        validate,               -- :: Encoding a => CompactString a -> m (CompactString a)
        validate_,              -- :: Encoding a => CompactString a -> CompactString a
        -- ** Encoding conversion
        module Data.CompactString.Encodings,
        recode,                 -- :: (Encoding a, Encoding b) => CompactString a -> m (CompactString b)
        recode_,                -- :: (Encoding a, Encoding b) => CompactString a -> CompactString b
        encode,                 -- :: (Encoding a, Encoding b) => a -> CompactString b -> m (ByteString)
        encode_,                -- :: (Encoding a, Encoding b) => a -> CompactString b -> ByteString
        decode,                 -- :: (Encoding a, Encoding b) => a -> ByteString -> m (CompactString b)
        decode_,                -- :: (Encoding a, Encoding b) => a -> ByteString -> CompactString b
        encodeBOM,              -- :: Encoding a => CompactString a -> m (ByteString)
        encodeBOM_,             -- :: Encoding a => CompactString a -> ByteString
        decodeBOM,              -- :: Encoding a => ByteString -> m (CompactString a)
        decodeBOM_,             -- :: Encoding a => ByteString -> CompactString a
        
        -- * I\/O with 'CompactString's
        
        -- ** Standard input and output
        getLine,                -- :: IO CompactString
        getContents,            -- :: IO CompactString
        putStr,                 -- :: CompactString -> IO ()
        putStrLn,               -- :: CompactString -> IO ()
        interact,               -- :: (CompactString -> CompactString) -> IO ()
        
        -- ** Files
        readFile,               -- :: FilePath -> IO CompactString
        readFile',              -- :: FilePath -> IO CompactString
        writeFile,              -- :: FilePath -> CompactString -> IO ()
        writeFile',             -- :: FilePath -> CompactString -> IO ()
        appendFile,             -- :: FilePath -> CompactString -> IO ()
        appendFile',            -- :: FilePath -> CompactString -> IO ()
        
        -- ** I\/O with Handles
        hGetLine,               -- :: Handle -> IO CompactString
        hGetContents,           -- :: Handle -> IO CompactString
        hGetContents',          -- :: Handle -> IO CompactString
        hGet,                   -- :: Handle -> Int -> IO CompactString
        hGetNonBlocking,        -- :: Handle -> Int -> IO CompactString
        hPut,                   -- :: Handle -> CompactString -> IO ()
        hPutStr,                -- :: Handle -> CompactString -> IO ()
        hPutStrLn,              -- :: Handle -> CompactString -> IO ()
        
        ) where

import Prelude hiding
        (length, head, tail, last, init, null, 
         map, reverse, foldl, foldr, foldl1, foldr1, concat, concatMap,
         scanl, scanl1, scanr, scanr1, replicate,
         take, drop, splitAt, takeWhile, dropWhile,
         span, break, any, all, elem, notElem,
         maximum, minimum, filter, zip, zipWith, unzip,
         lines, unlines, words, unwords,
         putStr, putStrLn, getContents, getLine, interact,
         readFile, writeFile, appendFile)

import Data.Monoid
import qualified Data.List as L
import Data.Maybe               (isJust, isNothing, listToMaybe)
import Data.String              (IsString(..))

import Foreign.Ptr              (minusPtr)
import Foreign.ForeignPtr       (withForeignPtr)

import System.IO                (Handle,openFile,hClose,IOMode(..),
                                 hSeek,hTell,SeekMode(..),stdin,stdout)
import qualified System.IO      (hGetLine)
import System.IO.Unsafe         (unsafePerformIO)

import Control.Monad            (liftM,MonadPlus)
import Control.Exception        (bracket)

import Data.Char                (isSpace)

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe   as B
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as C8

import Data.CompactString.Internal
import Data.CompactString.Fusion
import Data.CompactString.Unsafe
import Data.CompactString.Encodings

-- -----------------------------------------------------------------------------
--
-- Type signatures & documentation
--

#define COMPACTSTRING CompactString a
#define DESCRIPTION the encoding @a@
#define CONTEXT Encoding a =>
#define CONTEXT_ Encoding a, 
#define IF_FIXED(a,b) b
#define IF_NOT_REPRESENT(a) a
#include "signatures.include"

-- -----------------------------------------------------------------------------
--
-- Useful macros, until we have bang patterns
--

#define STRICT1(f) f _a             | _a                                     `seq` False = undefined
#define STRICT2(f) f _a _b          | _a `seq` _b                            `seq` False = undefined
#define STRICT3(f) f _a _b _c       | _a `seq` _b `seq` _c                   `seq` False = undefined
#define STRICT4(f) f _a _b _c _d    | _a `seq` _b `seq` _c `seq` _d          `seq` False = undefined
#define STRICT5(f) f _a _b _c _d _e | _a `seq` _b `seq` _c `seq` _d `seq` _e `seq` False = undefined

-- -----------------------------------------------------------------------------
--
-- Comparison
--

instance Encoding a => Eq (CompactString a) where
        (CS a) == (CS b) = a == b

instance Encoding a => Monoid (CompactString a) where
        mempty  = empty
        mappend = append
        mconcat = concat

instance Encoding a => Ord (CompactString a) where
        compare = doCompare

doCompare :: Encoding a => CompactString a -> CompactString a -> Ordering
doCompare a b
    | validOrdering (encoding a) = compare (unCS a) (unCS b)
     -- We can't compare the ByteStrings, because the encoding results in a different ordering
    | otherwise                  = compare' a b

-- -----------------------------------------------------------------------------
--
-- Construction/destruction
--

empty = CS $ B.empty
{-# INLINE empty #-}

singleton c = cs
    where
        cs = CS $ B.unsafeCreate l p
        (l,p) = pokeCharFun (encoding cs) c
{-# SPECIALIZE singleton :: Char -> CompactString UTF8 #-}

pack str = cs
    where
        cs = CS $ B.unsafeCreate (L.sum . L.map (pokeCharLen (encoding cs)) $ str) $ \p -> go p str
        go _ []     = return ()
        go p (x:xs) = do  l <- pokeChar (encoding cs) p x
                          go (p `plusPtr` l) xs
{-# SPECIALIZE pack :: String -> CompactString UTF8 #-}

unpack cs@(CS (PS ps ss ls)) = inlinePerformIO $ withForeignPtr ps $ \p -> return (loop p ss ls)
    where
        STRICT3(loop)
        loop _ _ 0 = []
        loop _ _ l | l < 0 = error $ "string length incorrect in " ++ show (PS ps ss ls) -- This really shouldn't happen!
        loop p s l = case inlinePerformIO $ peekChar (encoding cs) (p `plusPtr` s) of
                        (l',c) -> c : loop p (s + l') (l - l')
{-# SPECIALIZE unpack :: CompactString UTF8 -> String #-}

{-# RULES

"CompactString pack/unpack" forall cs.
    pack (unpack cs) = cs
"CompactString unpack/pack" forall ls.
    unpack (pack ls) = ls

  #-}

instance Encoding a => Show (CompactString a) where
        showsPrec p = showsPrec p . unpack

instance Encoding a => IsString (CompactString a) where
        fromString = pack

-- -----------------------------------------------------------------------------
--
-- Basic interface
--

null  = B.null . unCS
{-# INLINE null #-}

length cs@(CS (PS _ _ l)) = unsafeWithBuffer cs (length_ 0 l)
    where
        STRICT3(length_)
        length_ acc 0 _ = return acc
        length_ acc n p = do  l' <- peekCharLen (encoding cs) p
                              length_ (acc + 1) (n - l') (p `plusPtr` l')
{-# INLINE length #-}

cons c cs@(CS (PS _ _ l)) = unsafeWithBuffer cs $ \src -> create (l+l') $ \dst -> do
        pokeC dst
        memcpy (dst `plusPtr` l') src (fromIntegral l)
 where (l',pokeC) = pokeCharFun (encoding cs) c
{-# INLINE cons #-}

snoc cs@(CS (PS _ _ l)) c = unsafeWithBuffer cs $ \src -> create (l+l') $ \dst -> do
        memcpy dst src (fromIntegral l)
        pokeC (dst `plusPtr` l)
 where (l',pokeC) = pokeCharFun (encoding cs) c
{-# INLINE snoc #-}

append (CS xs) (CS ys) = CS (B.append xs ys)
{-# INLINE append #-}

-- -----------------------------------------------------------------------------
--
-- Head & tail and friends
--

head cs
    | null cs   = errorEmptyList "head"
    | otherwise = unsafeHead cs
{-# INLINE head #-}

tail cs
    | null cs   = errorEmptyList "tail"
    | otherwise = unsafeTail cs
{-# INLINE tail #-}

last cs
    | null cs   = errorEmptyList "last"
    | otherwise = unsafeLast cs
{-# INLINE last #-}

init cs
    | null cs   = errorEmptyList "init"
    | otherwise = unsafeInit cs
{-# INLINE init #-}

headView cs@(CS (PS x s l))
    | null cs   = Nothing
    | otherwise = let (headlen,c) = unsafeWithBuffer cs $ peekChar (encoding cs)
                  in  Just (c, CS (PS x (s+headlen) (l-headlen)))
{-# INLINE headView #-}

lastView cs@(CS (PS x s l))
    | null cs   = Nothing
    | otherwise = let (lastlen,c) = unsafeWithBuffer cs $ peekCharRev (encoding cs)
                  in  Just (CS (PS x s (l-lastlen)), c)
{-# INLINE lastView #-}


-- -----------------------------------------------------------------------------
--
-- List functions
--

reverse cs@(CS (PS _ _ l)) = unsafeWithBufferEnd cs $ create l . reverse_ l
    where
        STRICT3(reverse_)
        reverse_ 0 _   _   = return ()
        reverse_ n src dst = do
            i <- copyCharRev (encoding cs) src dst
            reverse_ (n-i) (src `plusPtr` negate i) (dst `plusPtr` i)
{-# SPECIALIZE reverse :: CompactString UTF8 -> CompactString UTF8 #-}

intersperse c cs@(CS (PS _ _ l))
 | l == 0    = cs
 | otherwise = unsafeWithBuffer cs $ create (l+(len-1)*lc) . intersperse_copy l
   where
       (lc,pokeC) = pokeCharFun (encoding cs) c
       len        = length cs
       STRICT3(intersperse_copy)
       intersperse_copy  0 _   _   = return ()
       intersperse_copy  n src dst = do  l' <- copyChar (encoding cs) src dst
                                         intersperse_inter (n-l') (src `plusPtr` l') (dst `plusPtr` l')
       STRICT3(intersperse_inter)
       intersperse_inter 0 _   _   = return ()
       intersperse_inter n src dst = do  pokeC dst
                                         intersperse_copy  n       src               (dst `plusPtr` lc)
{-# SPECIALIZE intersperse :: Char -> CompactString UTF8 -> CompactString UTF8 #-}

transpose ps = L.map pack (L.transpose (L.map unpack ps))

-- -----------------------------------------------------------------------------
--
-- Simple loops
--

--map f = loopArr . loopUp (mapEFL f) NoAcc
map f cs@(CS (PS _ _ l)) = result
  where
    result = CS $ inlinePerformIO $ withBuffer cs $ \p1 -> B.createAndTrim (newSize (encoding cs) l) $
             map_ p1
    map_ p1 p2 = go 0 0
      where
        STRICT2(go)
        go i j
           | i >= l     = return j
           | otherwise  = do
                (l1,c1) <- peekChar (encoding cs) (p1 `plusPtr` i)
                l2      <- pokeChar (encoding cs) (p2 `plusPtr` j) (f c1)
                go (i+l1) (j+l2)
{-# INLINE map #-}

--filter p = loopArr . loopUpC (filterEFL p) NoAcc
filter predicate cs@(CS (PS _ _ l))
    | null cs   = cs
    | otherwise = result
  where
    result = CS $ unsafeWithBuffer cs $ \p1 -> B.createAndTrim l $ filter_ p1
    filter_ p1 p2 = go 0 0
      where
        STRICT2(go)
        go i j | i >= l  = return j
               | otherwise = do
                    (l1,c1) <- peekChar (encoding cs) (p1 `plusPtr` i)
                    if predicate c1
                        then do l2 <- pokeChar (encoding cs) (p2 `plusPtr` j) c1
                                go (i+l1) (j+l2)
                        else    go (i+l1) j
{-# INLINE filter #-}

partition p cs = (filter p cs, filter (not . p) cs)

-- -----------------------------------------------------------------------------
--
-- Folds
--

foldl f z = loopUpFold f z
foldl' = foldl
foldr  f z = loopDownFold (flip f) z -- TODO : Is this too strict?
foldr' f z = loopDownFold (flip f) z


foldl1 f cs
    | null cs   = errorEmptyList "foldl1"
    | otherwise = foldl f (unsafeHead cs) (unsafeTail cs)
{-# INLINE foldl1 #-}

foldl1' f cs
    | null cs   = errorEmptyList "foldl1'"
    | otherwise = foldl' f (unsafeHead cs) (unsafeTail cs)
{-# INLINE foldl1' #-}

foldr1 f cs
    | null cs        = errorEmptyList "foldr1"
    | otherwise      = foldr f (unsafeLast cs) (unsafeInit cs)
{-# INLINE foldr1 #-}

foldr1' f cs
    | null cs        = errorEmptyList "foldr1'"
    | otherwise      = foldr' f (unsafeLast cs) (unsafeInit cs)
{-# INLINE [1] foldr1' #-}

-- ---------------------------------------------------------------------
-- Special folds

concat = CS . B.concat . L.map unCS

concatMap f = concat . foldr ((:) . f) []

intercalate s = concat . (L.intersperse s)
{-# INLINE [1] intercalate #-}

any p = isJust . find p
all p = isNothing . find (not . p)

maximum cs
    | null cs   = errorEmptyList "maximum"
    | otherwise = foldl1' max cs
minimum cs
    | null cs   = errorEmptyList "minimum"
    | otherwise = foldl1' min cs

-- ---------------------------------------------------------------------
-- Building CompactString : scans

scanl f z ps = loopArr . loopUp (scanEFL f) z $ (ps `snoc` '\0')
{-# INLINE scanl #-}

scanl1 f ps
    | null ps   = empty
    | otherwise = scanl f (unsafeHead ps) (unsafeTail ps)
{-# INLINE scanl1 #-}

scanr f z ps = loopArr . loopDown (scanEFL (flip f)) z $ ('\0' `cons` ps) -- extra space
{-# INLINE scanr #-}

scanr1 f cs
    | null cs   = empty
    | otherwise = scanr f (unsafeLast cs) (unsafeInit cs)
{-# INLINE scanr1 #-}

-- ---------------------------------------------------------------------
-- Building CompactString : Accumulating maps

mapAccumL f z = unSP . loopUp (mapAccumEFL f) z
{-# INLINE mapAccumL #-}
mapAccumR f z = unSP . loopDown (mapAccumEFL f) z
{-# INLINE mapAccumR #-}
mapIndexed f = loopArr . loopUp (mapIndexEFL f) 0
{-# INLINE mapIndexed #-}

-- ---------------------------------------------------------------------
-- Building CompactString : unfolding

replicate w c
    | w <= 0    = empty
    | otherwise = cs
 where
    cs = CS $ B.unsafeCreate (l*w) (go w)
    (l,pokeC) = pokeCharFun (encoding cs) c
    STRICT2(go)
    go 0 _   = return ()
    go n ptr = pokeC ptr >> go (n-1) (ptr `plusPtr` l)
{-# SPECIALIZE replicate :: Int -> Char -> CompactString UTF8 #-}

unfoldr f = concat . unfoldChunk 32 64
  where unfoldChunk n n' x =
          case unfoldrN n f x of
            (s, Nothing) -> s : []
            (s, Just x') -> s : unfoldChunk n' (n+n') x'


unfoldrN i f x0
    | i <= 0    = (empty, Just x0)
    | otherwise = result
  where
   result = (\(b,s) -> (CS b,s)) $ unsafePerformIO $ B.createAndTrim' (byteCount (encoding (fst result)) i) go_
   go_ start_p = go start_p x0 0
    where
        STRICT3(go)
        go p x n
         | n == i    = return (0, p `minusPtr` start_p, Just x)
         | otherwise = case f x of
                          Nothing     -> return (0, p `minusPtr` start_p, Nothing)
                          Just (w,x') -> do l <- pokeChar (encoding (fst result)) p w
                                            go (p `plusPtr` l) x' (n+1)


-- ---------------------------------------------------------------------
-- Substrings

take n cs@(CS (PS x s l))
    | n <= 0    = empty
    | idx == l  = cs
    | otherwise = CS $ PS x s idx
    where idx = charIndex cs n
{-# INLINE take #-}

drop n cs@(CS (PS x s l))
    | n <= 0    = cs
    | idx == l  = empty
    | otherwise = CS $ PS x (s+idx) (l-idx)
    where idx = charIndex cs n
{-# INLINE drop #-}

splitAt n cs@(CS (PS x s l))
    | n <= 0    = (empty, cs)
    | idx == l  = (cs, empty)
    | otherwise = (CS $ PS x s idx, CS $ PS x (s+idx) (l-idx))
    where idx = charIndex cs n
{-# INLINE splitAt #-}

takeWhile f cs@(CS bs) = CS $ B.unsafeTake (findIndexOrEnd (not . f) cs) bs
dropWhile f cs@(CS bs) = CS $ B.unsafeDrop (findIndexOrEnd (not . f) cs) bs
{-# INLINE takeWhile #-}
{-# INLINE dropWhile #-}

break p cs@(CS bs) = (CS $ B.unsafeTake n bs, CS $ B.unsafeDrop n bs)
    where n = findIndexOrEnd p cs
span p ps = break (not . p) ps
{-# INLINE [1] break #-}
{-# INLINE [1] span #-}

breakEnd p cs@(CS bs) = (CS $ B.unsafeTake n bs, CS $ B.unsafeDrop n bs)
    where n = findIndexOrBeginRev p cs
spanEnd p cs = breakEnd (not . p) cs
{-# INLINE [1] breakEnd #-}
{-# INLINE [1] spanEnd #-}

-- ---------------------------------------------------------------------
-- To list

group = groupBy (==)

groupBy k cs@(CS bs) = case headView cs of
     Nothing     -> []
     Just (x,xs) -> let n = pokeCharLen (encoding cs) x  +  findIndexOrEnd (not . k x) xs
                    in  CS (B.unsafeTake n bs) : groupBy k (CS (B.unsafeDrop n bs))


inits cs@(CS (PS x s l)) = inits_ 0
    where
        STRICT1(inits_)
        inits_ n
          | n >= l    = [cs]
          | otherwise = unsafeWithBuffer cs $ \ptr -> do
                          len <- peekCharLen (encoding cs) (ptr `plusPtr` n)
                          return (CS (PS x s n) : inits_ (n + len))

tails p | null p    = [empty]
        | otherwise = p : tails (unsafeTail p)


split x = splitWith (x==)

splitWith _ (CS (PS _ _ 0)) = []
splitWith p cs = loop p cs
    where
        STRICT2(loop)
        loop q qs = if null rest then [chunk]
                                 else chunk : loop q (unsafeTail rest)
            where (chunk,rest) = break q qs


lines = dropEmptyLast . split '\n'
    where  dropEmptyLast []           = []
           dropEmptyLast [x] | null x = []
           dropEmptyLast (x:xs)       = x : dropEmptyLast xs
{-# INLINE lines #-}

unlines [] = empty
unlines ss = (concat $ L.intersperse nl ss) `append` nl
    where nl = singleton '\n'

words = L.filter (not . null) . splitWith isSpace
{-# INLINE words #-}

unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- ---------------------------------------------------------------------
-- Searching

isPrefixOf (CS a) (CS b) = B.isPrefixOf a b
isSuffixOf (CS a) (CS b) = B.isSuffixOf a b

isInfixOf p s
 | validSubstring (encoding p) = B.isInfixOf (unCS p) (unCS s)
 | otherwise                   = not $ L.null $ findSubstrings p s
{-# INLINE isInfixOf #-}

findSubstring = (listToMaybe .) . findSubstrings

-- NOTE: We can't just call the ByteString version, because a three byte
--       encoded char has a valid 1 byte encoded char as a substring.
--       TODO: copy the KMP algorithm from Data.ByteString here
findSubstrings pat@(CS (PS _ _ m)) = search 0
    where
        STRICT2(search)
        search i str@(CS (PS _ _ n))
            | n < m                = []
            | pat `isPrefixOf` str = i : search (i+1) (unsafeTail str)
            | otherwise            =     search (i+1) (unsafeTail str)

-- ---------------------------------------------------------------------
-- Indexing CompactString

index cs@(CS (PS _ _ l)) n
 | n < 0     = moduleError "index" ("negative index: " ++ show n)
 | idx == l  = moduleError "index" ("index too large: " ++ show n ++ ", length = " ++ show (length cs))
 | otherwise = snd $ unsafeWithBuffer cs $ peekChar (encoding cs) . (`plusPtr` idx)
 where idx = charIndex cs n
{-# INLINE index #-}

-- ---------------------------------------------------------------------
-- Element searching

elem    c = any (c==)
notElem c = not . elem c
{-# INLINE elem #-}
{-# INLINE notElem #-}

elemIndex    c = findIndex    (c==)
elemIndexEnd c = findIndexEnd (c==)
elemIndices  c = findIndices  (c==)
{-# INLINE elemIndex #-}


find f cs@(CS (PS _ _ l)) = unsafeWithBuffer cs $ \ptr -> go ptr (ptr `plusPtr` l)
    where
        STRICT2(go)
        go p q | p == q    = return Nothing
               | otherwise = do (l',c) <- peekChar (encoding cs) p
                                if f c then return (Just c) 
                                       else go (p `plusPtr` l') q
{-# INLINE find #-}

findIndex f cs@(CS (PS _ _ l)) = unsafeWithBuffer cs $ \ptr -> go 0 ptr (ptr `plusPtr` l)
    where
        STRICT3(go)
        go n p q | p == q    = return Nothing
                 | otherwise = do (l',c) <- peekChar (encoding cs) p
                                  if f c then return (Just n) 
                                         else go (n+1) (p `plusPtr` l') q
{-# INLINE findIndex #-}

findIndexEnd f cs@(CS (PS _ _ l)) = unsafeWithBufferEnd cs $ \ptr -> go 1 ptr (ptr `plusPtr` (-l))
    where
        STRICT3(go)
        go n p q | p == q    = return Nothing
                 | otherwise = do (l',c) <- peekCharRev (encoding cs) p
                                  if f c then return $! Just $! length cs - n
                                         else go (n+1) (p `plusPtr` (-l')) q

findIndices p cs = loop 0 cs
    where
        STRICT2(loop)
        loop n qs = case headView qs of
                     Nothing       -> []
                     Just (q',qs')
                      | p q'       -> n : loop (n+1) qs'
                      | otherwise  ->     loop (n+1) qs'


count c = foldl' (\l x -> if x == c then l + 1 else l) 0


-- ---------------------------------------------------------------------
-- Zipping

zip xxs yys = case (headView xxs, headView yys) of
    (Nothing,     _          ) -> []
    (_,           Nothing    ) -> []
    (Just (x,xs), Just (y,ys)) -> (x,y) : zip xs ys

zipWith f xxs yys = case (headView xxs, headView yys) of
    (Nothing,     _          ) -> []
    (_,           Nothing    ) -> []
    (Just (x,xs), Just (y,ys)) -> f x y : zipWith f xs ys
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] zipWith #-}
#endif

zipWith' f a@(CS (PS _ _ l)) b@(CS (PS _ _ m)) = c
 where
  c = CS $ inlinePerformIO $
      withBuffer a $ \p1 ->
      withBuffer b $ \p2 ->
      B.createAndTrim (byteCount (encoding c) (charCount (encoding a) l `min` charCount (encoding b) m)) $
      zipWith_ p1 p2
  zipWith_ p1 p2 p3 = go 0 0 0
   where
    STRICT3(go)
    go i j k
       | i >= l || j >= m = return k
       | otherwise        = do
            (l1,c1) <- peekChar (encoding a) (p1 `plusPtr` i)
            (l2,c2) <- peekChar (encoding b) (p2 `plusPtr` j)
            l3      <- pokeChar (encoding c) (p3 `plusPtr` k) (f c1 c2)
            go (i+l1) (j+l2) (k+l3)
{-# INLINE zipWith' #-}

{-# RULES

"CompactString specialise zipWith"
 forall (f :: Char -> Char -> Char) p q .
    zipWith f p q = unpack (zipWith' f p q)

  #-}

unzip ls = (pack (L.map fst ls), pack (L.map snd ls))
{-# INLINE unzip #-}


-- ---------------------------------------------------------------------
-- Ordered CompactStrings

-- Implementation for lazy programmers
-- Maybe QuickSort would be appropriate here?
sort = pack . L.sort . unpack

-- | Compare two bytestrings, possibly with a different encoding.
compare' :: (Encoding a, Encoding b) => CompactString a -> CompactString b -> Ordering
compare' a@(CS (PS _ _ l1)) b@(CS (PS _ _ l2))
    = B.inlinePerformIO $
        withBuffer a $ \p1 ->
        withBuffer b $ \p2 -> comp p1 p2
  where
   comp p1 p2 = comp_ 0 0
    where
        STRICT2(comp_)
        comp_ pos1 pos2
         | pos1 >= l1  =  return $! if l1 < l2 then LT else EQ
         | pos2 >= l2  =  return $! GT
         | otherwise  =  do
                 (lc1,c1) <- peekChar (encoding a) (p1 `plusPtr` pos1)
                 (lc2,c2) <- peekChar (encoding b) (p2 `plusPtr` pos2)
                 if c1 /= c2
                   then return $! (c1 `compare` c2)
                   else comp_ (pos1 + lc1) (pos2 + lc2)

{-# SPECIALIZE doCompare :: CompactString UTF8 -> CompactString UTF8 -> Ordering #-}
{-# RULES
"CompactString: compare' UTF8"
  compare' = doCompare :: CompactString UTF8 -> CompactString UTF8 -> Ordering
"CompactString: compare' UTF32BE"
  compare' = doCompare :: CompactString UTF32BE -> CompactString UTF32BE -> Ordering
"CompactString: compare' ASCII"
  compare' = doCompare :: CompactString ASCII -> CompactString ASCII -> Ordering
"CompactString: compare' Latin1"
  compare' = doCompare :: CompactString Latin1 -> CompactString Latin1 -> Ordering
   #-}

-- -----------------------------------------------------------------------------
--
-- Encoding
--

-- for type inference
toByteStringAs :: Encoding a => a -> CompactString a -> ByteString
toByteStringAs _ = toByteString
unsafeFromByteStringAs :: Encoding a => a -> ByteString -> CompactString a
unsafeFromByteStringAs _ = unsafeFromByteString


toByteString = unCS

fromByteString   = validate   . unsafeFromByteString
fromByteString_  = validate_  . unsafeFromByteString
fromByteStringIO :: Encoding a => ByteString -> IO (CompactString a)
fromByteStringIO = validateIO . unsafeFromByteString

validate  = unsafeTry       . validateIO
validate_ = unsafePerformIO . validateIO

-- | Convert between two different encodings, fails if conversion is not possible.
recode :: (Encoding a, Encoding b, MonadPlus m) => CompactString a -> m (CompactString b)
recode  = unsafeTry       . recodeIO
-- | Convert between two different encodings, raises an error if conversion is not possible.
recode_ :: (Encoding a, Encoding b) => CompactString a -> CompactString b
recode_ = unsafePerformIO . recodeIO

-- | recode =<< validate
recodeV :: (Encoding a, Encoding b, MonadPlus m) => CompactString a -> m (CompactString b)
recodeV  = unsafeTry       . recodeVIO
-- | recode_ . validate_
recodeV_ :: (Encoding a, Encoding b) => CompactString a -> CompactString b
recodeV_ = unsafePerformIO . recodeVIO

encode  e = liftM (toByteStringAs e) . recode
encode_ e =       (toByteStringAs e) . recode_

decode  e = recodeV  . (unsafeFromByteStringAs e)
decode_ e = recodeV_ . (unsafeFromByteStringAs e)

encodeBOM  e = encode  e . cons '\xFEFF'
encodeBOM_ e = encode_ e . cons '\xFEFF'

decodeBOM  = unsafeTry       . decodeBOM_IO
decodeBOM_ = unsafePerformIO . decodeBOM_IO

{-# INLINE[1] validate   #-}
{-# INLINE[1] validate_  #-}
{-# INLINE[1] recode     #-}
{-# INLINE[1] recode_    #-}
{-# INLINE[1] recodeV    #-}
{-# INLINE[1] recodeV_   #-}
{-# INLINE[1] decodeBOM  #-}
{-# INLINE[1] decodeBOM_ #-}
{-# RULES

"CompactString: to/fromByteString"
  forall s.
  toByteString (unsafeFromByteString s) = s
"CompactString: from/toByteString"
  forall s.
  unsafeFromByteString (toByteString s) = s

"CompactString: recode  -> return"
  recode  = return
"CompactString: recode_ -> id"
  recode_ = id

"CompactString: recode/recode -> recode"
  forall s.
  recode s >>= recode = recode s
"CompactString: recode_/recode_ -> recode_"
  forall s.
  recode_ (recode_ s) = recode_ s

"CompactString: recode/validate"
  forall s.
  validate s >>= recode = recodeV s
"CompactString: recode_/validate_"
  forall s.
  recode_ (validate_ s) = recodeV_ s

"CompactString: recodeV  -> validate"
  recodeV   = validate
"CompactString: recodeV_ -> validate_"
  recodeV_  = validate_
"CompactString: recodeVIO -> validateIO"
  recodeVIO = validateIO
  
{-
-- TODO: Make these rules work
"CompactString: recode/fromByteString"
  forall bs.
  fromByteString bs >>= (recode :: (Encoding a, Encoding b, MonadPlus m) => CompactString a -> m (CompactString b))
  = decode (undefined::a) bs :: m (CompactString b)
"CompactString: recode_/fromByteString_"
  forall bs.
  recode_ (fromByteString_ bs :: Encoding a => CompactString a)       = decode_ (undefined::a) bs
-}

  #-}

decodeBOM_IO :: Encoding a => ByteString -> IO (CompactString a)
decodeBOM_IO bs
 | t2 == [0,0] && t4 == [0xFE,0xFF] = decodeIO (UTF32 BE) (B.drop 4 bs)
 | t2 == [0xFF,0xFE] && t4 == [0,0] = decodeIO (UTF32 LE) (B.drop 4 bs)
 | t2 == [0xFE,0xFF]                = decodeIO (UTF16 BE) (B.drop 2 bs)
 | t2 == [0xFF,0xFE]                = decodeIO (UTF16 LE) (B.drop 2 bs)
 | t3 == [0xEF,0xBB,0xBF]           = decodeIO UTF8       (B.drop 3 bs)
 | otherwise                        = decodeIO UTF8        bs -- no BOM
 where t2 = B.unpack (B.take 2 bs)
       t3 = B.unpack (B.take 3 bs)
       t4 = B.unpack (B.take 2 (B.drop 2 bs))
       decodeIO e = recodeVIO . (unsafeFromByteStringAs e)

-- | Validate encoding, convert to normal form
validateIO :: Encoding a => CompactString a -> IO (CompactString a)
validateIO cs@(CS (PS fp s l))
 | validEquality (encoding cs) = validateLength (encoding cs) l
                              >> withForeignPtr fp (\p -> check (p `plusPtr` s))
 | otherwise = recodeVIO_ cs -- There are multiple representations of the same string, convert to a normal form
 where
   check src = loop 0
     where
        STRICT1(loop)
        loop src_off
         | src_off == l = return cs
         | src_off >  l = failMessage "validate" "Incomplete character"
         | otherwise    = do (l',_) <- peekCharSafe (encoding cs) (l - src_off) (src `plusPtr` src_off)
                             loop (src_off+l')
{-# SPECIALIZE validateIO :: CompactString UTF8 -> IO (CompactString UTF8) #-}

-- | Convert between encodings
recodeIO :: (Encoding a, Encoding b) => CompactString a -> IO (CompactString b)
recodeIO a@(CS (PS fp s l))
 | l == 0    = return empty
 | otherwise = result
 where
   len    = byteCount (encoding_b) (charCount (encoding a) l)
   result = liftM CS $
               withForeignPtr fp $ \p -> 
               B.createAndTrim len $ doRecode (p `plusPtr` s)
   encoding_b = (undefined :: IO (CompactString a) -> Proxy a) result
   doRecode src dest = loop 0 0
     where
        STRICT2(loop)
        loop src_off dest_off
         | src_off >= l  =    return dest_off
         | otherwise     = do (l',c) <- peekChar (encoding a) (src  `plusPtr` src_off)
                              l''    <- pokeChar (encoding_b) (dest `plusPtr` dest_off) c
                              loop (src_off+l') (dest_off+l'')

-- | Validate encoding, convert to normal form
--   Can be rewritten by rules, in particular: recodeVIO -> validateIO
recodeVIO :: (Encoding a, Encoding b) => CompactString a -> IO (CompactString b)
recodeVIO = recodeVIO_
{-# INLINE[1] validateIO #-}

-- | Convert between encodings, use peekCharSafe
recodeVIO_ :: (Encoding a, Encoding b) => CompactString a -> IO (CompactString b)
recodeVIO_ a@(CS (PS fp s l))
 | l == 0    = return empty
 | otherwise = result
 where
   len    = byteCount (encoding_b) (charCount (encoding a) l)
   result = validateLength (encoding a) l
            >> (liftM CS $
                withForeignPtr fp $ \p -> 
                B.createAndTrim len $ doRecode (p `plusPtr` s))
   encoding_b = (undefined :: IO (CompactString a) -> Proxy a) result
   doRecode src dest = loop 0 0
     where
        STRICT2(loop)
        loop src_off dest_off
         | src_off >= l  =    return dest_off
         | otherwise     = do (l',c) <- peekCharSafe (encoding a) (l - src_off) (src  `plusPtr` src_off)
                              l''    <- pokeChar     (encoding_b)               (dest `plusPtr` dest_off) c
                              loop (src_off+l') (dest_off+l'')


-- ---------------------------------------------------------------------
-- Standard IO

getLine = hGetLine stdin

getContents = hGetContents stdin

putStr = hPut stdout

putStrLn = hPutStrLn stdout

interact transformer = putStr . transformer =<< getContents

-- ---------------------------------------------------------------------
-- File IO

readFile  f = C8.readFile f >>= fromByteStringIO
readFile' f = C8.readFile f >>= decodeBOM_IO

writeFile  f txt = C8.writeFile f (toByteString txt)
writeFile' f txt = C8.writeFile f (toByteString ('\xFEFF' `cons` txt))

appendFile  f txt = C8.appendFile f (toByteString txt)
appendFile' f txt = bracket (openFile f AppendMode) hClose
    (\h -> appendHandle h txt)

-- | Append a 'ByteString' to a file.
-- appendFile :: FilePath -> ByteString -> IO ()
-- TODO : Determine encoding used by the file, then append using the same encoding

appendHandle :: Encoding a => Handle -> CompactString a -> IO ()
appendHandle h cs = do
        pos <- hTell h
        hSeek h AbsoluteSeek 0
        enc <- findEncoding h
        bs <- enc cs
        hSeek h AbsoluteSeek pos
        B.hPut h bs

-- | Determine the encoding to use for a handle by examining the Byte Order Mark.
--   The handle should be positioned at the start of the file.
findEncoding :: Encoding a => Handle -> IO (CompactString a -> IO ByteString)
findEncoding h = do
        bs <- B.hGet h 4
        return (encodingOf bs)
 where encodingOf bs
        | B.null bs                        = return . toByteString . cons '\xFEFF' -- empty file, start with a BOM
        | t2 == [0,0] && t4 == [0xFE,0xFF] = encodeIO (UTF32 BE)
        | t2 == [0xFF,0xFE] && t4 == [0,0] = encodeIO (UTF32 LE)
        | t2 == [0xFE,0xFF]                = encodeIO (UTF16 BE)
        | t2 == [0xFF,0xFE]                = encodeIO (UTF16 LE)
        | otherwise                        = encodeIO UTF8 -- no BOM or UTF8 BOM
        where t2 = B.unpack (B.take 2 bs)
              t4 = B.unpack (B.take 2 (B.drop 2 bs))
              encodeIO e = liftM (toByteStringAs e) . recodeIO

-- ---------------------------------------------------------------------
-- Handle IO

hGetLine h = System.IO.hGetLine h >>= return . pack

hGetContents  h = B.hGetContents h >>= fromByteStringIO
hGetContents' h = B.hGetContents h >>= decodeBOM_IO

hGet            h i = B.hGet            h i >>= fromByteStringIO
hGetNonBlocking h i = B.hGetNonBlocking h i >>= fromByteStringIO

hPut h = B.hPut h . toByteString

hPutStr = hPut

hPutStrLn h cs@(CS bs)
    | B.length bs < 1024 = hPut h (cs `snoc` '\n')
    | otherwise          = hPut h cs >> hPut h (singleton '\n' `asTypeOf` cs) -- don't copy


-- ---------------------------------------------------------------------
-- Internal utilities

-- | Find the byte position corresponding to the given character index,
--   the index must be positive.
charIndex :: Encoding a => CompactString a -> Int -> Int
charIndex cs@(CS (PS _ _ l)) n = unsafeWithBuffer cs $ \src -> (go src n 0)
    where
        STRICT3(go)
        go _   0 p = return p
        go src i p
         | p >= l     = return l
         | otherwise  = do  l' <- peekCharLen (encoding cs) (src `plusPtr` p)
                            go src (i-1) (p+l')

-- | 'findIndexOrEnd' is a variant of findIndex, that returns the length
-- in bytes of the string if no element is found, rather than Nothing.
findIndexOrEnd :: Encoding a => (Char -> Bool) -> CompactString a -> Int
findIndexOrEnd f cs@(CS (PS _ _ l)) = unsafeWithBuffer cs $ \ptr -> go ptr 0
    where
        STRICT2(go)
        go ptr n | n >= l    = return l
                 | otherwise = do (l',c) <- peekChar (encoding cs) ptr
                                  if f c then return n
                                         else go (ptr `plusPtr` l') (n+l')
{-# INLINE findIndexOrEnd #-}

-- | 'findIndexOrBeginRev' is a variant of findIndexOrEnd, that searches
-- from the end instead of from the start
findIndexOrBeginRev :: Encoding a => (Char -> Bool) -> CompactString a -> Int
findIndexOrBeginRev f cs@(CS (PS _ _ l)) = unsafeWithBufferEnd cs $ \ptr -> go ptr l
    where
        STRICT2(go)
        go ptr n | n <= 0    = return 0
                 | otherwise = do (l',c) <- peekCharRev (encoding cs) ptr
                                  if f c then return n
                                         else go (ptr `plusPtr` (-l')) (n-l')
{-# INLINE findIndexOrBeginRev #-}
