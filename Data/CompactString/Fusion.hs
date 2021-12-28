-- |
-- Module      : Data.CompactString.Fusion
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : untested
-- 
-- Fusable loop functions, mirrors "Data.ByteString.Fusion".
--
module Data.CompactString.Fusion
	(	loopAcc, loopArr, NoAcc(..)
	,	foldEFL, mapEFL, filterEFL, scanEFL, mapAccumEFL, mapIndexEFL
	,	loopUp, loopUpC, loopDown, loopUpFold, loopDownFold
	) where

import Data.CompactString.Internal
import qualified Data.ByteString.Internal as B

-- -----------------------------------------------------------------------------
--
-- Same as older bytestrings
--

-- |Data type for accumulators which can be ignored. The rewrite rules rely on
-- the fact that no bottoms of this type are ever constructed; hence, we can
-- assume @(_ :: NoAcc) `seq` x = x@.
--
data NoAcc = NoAcc

-- | Projection functions that are fusion friendly (as in, we determine when
-- they are inlined)
loopArr :: (PairS acc arr) -> arr
loopArr (_ :*: arr) = arr
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopArr #-}
#endif

loopAcc :: (PairS acc arr) -> acc
loopAcc (acc :*: _) = acc
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopAcc #-}
#endif

-- -----------------------------------------------------------------------------
--
-- EFLs
--

mapEFL :: (Char -> Char) -> AccEFL NoAcc
mapEFL f = \_ e -> (NoAcc :*: (JustS $ f e))

foldEFL :: (acc -> Char -> acc) -> AccEFL acc
foldEFL f = \a e -> (f a e :*: NothingS)

filterEFL :: (Char -> Bool) -> AccEFL NoAcc
filterEFL p = \_ e -> if p e then (NoAcc :*: JustS e)
                             else (NoAcc :*: NothingS)

scanEFL :: (Char -> Char -> Char) -> AccEFL Char
scanEFL f = \a e -> (f a e :*: JustS a)

-- | Element function implementing a map and fold
mapAccumEFL :: (acc -> Char -> (acc, Char)) -> AccEFL acc
mapAccumEFL f = \a e -> case f a e of (a', e') -> (a' :*: JustS e')

-- | Element function implementing a map with index
mapIndexEFL :: (Int -> Char -> Char) -> AccEFL Int
mapIndexEFL f = \i e -> let i' = i+1 in i' `seq` (i' :*: JustS (f i e))

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL #-}
{-# INLINE [1] mapEFL #-}
{-# INLINE [1] filterEFL #-}
{-# INLINE [1] scanEFL #-}
{-# INLINE [1] mapAccumEFL #-}
{-# INLINE [1] mapIndexEFL #-}
#endif

-- -----------------------------------------------------------------------------
--
-- Looping
--

loopUp :: Encoding a => AccEFL acc -> acc -> CompactString a -> PairS acc (CompactString a)
loopUp f a arr = loopWrapper (newSize (encoding arr)) (doUpLoop (encoding arr) f a) arr

-- | like loopUp, but the size of the buffer can only become smaller
loopUpC :: Encoding a => AccEFL acc -> acc -> CompactString a -> PairS acc (CompactString a)
loopUpC f a arr = loopWrapper id (doUpLoop (encoding arr) f a) arr

loopDown :: Encoding a => AccEFL acc -> acc -> CompactString a -> PairS acc (CompactString a)
loopDown f a arr = loopWrapper (newSize (encoding arr)) (doDownLoop (encoding arr) f a) arr

loopUpFold :: Encoding a => FoldEFL acc -> acc -> CompactString a -> acc
loopUpFold f a arr = loopWrapperFold (doUpLoopFold (encoding arr) f a) arr

loopDownFold :: Encoding a => FoldEFL acc -> acc -> CompactString a -> acc
loopDownFold f a arr = loopWrapperFold (doDownLoopFold (encoding arr) f a) arr

-------------------------------------------------------------------------------
-- Wrappers

-- first argument is the factor by which the string can grow
-- maxCharSize for map (all 1 byte chars become maxCharSize byte chars)
loopWrapper :: (Int -> Int) -> ImperativeLoop acc -> CompactString a -> PairS acc (CompactString a)
loopWrapper factor body cs@(CS (B.PS _ _ srcLen)) = unsafeWithBuffer cs $ \src -> do
    (ps, acc) <- B.createAndTrim' (factor srcLen) $ \dest -> do
        (acc :*: destOffset :*: destLen) <- body src dest srcLen
        return (destOffset, destLen, acc)
    return (acc :*: CS ps)

loopWrapperFold :: ImperativeLoop_ acc -> CompactString a -> acc
loopWrapperFold body cs@(CS (B.PS _ _ srcLen)) = unsafeWithBuffer cs $ \src -> body src srcLen
