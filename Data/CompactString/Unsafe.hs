-- |
-- Module      : Data.CompactString.Unsafe
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : untested
-- 
-- Unsafe functions on 'CompactString's.
-- All these functions can lead to crashes if not used properly.
--
module Data.CompactString.Unsafe (
        
        -- * Basic interface
        unsafeHead,                   -- :: Encoding a => CompactString a -> Char
        unsafeLast,                   -- :: Encoding a => CompactString a -> Char
        unsafeTail,                   -- :: Encoding a => CompactString a -> CompactString
        unsafeInit,                   -- :: Encoding a => CompactString a -> CompactString
        
        -- * Conversion from 'ByteString'
        unsafeFromByteString          -- :: ByteString -> CompactString a
        
        ) where

import Data.CompactString.Internal

-- -----------------------------------------------------------------------------
--
-- Basic interface
--

-- | A variety of 'head' for non-empty CompactString. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the CompactString is non-empty.
unsafeHead :: Encoding a => CompactString a -> Char
unsafeHead cs = snd $ unsafeWithBuffer cs $ peekChar (encoding cs)
{-# INLINE unsafeHead #-}

-- | A variety of 'tail' for non-empty CompactString. 'unsafeTail' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the CompactString is non-empty.
unsafeTail :: Encoding a => CompactString a -> CompactString a
unsafeTail cs@(CS (PS x s l))
     = let headlen = unsafeWithBuffer cs $ peekCharLen (encoding cs)
       in CS (PS x (s+headlen) (l-headlen))
{-# INLINE unsafeTail #-}

-- | A variety of 'last' for non-empty CompactString. 'unsafeLast' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the CompactString is non-empty.
unsafeLast :: Encoding a => CompactString a -> Char
unsafeLast cs = snd $ unsafeWithBufferEnd cs $ peekCharRev (encoding cs)
{-# INLINE unsafeLast #-}

-- | A variety of 'init' for non-empty CompactString. 'unsafeInit' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the CompactString is non-empty.
unsafeInit :: Encoding a => CompactString a -> CompactString a
unsafeInit cs@(CS (PS x s l))
     = let lastlen = unsafeWithBufferEnd cs $ peekCharLenRev (encoding cs)
       in CS (PS x s (l-lastlen))
{-# INLINE unsafeInit #-}

-- -----------------------------------------------------------------------------
--
-- Conversion
--

-- | Convert a ByteString to a CompactString,
--   does not check whether the ByteString represents a valid string in the encoding a.
unsafeFromByteString :: ByteString -> CompactString a
unsafeFromByteString = CS
