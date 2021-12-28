-- |
-- Module      : Data.CompactString.Internal
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : untested
-- 
-- Internal functions for the CompactString type.
--
module Data.CompactString.Internal (
        CompactString(..),
        Proxy, encoding, Encoding(..),
        PairS(..), MaybeS(..), unSP,
        AccEFL, FoldEFL, ImperativeLoop, ImperativeLoop_,
        ByteString(..), memcpy, inlinePerformIO,
        withBuffer, withBufferEnd, unsafeWithBuffer, unsafeWithBufferEnd, create,
        ord, unsafeChr, returnChr,
        plusPtr, peekByteOff, pokeByteOff, peek, poke,
        failMessage, moduleError, errorEmptyList, unsafeTry, unsafeTryIO
        ) where

import Foreign.Ptr              (Ptr)
import qualified Foreign.Ptr    (plusPtr)
import Foreign.Storable         (Storable, peek, poke)
import qualified Foreign.Storable
import Foreign.ForeignPtr       (withForeignPtr)

import Data.Word                (Word8, Word32)
import Data.Char                (ord)

import Control.Monad
import Control.Exception
import System.IO.Error		(isUserError, ioeGetErrorString)

#if defined(__GLASGOW_HASKELL__)
import GHC.Base                 (unsafeChr)
#else
import Data.Char                (chr)
#endif

import System.IO.Unsafe

import Data.ByteString.Internal (ByteString(..), memcpy, inlinePerformIO)
import qualified Data.ByteString.Internal as B

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
-- Utilities
--

data PairS a b = {-# UNPACK #-} !a :*: {-# UNPACK #-} !b
data MaybeS a = NothingS | JustS {-# UNPACK #-} !a
infixl 2 :*:

unSP :: PairS a b -> (a,b)
unSP (a :*: b) = (a,b)

-- -----------------------------------------------------------------------------
--
-- Type
--

-- | A String using a compact, strict representation.
--   A @CompactString a@ is encoded using encoding @a@, for example @CompactString 'UTF8'@.
newtype CompactString a = CS { unCS :: ByteString }

-- Invariants used by CompactString:
--  - All characters in the bytestring are complete and valid (see below)
--  - Characters use the shortest possible encoding

-- -----------------------------------------------------------------------------
--
-- Encoding
--

-- From Data.Proxy proposal
data Proxy a

-- | A way to encode characters into bytes
class Encoding a where
        -- | Given a character returns the length of that character,
        --   and a function to write it to a memory buffer.
        --   if the encoding can not represent the character, the io function should @fail@.
        pokeCharFun :: Proxy a -> Char -> (Int, Ptr Word8 -> IO ())
        -- | The size needed to store a character
        pokeCharLen :: Proxy a -> Char -> Int
        pokeCharLen a = fst . pokeCharFun a
        -- | Write a character and return the size used
        pokeChar :: Proxy a -> Ptr Word8 -> Char -> IO Int
        pokeChar enc p c = case pokeCharFun enc c of (l,f) -> f p >> return l
        {-# INLINE pokeChar #-}
        -- | Write a character given a pointer to its last byte, and return the size used
        pokeCharRev :: Proxy a -> Ptr Word8 -> Char -> IO Int
        pokeCharRev enc p c = case pokeCharFun enc c of (l,f) -> f (p `plusPtr` (1-l)) >> return l
        {-# INLINE pokeCharRev #-}
        
        -- | Read a character from a memory buffer, return it and its length.
        --   The buffer is guaranteed to contain a valid character.
        peekChar :: Proxy a -> Ptr Word8 -> IO (Int, Char)
        -- | Return the length of the character in a memory buffer
        peekCharLen :: Proxy a -> Ptr Word8 -> IO Int
        -- | Read a character from a memory buffer, return it and its length,
        --   given a pointer to the /last/ byte.
        --   The buffer is guaranteed to contain a valid character.
        peekCharRev :: Proxy a -> Ptr Word8 -> IO (Int, Char)
        -- | Return the length of the character in a memory buffer,
        --   given a pointer to the /last/ byte.
        peekCharLenRev :: Proxy a -> Ptr Word8 -> IO Int
        
        -- | Read a character from a memory buffer, return it and its length.
        --   The buffer is not guaranteed to contain a valid character, so that should
        --   be verified. There is also no guarantee that the length of the buffer (also given)
        --   is sufficient to contain a whole character.
        peekCharSafe :: Proxy a -> Int -> Ptr Word8 -> IO (Int, Char)
        -- | Validate the length, should be used before peekCharSafe is called.
        --   Can be used to remove the number of checks used by peekCharSafe.
        validateLength :: Proxy a -> Int -> IO ()
        validateLength _ _ = return ()
        
        -- | Copy a character from one buffer to another, return the length of the character
        copyChar :: Proxy a -> Ptr Word8 -> Ptr Word8 -> IO Int
        copyChar enc src dst = do
                (l,c) <- peekChar enc src
                pokeChar enc dst c
                return l
        -- | Copy a character from one buffer to another, where the source pointer
        --   points to the last byte of the character.
        --   return the length of the character.
        copyCharRev :: Proxy a -> Ptr Word8 -> Ptr Word8 -> IO Int
        copyCharRev enc src dst = do
                (l,c) <- peekCharRev enc src
                pokeChar enc dst c
                return l
        
        -- | Is ASCII a valid subset of the encoding?
        containsASCII :: Proxy a -> Bool
        -- | Is @(a == b) == (toBS a == toBS b)@?
        validEquality  :: Proxy a -> Bool
        validEquality _ = True
        -- | Is @(a `compare` b) == (toBS a `compare` toBS b)@?
        validOrdering  :: Proxy a -> Bool
        -- | Is @(a `isSubstringOf` b) == (toBS a `isSubstringOf` toBS b)@?
        validSubstring :: Proxy a -> Bool
        
        -- | What is the maximum number of character a string with the given number of bytes contains?
        charCount :: Proxy a -> Int -> Int
        charCount _ n = n
        -- | What is the maximum number of bytes a string with the given number of characters contains?
        byteCount :: Proxy a -> Int -> Int
        -- | What is the maximum size in bytes after transforming (using map) a string?
        newSize :: Proxy a -> Int -> Int
        newSize e = byteCount e . charCount e
        
        -----------------------------------------------------------------------------
        --
        -- Fusion
        --
        
        doUpLoop :: Proxy a -> AccEFL acc -> acc -> ImperativeLoop acc
        doUpLoop enc f acc0 src dest len = loop 0 0 acc0
          where STRICT3(loop)
                loop src_off dest_off acc
                    | src_off >= len = return (acc :*: 0 :*: dest_off)
                    | otherwise      = do
                        (l,x) <- peekChar enc (src `plusPtr` src_off)
                        case f acc x of
                          (acc' :*: NothingS) ->    loop (src_off+l)  dest_off     acc'
                          (acc' :*: JustS x') -> do l' <- pokeChar enc (dest `plusPtr` dest_off) x'
                                                    loop (src_off+l) (dest_off+l') acc'
        
        doDownLoop :: Proxy a -> AccEFL acc -> acc -> ImperativeLoop acc
        doDownLoop enc f acc0 src dest len = loop (len-1) (newSize enc len-1) acc0
          where STRICT3(loop)
                loop src_off dest_off acc
                    | src_off < 0    = return (acc :*: dest_off + 1 :*: newSize enc len - (dest_off+1))
                    | otherwise      = do
                        (l,x) <- peekCharRev enc (src `plusPtr` src_off)
                        case f acc x of
                          (acc' :*: NothingS) ->    loop (src_off-l)  dest_off     acc'
                          (acc' :*: JustS x') -> do l' <- pokeCharRev enc (dest `plusPtr` dest_off) x'
                                                    loop (src_off-l) (dest_off-l') acc'
        
        doUpLoopFold :: Proxy a -> FoldEFL acc -> acc -> ImperativeLoop_ acc
        doUpLoopFold enc f acc0 src len = loop 0 acc0
          where STRICT2(loop)
                loop src_off acc
                    | src_off >= len = return acc
                    | otherwise      = do
                        (l,x) <- peekChar enc (src `plusPtr` src_off)
                        loop (src_off + l) (f acc x)
        
        doDownLoopFold :: Proxy a -> FoldEFL acc -> acc -> ImperativeLoop_ acc
        doDownLoopFold enc f acc0 src len = loop (len-1) acc0
          where STRICT2(loop)
                loop src_off acc
                    | src_off < 0 = return acc
                    | otherwise   = do
                        (l,x) <- peekCharRev enc (src `plusPtr` src_off)
                        loop (src_off - l) (f acc x)

-- -----------------------------------------------------------------------------
--
-- Fusion types
--

-- |Type of loop functions
type AccEFL acc  = acc -> Char -> (PairS acc (MaybeS Char))
type FoldEFL acc = acc -> Char ->        acc

-- | An imperative loop transforming a string, using an accumulating parameter.
--   See Data.ByteString.Fusion
type ImperativeLoop acc =
    Ptr Word8          -- pointer to the start of the source byte array
 -> Ptr Word8          -- pointer to ther start of the destination byte array
 -> Int                -- length of the source byte array
 -> IO (PairS (PairS acc Int) Int) -- result and offset, length of dest that was filled

-- | ImperativeLoop with no output
type ImperativeLoop_ acc =
    Ptr Word8          -- pointer to the start of the source byte array
 -> Int                -- length of the source byte array
 -> IO acc             -- result

-- -----------------------------------------------------------------------------
--
-- Utilities : buffer stuff
--

-- | Perform a function given a pointer to the buffer of a CompactString
withBuffer :: CompactString a -> (Ptr Word8 -> IO b) -> IO b
withBuffer (CS (PS x s _)) f = withForeignPtr x $ \p -> f (p `plusPtr` s)
{-# INLINE withBuffer #-}

-- | Perform a function given a pointer to the last byte in the buffer of a CompactString
withBufferEnd :: CompactString a -> (Ptr Word8 -> IO b) -> IO b
withBufferEnd (CS (PS x s l)) f = withForeignPtr x $ \p -> f (p `plusPtr` (s + l - 1))
{-# INLINE withBufferEnd #-}

-- | Perform a function given a pointer to the buffer of a CompactString
unsafeWithBuffer :: CompactString a -> (Ptr Word8 -> IO b) -> b
unsafeWithBuffer cs f = inlinePerformIO $ withBuffer cs f
{-# INLINE unsafeWithBuffer #-}

-- | Perform a function given a pointer to the last byte in the buffer of a CompactString
unsafeWithBufferEnd :: CompactString a -> (Ptr Word8 -> IO b) -> b
unsafeWithBufferEnd cs f = inlinePerformIO $ withBufferEnd cs f
{-# INLINE unsafeWithBufferEnd #-}

create :: Int -> (Ptr Word8 -> IO ()) -> IO (CompactString a)
create len f = liftM CS $ B.create len f
{-# INLINE create #-}

-- -----------------------------------------------------------------------------
--
-- Utilities : characters
--

#if !defined(__GLASGOW_HASKELL__)
unsafeChr = chr
#endif

-- | Safe variant of chr, combined with return; does more checks.
--   At least GHC does not check for surrogate pairs
returnChr :: Int -> Word32 -> IO (Int, Char)
returnChr a c
 | c >= 0xD800 && c <= 0xDFFF = failMessage "decode" "Surrogate character"
 | c > 0x10FFFF               = failMessage "decode" "Character out of range"
 | otherwise                  = return (a, unsafeChr $ fromIntegral c)

-- -----------------------------------------------------------------------------
--
-- Utilities : Type safety/inference
--

-- | plusPtr that preserves the pointer type
plusPtr :: Ptr a -> Int -> Ptr a
plusPtr = Foreign.Ptr.plusPtr

peekByteOff :: Storable a => Ptr a -> Int -> IO a
peekByteOff = Foreign.Storable.peekByteOff

pokeByteOff :: Storable a => Ptr a -> Int -> a -> IO ()
pokeByteOff = Foreign.Storable.pokeByteOff

encoding :: CompactString a -> Proxy a
encoding = undefined

-- -----------------------------------------------------------------------------
--
-- Utilities : Error handling
--
-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
--

-- | Fail with an error message including the module name and function
failMessage :: String -> String -> IO a
failMessage fun msg = fail ("Data.CompactString." ++ fun ++ ':':' ':msg)
{-# NOINLINE failMessage #-}

-- | Raise an errorr, with the message including the module name and function
moduleError :: String -> String -> a
moduleError fun msg = error ("Data.CompactString." ++ fun ++ ':':' ':msg)
{-# NOINLINE moduleError #-}

errorEmptyList :: String -> a
errorEmptyList fun = moduleError fun "empty CompactString"
{-# NOINLINE errorEmptyList #-}

-- | Catch exceptions from fail in the IO monad, and wrap them in another monad
unsafeTry :: MonadPlus m => IO a -> m a
unsafeTry ioa = unsafePerformIO (unsafeTryIO ioa)

-- | Catch exceptions from fail in the IO monad, and wrap them in another monad
unsafeTryIO :: MonadPlus m => IO a -> IO (m a)
unsafeTryIO ioa = handleJust userErrors (return . fail) (fmap return ioa)

userErrors :: IOError -> Maybe String
userErrors e = if isUserError e then Just (ioeGetErrorString e) else Nothing
