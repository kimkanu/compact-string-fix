-- |
-- Module      : Data.CompactString.Encodings
-- License     : BSD-style
-- Maintainer  : twanvl@gmail.com
-- Stability   : experimental
-- Portability : untested
-- 
-- Different encodings of characters into bytes.
--
module Data.CompactString.Encodings (
        
        -- * Unicode encodings
        UTF8(..),
        BE(..), LE(..), Native,
        UTF16(..), UTF16BE, UTF16LE, UTF16Native,
        UTF32(..), UTF32BE, UTF32LE, UTF32Native,
        
        -- * Other encodings
        ASCII(..),
        Latin1(..),
        
        -- * Non-standard encodings
        Compact(..)
        
        ) where

import Data.Word
import Data.Bits

import Foreign.Ptr              (Ptr)

import Control.Monad            (when, unless)

import Data.CompactString.Internal

-- -----------------------------------------------------------------------------
--
-- Encoding : UTF8
--

-- | Tag representing the UTF-8 encoding.
--   Use @'CompactString' UTF8@ for UTF-8 encoded strings.
data UTF8 = UTF8

instance Encoding UTF8 where
        pokeCharFun _ c = case ord c of
         x | x < 0x80    -> (1, \p ->    poke        p   $  fromIntegral  x )
           | x < 0x800   -> (2, \p -> do poke        p   $  fromIntegral (x `shiftR`  6)           .|. 0xC0
                                         pokeByteOff p 1 $ (fromIntegral  x              .&. 0x3F) .|. 0x80)
           | x < 0x10000 -> (3, \p -> do poke        p   $  fromIntegral (x `shiftR` 12)           .|. 0xE0
                                         pokeByteOff p 1 $ (fromIntegral (x `shiftR`  6) .&. 0x3F) .|. 0x80
                                         pokeByteOff p 2 $ (fromIntegral  x              .&. 0x3F) .|. 0x80)
           | otherwise   -> (4, \p -> do poke        p   $  fromIntegral (x `shiftR` 18)           .|. 0xF0
                                         pokeByteOff p 1 $ (fromIntegral (x `shiftR` 12) .&. 0x3F) .|. 0x80
                                         pokeByteOff p 2 $ (fromIntegral (x `shiftR`  6) .&. 0x3F) .|. 0x80
                                         pokeByteOff p 3 $ (fromIntegral  x              .&. 0x3F) .|. 0x80)
        {-# INLINE pokeCharFun #-}
        
        
        peekChar _ p = do
                a <- peek p
                case charLenUTF8 a of
                 1 ->     return (1, decodeUTF8_1 a)
                 2 -> do  b <- peekByteOff p 1
                          return (2, decodeUTF8_2 a b)
                 3 -> do  b <- peekByteOff p 1
                          c <- peekByteOff p 2
                          return (3, decodeUTF8_3 a b c)
                 _ -> do  b <- peekByteOff p 1
                          c <- peekByteOff p 2
                          d <- peekByteOff p 3
                          return (4, decodeUTF8_4 a b c d)
        
        peekCharLen _ p = do
                a <- peek p
                return (charLenUTF8 a)
        {-# INLINE peekCharLen #-}
        
        peekCharRev _ p =
                peek p             >>= \a -> if not (a `testBit` 7) then return (1, decodeUTF8_1 a)     else
                peekByteOff p (-1) >>= \b -> if     (b `testBit` 6) then return (2, decodeUTF8_2 b a)   else
                peekByteOff p (-2) >>= \c -> if     (c `testBit` 6) then return (3, decodeUTF8_3 c b a) else
                peekByteOff p (-3) >>= \d ->                             return (4, decodeUTF8_4 d c b a)
        
        peekCharLenRev _ p = 
                peek p             >>= \a -> if not (a `testBit` 7) then return 1 else
                peekByteOff p (-1) >>= \b -> if     (b `testBit` 6) then return 2 else
                peekByteOff p (-2) >>= \c -> if     (c `testBit` 6) then return 3 else
                                                                         return 4
        {-# INLINE peekCharLenRev #-}
        
        
        peekCharSafe _ l p = do
                a <- peek p
                case () of
                 _ | not (a `testBit` 7) -> returnChr 1 (fromIntegral a)
                   | not (a `testBit` 6) -> failMessage "decode UTF8" "Invalid UTF8"
                   | not (a `testBit` 5) -> do  require 2
                                                b <- peekMore 1
                                                let x = ((fromIntegral a .&. 0x1F) `shiftL` 6) .|. b
                                                tooShort (x < 0x80)
                                                returnChr 2 x
                   | not (a `testBit` 4) -> do  require 3
                                                b <- peekMore 1
                                                c <- peekMore 2
                                                let x = ((fromIntegral a .&. 0x0F) `shiftL` 12) .|. (b `shiftL` 6) .|. c
                                                tooShort (x < 0x800)
                                                returnChr 3 x
                   | not (a `testBit` 3) -> do  require 4
                                                b <- peekMore 1
                                                c <- peekMore 2
                                                d <- peekMore 3
                                                let x = ((fromIntegral a .&. 0x07) `shiftL` 18) .|. (b `shiftL` 12) .|. (c `shiftL` 6) .|. d
                                                tooShort (x < 0x10000)
                                                returnChr 4 x
                   | otherwise           -> failMessage "decode UTF8" "Invalid UTF8"
         where	peekMore off = do  x <- peekByteOff p off
                                   when (x < 0x80 || x > 0xBF) (failMessage "decode UTF8" "Invalid UTF8")
                                   return $ fromIntegral (x .&. 0x3F)
                require len = when (l < len) (failMessage "decode UTF8" "Not enough input")
                tooShort b  = when b         (failMessage "decode UTF8" "Shorter encoding possible")
        
        
        copyChar enc src dst = do
                l <- peekCharLen enc src
                memcpy dst src (fromIntegral l)
                return l
        copyCharRev enc src dst = do
                l <- peekCharLenRev enc src
                memcpy dst (src `plusPtr` (1 - l)) (fromIntegral l)
                return l
        
        
        containsASCII  _ = True
        validOrdering  _ = True
        validSubstring _ = True
        byteCount _ n = 4 * n -- A char is at most 4 bytes


-- | Length of a character is determined by the first byte
charLenUTF8 :: Word8 -> Int
charLenUTF8 x
 | not (x `testBit` 7) = 1
 | not (x `testBit` 5) = 2
 | not (x `testBit` 4) = 3
 | otherwise           = 4 -- assumes the bytestring is valid UTF8
{-# INLINE charLenUTF8 #-}

-- | Decode a UTF8 encoded character
decodeUTF8_1 :: Word8 -> Char
decodeUTF8_1 a
 = unsafeChr $ (fromIntegral a)
decodeUTF8_2 :: Word8 -> Word8 -> Char
decodeUTF8_2 a b
 = unsafeChr $ (fromIntegral a `shiftL` 6)
         `xor` (fromIntegral b)
         `xor` 0x3080
decodeUTF8_3 :: Word8 -> Word8 -> Word8 -> Char
decodeUTF8_3 a b c
 = unsafeChr $ (fromIntegral a `shiftL` 12)
         `xor` (fromIntegral b `shiftL` 6)
         `xor` (fromIntegral c)
         `xor` 0xE2080
decodeUTF8_4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
decodeUTF8_4 a b c d
 = unsafeChr $ (fromIntegral a `shiftL` 18)
         `xor` (fromIntegral b `shiftL` 12)
         `xor` (fromIntegral c `shiftL` 6)
         `xor` (fromIntegral d)
         `xor` 0x3C82080
-- NOTE: the masks at the end are from the tag bits
-- for example the last one is:
--   11110001000010000010000000


-- -----------------------------------------------------------------------------
--
-- Endianness
--

-- | Tag representing big endian encoding
data BE = BE
-- | Tag representing little endian encoding
data LE = LE
-- | The platform native endianness
type Native = LE -- TODO : detect

-- | Class representing endianness
class Endian e where
        -- | Read a 16 bit word from the given location
        peekWord16 :: Proxy (t e) -> Ptr Word8 -> IO Word16
        -- | Write a 16 bit word to the given location
        pokeWord16 :: Proxy (t e) -> Ptr Word8 -> Word16 -> IO ()
        -- | Read a 32 bit word from the given location
        peekWord32 :: Proxy (t e) -> Ptr Word8 -> IO Word32
        -- | Write a 32 bit word to the given location
        pokeWord32 :: Proxy (t e) -> Ptr Word8 -> Word32 -> IO ()
        -- | Is this a big endian encoding
        isBE :: Proxy (t e) -> Bool

instance Endian BE where
        peekWord16 _ ptr = do
                a <- peek        ptr
                b <- peekByteOff ptr 1
                return ((fromIntegral a `shiftL` 8) .|. fromIntegral b)
        {-# INLINE peekWord16 #-}
        pokeWord16 _ ptr x = do
                poke        ptr   $ fromIntegral (x `shiftR` 8)
                pokeByteOff ptr 1 $ fromIntegral  x
        {-# INLINE pokeWord16 #-}
        peekWord32 _ ptr = do
                a <- peek        ptr
                b <- peekByteOff ptr 1
                c <- peekByteOff ptr 2
                d <- peekByteOff ptr 3
                return $ (fromIntegral a `shiftL` 24) .|.
                         (fromIntegral b `shiftL` 16) .|.
                         (fromIntegral c `shiftL` 8 ) .|.
                         (fromIntegral d            )
        {-# INLINE peekWord32 #-}
        pokeWord32 _ ptr x = do
                poke        ptr   $ fromIntegral (x `shiftR` 24)
                pokeByteOff ptr 1 $ fromIntegral (x `shiftR` 16)
                pokeByteOff ptr 2 $ fromIntegral (x `shiftR` 8)
                pokeByteOff ptr 3 $ fromIntegral x
        {-# INLINE pokeWord32 #-}
        isBE _ = True

instance Endian LE where
        peekWord16 _ ptr = do
                a <- peek        ptr
                b <- peekByteOff ptr 1
                return ((fromIntegral b `shiftL` 8) .|. fromIntegral a)
        {-# INLINE peekWord16 #-}
        pokeWord16 _ ptr x = do
                poke        ptr   $ fromIntegral  x
                pokeByteOff ptr 1 $ fromIntegral (x `shiftR` 8)
        {-# INLINE pokeWord16 #-}
        peekWord32 _ ptr = do
                a <- peek        ptr
                b <- peekByteOff ptr 1
                c <- peekByteOff ptr 2
                d <- peekByteOff ptr 3
                return $ (fromIntegral d `shiftL` 24) .|.
                         (fromIntegral c `shiftL` 16) .|.
                         (fromIntegral b `shiftL` 8 ) .|.
                         (fromIntegral a            )
        {-# INLINE peekWord32 #-}
        pokeWord32 _ ptr x = do
                poke        ptr   $ fromIntegral x
                pokeByteOff ptr 1 $ fromIntegral (x `shiftR` 8)
                pokeByteOff ptr 2 $ fromIntegral (x `shiftR` 16)
                pokeByteOff ptr 3 $ fromIntegral (x `shiftR` 24)
        {-# INLINE pokeWord32 #-}
        isBE _ = False

-- -----------------------------------------------------------------------------
--
-- Encoding : UTF16
--

-- | Tag representing the UTF-16 encoding
data UTF16 endianness = UTF16 endianness

-- | Tag representing the big endian UTF-16 encoding, aka. UTF-16BE.
type UTF16BE = UTF16 BE

-- | Tag representing the little endian UTF-16 encoding, aka. UTF-16LE.
type UTF16LE = UTF16 LE

-- | Tag representing the platform native UTF-16 encoding.
type UTF16Native = UTF16 Native

instance Endian e => Encoding (UTF16 e) where
        pokeCharFun enc c
         | isSurrogate x = (2, \_ -> failMessage "encoding UTF16" "Surrogate character")
         | x <= 0xFFFF   = (2, \p -> pokeWord16 enc p $ fromIntegral x)
         | otherwise     = (4, \p -> pokeWord16 enc p               h
                                >> pokeWord16 enc (p `plusPtr` 2) l)
         where  x  = ord c
                x' = x - 0x10000
                h  = fromIntegral $ (x' `shiftR` 10) .|. 0xD800
                l  = fromIntegral $ (x' .&. 0x3FF)   .|. 0xDC00
        
        pokeCharLen _ c = if ord c <= 0xFFFF then 2 else 4
        
        peekChar       enc p = do  a <- peekWord16 enc p
                                   if isSurrogate a
                                    then do
                                       b <- peekWord16 enc (p `plusPtr` 2)
                                       decodeUTF16 a b
                                    else
                                       return (2, unsafeChr $ fromIntegral a)
        peekCharRev    enc p = do  a <- peekWord16 enc (p `plusPtr` (-1))
                                   if isSurrogate a
                                    then do
                                       b <- peekWord16 enc (p `plusPtr` (-3))
                                       decodeUTF16 b a
                                    else
                                       return (2, unsafeChr $ fromIntegral a)
        peekCharLen    enc p = do  a <- peekWord16 enc p
                                   return (if isSurrogate a then 4 else 2)
        peekCharLenRev enc p = do  a <- peekWord16 enc (p `plusPtr` (-1))
                                   return (if isSurrogate a then 4 else 2)
        
        peekCharSafe enc l p = do  a <- peekWord16 enc p
                                   if isSurrogate a
                                    then do
                                       when (l < 4) (failMessage "decode UTF16" "Not enough input")
                                       b <- peekWord16 enc (p `plusPtr` 2)
                                       decodeUTF16Safe a b
                                    else
                                       return (2, unsafeChr $ fromIntegral a)
        validateLength _ len
         | len `mod` 2 /= 0 = failMessage "decode UTF16" "Not enough input"
         | otherwise        = return ()
        
        containsASCII  _ = False
        validEquality  _ = False -- there are two ways to encode a character
        validOrdering  _ = False
        validSubstring _ = False
        
        charCount _ n = n `div` 2 -- at least 2 bytes per char
        byteCount _ n = n * 4     -- at most  4 bytes per char
        newSize   _ n = n * 2

isSurrogate :: (Num a, Ord a) => a -> Bool
isSurrogate c = c >= 0xD800 && c <= 0xDFFF

-- | Decode an UTF16 surrogate pair, does no error checking
decodeUTF16 :: Word16 -> Word16 -> IO (Int,Char)
decodeUTF16 a b
 | a <= 0xDBFF = return (4, unsafeChr $ (((fromIntegral a .&. 0x3FF) `shiftL` 10) .|. (fromIntegral b .&. 0x3FF)) + 0x10000)
 | otherwise   = return (4, unsafeChr $ (((fromIntegral b .&. 0x3FF) `shiftL` 10) .|. (fromIntegral a .&. 0x3FF)) + 0x10000)

decodeUTF16Safe :: Word16 -> Word16 -> IO (Int,Char)
decodeUTF16Safe a b
 | ahi && blo = returnChr 4 $ (((fromIntegral a .&. 0x3FF) `shiftL` 10) .|. (fromIntegral b .&. 0x3FF)) + 0x10000
 | alo && bhi = returnChr 4 $ (((fromIntegral b .&. 0x3FF) `shiftL` 10) .|. (fromIntegral a .&. 0x3FF)) + 0x10000
 | otherwise  = failMessage "decode UTF16" "Unpaired surrogate"
 where  ahi = a >= 0xD800 && a <= 0xDBFF
        alo = a >= 0xDC00 && a <= 0xDFFF
        bhi = b >= 0xD800 && b <= 0xDBFF
        blo = b >= 0xDC00 && b <= 0xDFFF

-- -----------------------------------------------------------------------------
--
-- Encoding : UTF32
--

-- | Tag representing the UTF-32 encoding
data UTF32 endianness = UTF32 endianness

-- | Tag representing the big endian UTF-32 encoding, aka. UTF-32BE.
type UTF32BE = UTF32 BE

-- | Tag representing the little endian UTF-32 encoding, aka. UTF-32LE.
type UTF32LE = UTF32 LE

-- | Tag representing the platform native UTF-32 encoding.
type UTF32Native = UTF32 Native

instance Endian e => Encoding (UTF32 e) where
        pokeCharFun enc c = (4, \p -> pokeWord32 enc p (fromIntegral (ord c)))
        
        pokeCharLen _ _ = 4
        
        peekChar       enc p = do  a <- peekWord32 enc p
                                   return (4, unsafeChr $ fromIntegral a)
        peekCharRev    enc p = do  a <- peekWord32 enc (p `plusPtr` (-3))
                                   return (4, unsafeChr $ fromIntegral a)
        peekCharLen    _   _ = return 4
        peekCharLenRev _   _ = return 4
        
        peekCharSafe enc _ p = do  a <- peekWord32 enc p
                                   returnChr 4 a
        validateLength _ len
         | len `mod` 4 /= 0 = failMessage "decode UTF32" "Not enough input"
         | otherwise        = return ()
        
        containsASCII  _   = False
        validOrdering  enc = isBE enc
        validSubstring _   = True
        
        charCount _ n = n `div` 4 -- at least 4 bytes per char
        byteCount _ n = n * 4     -- at most  4 bytes per char
        newSize   _ n = n

-- -----------------------------------------------------------------------------
--
-- Encoding : ASCII
--

-- | Tag representing the ASCII encoding.
data ASCII = ASCII

instance Encoding ASCII where
        pokeCharFun _ c
         | c <= '\x7F' = (1, \p -> poke p (fromIntegral (ord c)))
         | otherwise   = (1, \_ -> failMessage "encode ASCII" "Not an ASCII character")
        
        pokeCharLen _ _ = 1
        
        peekChar       _ p = do  a <- peek p
                                 return (1, unsafeChr $ fromIntegral a)
        peekCharRev    _ p = do  a <- peek p
                                 return (1, unsafeChr $ fromIntegral a)
        peekCharLen    _ _ = return 1
        peekCharLenRev _ _ = return 1
        
        peekCharSafe _ _ p = do  a <- peek p
                                 unless (a <= 0x7F) (failMessage "decode ASCII" "Not an ASCII character")
                                 returnChr 1 (fromIntegral a)
        
        containsASCII  _ = True
        validOrdering  _ = True
        validSubstring _ = True
        
        charCount _ n = n
        byteCount _ n = n
        newSize   _ n = n

-- -----------------------------------------------------------------------------
--
-- Encoding : Latin1
--

-- | Tag representing the ISO 8859-1 encoding (latin 1).
data Latin1 = Latin1

instance Encoding Latin1 where
        pokeCharFun _ c
         | c <= '\xFF' = (1, \p -> poke p (fromIntegral (ord c)))
         | otherwise   = (1, \_ -> failMessage "encode Latin1" "Not a Latin1 character")
        
        pokeCharLen _ _ = 1
        
        peekChar       _ p = do  a <- peek p
                                 return (1, unsafeChr $ fromIntegral a)
        peekCharRev    _ p = do  a <- peek p
                                 return (1, unsafeChr $ fromIntegral a)
        peekCharLen    _ _ = return 1
        peekCharLenRev _ _ = return 1
        
        peekCharSafe _ _ p = do  a <- peek p
                                 unless (a <= 0xFF) (failMessage "decode Latin1" "Not a Latin1 character")
                                 returnChr 1 (fromIntegral a)
        
        containsASCII  _ = True
        validOrdering  _ = True
        validSubstring _ = True
        
        charCount _ n = n
        byteCount _ n = n
        newSize   _ n = n

-- -----------------------------------------------------------------------------
--
-- Encoding : Compact
--

-- | Tag representing a custom encoding optimized for memory usage.
--
-- This encoding looks like UTF-8, but is slightly more efficient.
-- It requires at most 3 byes per character, as opposed to 4 for UTF-8.
--
-- Encoding looks like:
--
-- >                   0zzzzzzz -> 0zzzzzzz
-- >          00yyyyyy yzzzzzzz -> 1xxxxxxx 1yyyyyyy
-- > 000xxxxx xxyyyyyy yzzzzzzz -> 1xxxxxxx 0yyyyyyy 1zzzzzzz
--
-- The reasoning behind the tag bits is that this allows the char to be read both forwards
-- and backwards.
data Compact = Compact

instance Encoding Compact where
        pokeCharFun _ c = case ord c of
         x | x < 0x80   -> (1, \p ->    poke        p   $ fromIntegral  x )
           | x < 0x4000 -> (2, \p -> do poke        p   $ fromIntegral (x `shiftR`  7) .|. 0x80
                                        pokeByteOff p 1 $ fromIntegral  x              .|. 0x80 )
           | otherwise  -> (3, \p -> do poke        p   $ fromIntegral (x `shiftR` 14) .|. 0x80
                                        pokeByteOff p 1 $ fromIntegral (x `shiftR`  7) .&. 0x7F
                                        pokeByteOff p 2 $ fromIntegral  x              .|. 0x80 )
        {-# INLINE pokeCharFun #-}
        
        pokeCharLen _ c = case ord c of
         x | x < 0x80   -> 1
           | x < 0x4000 -> 2
           | otherwise  -> 3
        {-# INLINE pokeCharLen #-}
        
        
        peekChar _ p = do
                aw <- peek p
                let a = fromIntegral aw
                if a `testBit` 7
                 then do
                    bw <- peekByteOff p 1
                    let b = fromIntegral bw
                    if b `testBit` 7
                     then                            -- 2 bytes
                        return (2, unsafeChr $ (a `shiftL` 7) `xor` b `xor` 0x4080 )
                     else do                         -- 3 bytes
                        cw <- peekByteOff p 2
                        let c = fromIntegral cw
                        return (3, unsafeChr $ (a `shiftL` 14) `xor` (b `shiftL` 7) `xor` c `xor` 0x200080 )
                 else return (1, unsafeChr a)        -- 1 byte
        
        peekCharLen _ p = do
                a <- peek p
                if a `testBit` 7
                 then do
                    b <- peekByteOff p 1
                    return (if b `testBit` 7 then 2 else 3)
                 else return 1
        {-# INLINE peekCharLen #-}
        
        peekCharRev _ p = do
                cw <- peek p
                let c = fromIntegral cw
                if c `testBit` 7
                 then do
                    bw <- peekByteOff p (-1)
                    let b = fromIntegral bw
                    if b `testBit` 7
                     then                            -- 2 bytes
                        return (2, unsafeChr $ (b `shiftL` 7) `xor` c `xor` 0x4080 )
                     else do                         -- 3 bytes
                        aw <- peekByteOff p (-2)
                        let a = fromIntegral aw
                        return (3, unsafeChr $ (a `shiftL` 14) `xor` (b `shiftL` 7) `xor` c `xor` 0x200080 )
                 else return (1, unsafeChr c)        -- 1 byte
        
        peekCharLenRev _ p = do
                a <- peek p
                if a `testBit` 7
                 then do
                    b <- peekByteOff p (-1)
                    return (if b `testBit` 7 then 2 else 3)
                 else return 1
        {-# INLINE peekCharLenRev #-}
        
        peekCharSafe _ l p = do
                aw <- peek p
                let a = fromIntegral aw
                if a `testBit` 7
                 then do
                    require 2
                    bw <- peekByteOff p 1
                    let b = fromIntegral bw
                    if b `testBit` 7
                     then do                         -- 2 bytes
                        let x = ((a `shiftL` 7) `xor` b `xor` 0x4080)
                        tooShort (x < 0x80)
                        returnChr 2 x
                     else do                         -- 3 bytes
                        require 3
                        cw <- peekByteOff p 2
                        let c = fromIntegral cw
                        unless (c `testBit` 7) (failMessage "decode Compact" "Invalid byte sequence")
                        let x = ((a `shiftL` 14) `xor` (b `shiftL` 7) `xor` c `xor` 0x200080)
                        tooShort (x < 0x4000)
                        returnChr 3 x
                 else returnChr 1 a        -- 1 byte
         where  require len = when (l < len) (failMessage "decode Compact" "Not enough input")
                tooShort b  = when b         (failMessage "decode UTF8" "Shorter encoding possible")
        
        copyChar _ src dst = do
                aw <- peek src
                poke dst aw
                if aw `testBit` 7
                 then do
                     bw <- peekByteOff src 1
                     pokeByteOff dst 1 bw
                     if bw `testBit` 7
                      then do -- 2 bytes
                         return 2
                      else do -- 3 bytes
                         peekByteOff src 2 >>= pokeByteOff dst 2
                         return 3
                 else do      -- 1 byte
                         return 1
        
        copyCharRev _ src dst = do
                aw <- peek src
                if aw `testBit` 7
                 then do
                     bw <- peekByteOff src (-1)
                     if bw `testBit` 7
                      then do -- 2 bytes
                         poke        dst   bw
                         pokeByteOff dst 1 aw
                         return 2
                      else do -- 3 bytes
                         cw <- peekByteOff src (-2)
                         poke        dst   cw
                         pokeByteOff dst 1 bw
                         pokeByteOff dst 2 aw
                         return 3
                 else do      -- 1 byte
                         poke dst aw
                         return 1
        
        
        containsASCII  _ = True
        validOrdering  _ = False
        validSubstring _ = False
        byteCount _ n = 3 * n -- A char is at most 3 bytes
