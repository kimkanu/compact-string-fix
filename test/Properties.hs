{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.QuickCheck
import Data.Char (ord)
import Control.Monad

import Text.Printf
import Debug.Trace
import System.Environment

import qualified Data.CompactString.UTF8 as C
import qualified Data.List          as L
import Data.CompactString.Encodings

import qualified Data.ByteString as B

import QuickCheckUtils hiding (C)

type C = C.CompactString

------------------------------------------------------------------------
-- Functions not in Data.List

l_headView []     = Nothing
l_headView (x:xs) = Just (x, xs)
l_lastView xs = if L.null xs then Nothing else Just (L.init xs, L.last xs)

l_mapIndexed f = L.zipWith f [0..]

l_spanEnd p = (\(x,y) -> (reverse y, reverse x)) . span p . reverse
l_breakEnd p = l_spanEnd (not . p)

l_findSubstring  pat str = case l_findSubstrings pat str of
                                []     -> Nothing
                                (x:xs) -> Just x
l_findSubstrings pat str = L.map fst $ L.filter snd $ zip [0..] $ L.map (pat `L.isPrefixOf`) (L.tails str)

l_split     c    = l_splitWith (==c)
l_splitWith p [] = []
l_splitWith p xs = sw xs
 where sw xs = case L.break p xs of
            (_ ,[]    ) -> [xs]
            (ys,(_:zs)) -> ys : sw zs

l_count c = L.length . L.filter (==c)

------------------------------------------------------------------------
-- Properties : Internal consistency

prop_validate cs = C.validate cs == Just cs

------------------------------------------------------------------------
-- Properties : Functions not in the prelude

prop_pack   xs = C.unpack (C.pack xs) == xs
prop_unpack cs = C.pack (C.unpack cs) == cs

prop_elemIndexEnd c xs =
        C.elemIndexEnd c xs ==
        (-) (C.length xs - 1) `fmap` C.elemIndex c (C.reverse xs)

prop_findIndexEnd c xs =
        C.findIndexEnd c xs ==
        (-) (C.length xs - 1) `fmap` C.findIndex c (C.reverse xs)


------------------------------------------------------------------------
-- Properties : Comparing with lists

prop_eq         = eq2
        ((==)           :: C -> C -> Bool)
        ((==)           :: S -> S -> Bool)
prop_compare    = eq2
        (compare        :: C -> C -> Ordering)
        (compare        :: S -> S -> Ordering)
prop_show       = eq1
        (show           :: C -> String)
        (show           :: S -> String)

prop_empty      = C.empty       `eq0`           ""
prop_singleton  = C.singleton   `eq1`           (return         :: Char -> S)

prop_cons       = C.cons        `eq2`           ((:)            :: Char -> S -> S)
prop_snoc       = C.snoc        `eq2`           ((\s c -> s ++ [c]) :: S -> Char -> S)
prop_append     = C.append      `eq2`           ((++)           :: S -> S -> S)
prop_head       = C.head        `eqnotnull1`    (L.head         :: S -> Char)
prop_tail       = C.tail        `eqnotnull1`    (L.tail         :: S -> S)
prop_last       = C.last        `eqnotnull1`    (L.last         :: S -> Char)
prop_init       = C.init        `eqnotnull1`    (L.init         :: S -> S)
prop_headView   = C.headView    `eq1`           (l_headView     :: S -> Maybe (Char, S))
prop_lastView   = C.lastView    `eq1`           (l_lastView     :: S -> Maybe (S, Char))
prop_null       = C.null        `eq1`           (L.null         :: S -> Bool)
prop_length     = C.length      `eq1`           (L.length       :: S -> Int)

prop_map        = C.map         `eq2`           (L.map          :: (Char -> Char) -> (S -> S))
prop_reverse    = C.reverse     `eq1`           (L.reverse      :: S -> S)
prop_intersperse= C.intersperse `eq2`           (L.intersperse  :: Char -> S -> S)
prop_transpose  = C.transpose   `eq1`           (L.transpose    :: [S] -> [S])

prop_foldl      = eq3
        (C.foldl        :: (Int -> Char -> Int) -> Int -> C -> Int)
        (L.foldl        :: (Int -> Char -> Int) -> Int -> S -> Int)
prop_foldr      = eq3
        (C.foldr        :: (Char -> Int -> Int) -> Int -> C -> Int)
        (L.foldr        :: (Char -> Int -> Int) -> Int -> S -> Int)
prop_foldl'     = eq3
        (C.foldl'       :: (Int -> Char -> Int) -> Int -> C -> Int)
        (L.foldl'       :: (Int -> Char -> Int) -> Int -> S -> Int)
prop_foldl1     = C.foldl1      `eqnotnull2`    (L.foldl1       :: (Char -> Char -> Char) -> S -> Char)
prop_foldr1     = C.foldr1      `eqnotnull2`    (L.foldr1       :: (Char -> Char -> Char) -> S -> Char)
prop_foldl1'    = C.foldl1'     `eqnotnull2`    (L.foldl1'      :: (Char -> Char -> Char) -> S -> Char)

prop_concat     = C.concat      `eq1`           (L.concat       :: [S] -> S)
prop_concatMap  = C.concatMap   `eq2b`          (L.concatMap    :: (Char -> S) -> S -> S)
prop_any        = C.any         `eq2`           (L.any          :: (Char -> Bool) -> S -> Bool)
prop_all        = C.all         `eq2`           (L.all          :: (Char -> Bool) -> S -> Bool)
prop_maximum    = C.maximum     `eqnotnull1`    (L.maximum      :: S -> Char)
prop_minimum    = C.minimum     `eqnotnull1`    (L.minimum      :: S -> Char)

prop_concatMap2 = c_concatMap2  `eq2b`          (L.concatMap    :: (Char -> S) -> S -> S)
    where c_concatMap2 f = C.concat . L.map (C.validate_ . f) . C.unpack
prop_concatMap3 = c_concatMap3  `eq2`          (L.map    :: (Char -> S) -> S -> [S])
    where
      c_concatMap3 :: (Char -> C) -> C -> [C]
      c_concatMap3 f = L.map f . C.unpack

prop_concatMapFoldr = eq2
        (\f -> C.foldr ((:) . f) [] :: C -> [C])
        (\f -> L.foldr ((:) . f) [] :: S -> [S])

prop_scanl      = C.scanl       `eq3`           (L.scanl        :: (Char -> Char -> Char) -> Char -> S -> S)
prop_scanr      = C.scanr       `eq3`           (L.scanr        :: (Char -> Char -> Char) -> Char -> S -> S)
prop_scanl1     = C.scanl1      `eqnotnull2`    (L.scanl1       :: (Char -> Char -> Char) -> S -> S)
prop_scanr1     = C.scanr1      `eqnotnull2`    (L.scanr1       :: (Char -> Char -> Char) -> S -> S)

prop_mapAccumL  = eq3
        (C.mapAccumL    :: (Int -> Char -> (Int, Char)) -> Int -> C -> (Int, C))
        (L.mapAccumL    :: (Int -> Char -> (Int, Char)) -> Int -> S -> (Int, S))
prop_mapAccumR  = eq3
        (C.mapAccumR    :: (Int -> Char -> (Int, Char)) -> Int -> C -> (Int, C))
        (L.mapAccumR    :: (Int -> Char -> (Int, Char)) -> Int -> S -> (Int, S))
prop_mapIndexed = eq2
        (C.mapIndexed   :: (Int -> Char -> Char) -> C -> C)
        (l_mapIndexed   :: (Int -> Char -> Char) -> S -> S)

prop_replicate          = C.replicate      `eq2`        (L.replicate    :: Int -> Char -> S)
-- note: we have to take a finite length, otherwise we could loop
--       since C.unfoldr is strict this is not possible
--prop_unfoldr  = eq3
--      (\n -> C.take n (C.unfoldr      :: (Int -> Maybe (Char, Int)) -> Int -> C))
--      (\n -> C.take n (L.unfoldr      :: (Int -> Maybe (Char, Int)) -> Int -> S))
prop_unfoldrN   = eq3
        (\n f a -> fst      $ (C.unfoldrN :: Int -> (Int -> Maybe (Char, Int)) -> Int -> (C, Maybe Int)) n f a)
        (\n f a -> L.take n $ (L.unfoldr  ::        (Int -> Maybe (Char, Int)) -> Int ->  S            )   f a)

prop_take               = C.take           `eq2`        (L.take         :: Int -> S -> S)
prop_drop               = C.drop           `eq2`        (L.drop         :: Int -> S -> S)
prop_splitAt            = C.splitAt        `eq2`        (L.splitAt      :: Int -> S -> (S,S))
prop_takeWhile          = C.takeWhile      `eq2`        (L.takeWhile    :: (Char -> Bool) -> S -> S)
prop_dropWhile          = C.dropWhile      `eq2`        (L.dropWhile    :: (Char -> Bool) -> S -> S)
prop_break              = C.break          `eq2`        (L.break        :: (Char -> Bool) -> S -> (S,S))
prop_span               = C.span           `eq2`        (L.span         :: (Char -> Bool) -> S -> (S,S))
prop_breakEnd           = C.breakEnd       `eq2`        (l_breakEnd     :: (Char -> Bool) -> S -> (S,S))
prop_spanEnd            = C.spanEnd        `eq2`        (l_spanEnd      :: (Char -> Bool) -> S -> (S,S))
prop_group              = C.group          `eq1`        (L.group        :: S -> [S])
prop_groupBy            = C.groupBy        `eq2`        (L.groupBy      :: (Char -> Char -> Bool) -> S -> [S])
prop_inits              = C.inits          `eq1`        (L.inits        :: S -> [S])
prop_tails              = C.tails          `eq1`        (L.tails        :: S -> [S])

prop_split              = C.split          `eq2`        (l_split        :: Char          -> S -> [S])
prop_splitWith          = C.splitWith      `eq2`        (l_splitWith    :: (Char -> Bool) ->S -> [S])

prop_lines              = C.lines          `eq1`        (L.lines        :: S -> [S])
prop_unlines            = C.unlines        `eq1`        (L.unlines      :: [S] -> S)
prop_words              = C.words          `eq1`        (L.words        :: S -> [S])
prop_unwords            = C.unwords        `eq1`        (L.unwords      :: [S] -> S)

prop_isPrefixOf         = C.isPrefixOf     `eq2`        (L.isPrefixOf   :: S -> S -> Bool)
prop_isSuffixOf         = C.isSuffixOf     `eq2`        (L.isSuffixOf   :: S -> S -> Bool)
prop_isSubstringOf      = C.isInfixOf      `eq2`        (L.isInfixOf    :: S -> S -> Bool)

prop_findSubstring      = C.findSubstring  `eq2`        (l_findSubstring:: S -> S -> Maybe Int)
prop_findSubstrings     = C.findSubstrings `eq2`        (l_findSubstrings::S -> S -> [Int])

prop_elem               = C.elem           `eq2`        (L.elem         :: Char -> S -> Bool)
prop_notElem            = C.notElem        `eq2`        (L.notElem      :: Char -> S -> Bool)

prop_find               = C.find           `eq2`        (L.find         :: (Char -> Bool) -> S -> Maybe Char)
prop_filter             = C.filter         `eq2`        (L.filter       :: (Char -> Bool) -> S -> S)
prop_partition          = C.partition      `eq2`        (L.partition    :: (Char -> Bool) -> S -> (S,S))

prop_index              = C.index          `eqexcept2`  ((L.!!)         :: S -> Int -> Char)
prop_elemIndex          = C.elemIndex      `eq2`        (L.elemIndex    :: Char -> S -> Maybe Int)
prop_elemIndices        = C.elemIndices    `eq2`        (L.elemIndices  :: Char -> S -> [Int])
prop_findIndex          = C.findIndex      `eq2`        (L.findIndex    :: (Char -> Bool) -> S -> Maybe Int)
prop_findIndices        = C.findIndices    `eq2`        (L.findIndices  :: (Char -> Bool) -> S -> [Int])
prop_count              = C.count          `eq2`        (l_count        :: Char -> S -> Int)

prop_zip                = C.zip            `eq2`        (L.zip          :: S -> S -> [(Char,Char)])
prop_zipWith            = eq3
        (C.zipWith      :: (Char -> Char -> Int) -> C -> C -> [Int])
        (L.zipWith      :: (Char -> Char -> Int) -> S -> S -> [Int])
prop_zipWith'           = C.zipWith'       `eq3`        (L.zipWith      :: (Char -> Char -> Char) -> S -> S -> S)
prop_unzip              = C.unzip          `eq1`        (L.unzip        :: [(Char,Char)] -> (S,S))

prop_sort               = C.sort           `eq1`        (L.sort         :: S -> S)

------------------------------------------------------------------------
-- Properties : Encding

-- "C.fromByteString . C.toByteString = id"
prop_fromToByteString cs = C.fromByteString (C.toByteString cs)  ==  Just cs

-- "decode . encode = id"
prop_de enc = \x -> case C.encode enc x of
        Nothing -> False ==> False
        Just  y -> property $ C.decode enc y == Just x

-- "encode . decode = id"
prop_ed enc = \x -> case C.decode enc x of
        Nothing -> False ==> False
        Just  y -> property $ C.encode enc y == Just x

-- "decodeBOM . encodeBOM = id"
prop_decode_bom enc = \x -> C.decodeBOM (C.encodeBOM_ enc x) == Just x

prop_decode_encode_UTF8    = prop_de UTF8
prop_decode_encode_UTF16BE = prop_de (UTF16 BE)
prop_decode_encode_UTF16LE = prop_de (UTF16 LE)
prop_decode_encode_UTF32BE = prop_de (UTF32 BE)
prop_decode_encode_UTF32LE = prop_de (UTF32 LE)
prop_decode_encode_ASCII   = prop_de ASCII
prop_decode_encode_Latin1  = prop_de Latin1
prop_decode_encode_Compact = prop_de Compact

prop_encode_decode_UTF8    = prop_ed UTF8
-- This does not hold for UTF16, because two different bytestrings can represent the
-- same Unicode string: The order of a surrogate pair can be swapped
--prop_encode_decode_UTF16BE = prop_ed (decode (UTF16 BE)) (encode (UTF16 BE))
--prop_encode_decode_UTF16LE = prop_ed (decode (UTF16 LE)) (encode (UTF16 LE))
prop_encode_decode_UTF32BE = prop_ed (UTF32 BE)
prop_encode_decode_UTF32LE = prop_ed (UTF32 LE)
prop_encode_decode_ASCII   = prop_ed ASCII
prop_encode_decode_Latin1  = prop_ed Latin1
prop_encode_decode_Compact = prop_ed Compact

prop_decode_no_bom      = \x -> C.take 1 x /= C.pack "\xFEFF"
                        ==> C.decodeBOM (C.encode_ UTF8 x) == Just x
prop_decode_bom_UTF8    = prop_decode_bom UTF8
prop_decode_bom_UTF16BE = prop_decode_bom (UTF16 BE)
prop_decode_bom_UTF16LE = prop_decode_bom (UTF16 LE)
prop_decode_bom_UTF32BE = prop_decode_bom (UTF32 BE)
prop_decode_bom_UTF32LE = prop_decode_bom (UTF32 LE)

------------------------------------------------------------------------
-- The tests

tests = list_tests
     ++ other_tests
     ++ encoding_tests

list_tests :: [(String, Int -> IO ())]
list_tests =
    [("validate",       mytest prop_validate)
    
    ,("eq",             mytest prop_eq)
    ,("compare",        mytest prop_compare)
    ,("show",           mytest prop_show)
    
    ,("empty",          mytest prop_empty)
    ,("singleton",      mytest prop_singleton)
    
    ,("cons",           mytest prop_cons)
    ,("snoc",           mytest prop_snoc)
    ,("append",         mytest prop_append)
    ,("head",           mytest prop_head)
    ,("tail",           mytest prop_tail)
    ,("init",           mytest prop_init)
    ,("null",           mytest prop_null)
    ,("length",         mytest prop_length)
    
    ,("map",            mytest prop_map)
    ,("reverse",        mytest prop_reverse)
    ,("intersperse",    mytest prop_intersperse)
    ,("transpose",      mytest prop_transpose)
    ,("foldl",          mytest prop_foldl)
    ,("foldr",          mytest prop_foldr)
    ,("foldl'",         mytest prop_foldl')
    ,("foldl1",         mytest prop_foldl1)
    ,("foldr1",         mytest prop_foldr1)
    ,("foldl1'",        mytest prop_foldl1')
    
    ,("concat",         mytest prop_concat)
    ,("concatMap",      mytest prop_concatMap)
    ,("any",            mytest prop_any)
    ,("all",            mytest prop_all)
    ,("maximum",        mytest prop_maximum)
    ,("minimum",        mytest prop_minimum)
    
    ,("scanl",          mytest prop_scanl)
    ,("scanr",          mytest prop_scanr)
    ,("scanl1",         mytest prop_scanl1)
    ,("scanr1",         mytest prop_scanr1)
    
    ,("mapAccumL",      mytest prop_mapAccumL)
    ,("mapAccumR",      mytest prop_mapAccumR)
    ,("mapIndexed",     mytest prop_mapIndexed)
    
    ,("replicate",      mytest prop_replicate)
    ,("unfoldrN",       mytest prop_unfoldrN)
    
    ,("take",           mytest prop_take)
    ,("drop",           mytest prop_drop)
    ,("splitAt",        mytest prop_splitAt)
    ,("takeWhile",      mytest prop_takeWhile)
    ,("dropWhile",      mytest prop_dropWhile)
    ,("break",          mytest prop_break)
    ,("span",           mytest prop_span)
    ,("breakEnd",       mytest prop_breakEnd)
    ,("spanEnd",        mytest prop_spanEnd)
    ,("group",          mytest prop_group)
    ,("groupBy",        mytest prop_groupBy)
    ,("inits",          mytest prop_inits)
    ,("tails",          mytest prop_tails)

    ,("split",          mytest prop_split)
    ,("splitWith",      mytest prop_splitWith)

    ,("lines",          mytest prop_lines)
    ,("unlines",        mytest prop_unlines)
    ,("words",          mytest prop_words)
    ,("unwords",        mytest prop_unwords)
    
    ,("isPrefixOf",     mytest prop_isPrefixOf)
    ,("isSuffixOf",     mytest prop_isSuffixOf)
    
    ,("isSubstringOf",  mytest prop_isSubstringOf)
    ,("findSubstring",  mytest prop_findSubstring)
    ,("findSubstrings", mytest prop_findSubstrings)
    
    ,("elem",           mytest prop_elem)
    ,("notElem",        mytest prop_notElem)
    
    ,("find",           mytest prop_find)
    ,("filter",         mytest prop_filter)
    ,("partition",      mytest prop_partition)
    
    ,("index",          mytest prop_index)
    ,("elemIndex",      mytest prop_elemIndex)
    ,("elemIndices",    mytest prop_elemIndices)
    ,("findIndex",      mytest prop_findIndex)
    ,("findIndices",    mytest prop_findIndices)
    ,("count",          mytest prop_count)
    
    ,("zip",            mytest prop_zip)
    ,("zipWith",        mytest prop_zipWith)
    ,("zipWith'",       mytest prop_zipWith')
    ,("unzip",          mytest prop_unzip)
    
    ,("sort",           mytest prop_sort)
    
    {-
    ,("",       mytest )
    -}
    ]

other_tests :: [(String, Int -> IO ())]
other_tests =
    [("unpack . pack",          mytest prop_pack)
    ,("pack . unpack",          mytest prop_unpack)
    ,("elemIndexEnd",           mytest prop_elemIndexEnd)
    ,("findIndexEnd",           mytest prop_findIndexEnd)
    ]

encoding_tests :: [(String, Int -> IO ())]
encoding_tests =
    [("from . toByteString",            mytest prop_fromToByteString)
    
    ,("decode . encode: UTF-8",         mytest prop_decode_encode_UTF8)
    ,("decode . encode: UTF-16BE",      mytest prop_decode_encode_UTF16BE)
    ,("decode . encode: UTF-16LE",      mytest prop_decode_encode_UTF16LE)
    ,("decode . encode: UTF-32BE",      mytest prop_decode_encode_UTF32BE)
    ,("decode . encode: UTF-32LE",      mytest prop_decode_encode_UTF32LE)
    ,("decode . encode: ASCII",         mytest prop_decode_encode_ASCII)
    ,("decode . encode: Latin1",        mytest prop_decode_encode_Latin1)
    ,("decode . encode: Compact",       mytest prop_decode_encode_Compact)
    
    ,("encode . decode: UTF-8",         mytest prop_encode_decode_UTF8)
    --,("encode . decode: UTF-16BE",    mytest prop_encode_decode_UTF16BE)
    --,("encode . decode: UTF-16LE",    mytest prop_encode_decode_UTF16LE)
    ,("encode . decode: UTF-32BE",      mytest prop_encode_decode_UTF32BE)
    ,("encode . decode: UTF-32LE",      mytest prop_encode_decode_UTF32LE)
    ,("encode . decode: ASCII",         mytest prop_encode_decode_ASCII)
    ,("encode . decode: Latin1",        mytest prop_encode_decode_Latin1)
    ,("encode . decode: Compact",       mytest prop_encode_decode_Compact)
    
    ,("decode no BOM",                  mytest prop_decode_no_bom)
    ,("decode / BOM: UTF-8",            mytest prop_decode_bom_UTF8)
    ,("decode / BOM: UTF-16BE",         mytest prop_decode_bom_UTF16BE)
    ,("decode / BOM: UTF-16LE",         mytest prop_decode_bom_UTF16LE)
    ,("decode / BOM: UTF-32BE",         mytest prop_decode_bom_UTF32BE)
    ,("decode / BOM: UTF-32LE",         mytest prop_decode_bom_UTF32LE)
    ]

------------------------------------------------------------------------
-- The entry point

main = run tests

run :: [(String, Int -> IO ())] -> IO ()
run tests = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests
