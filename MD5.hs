module MD5 (md5)
    where

import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Word
import Text.Printf

type Hash = (Word32, Word32, Word32, Word32, Int)
type HashS = State Hash ()
type Hasher = Hash -> (Word32, Word32)
type Munch = (String, Word64, [Char], [Word32], Hash)
type MunchS = State Munch ()

md5 :: String -> String
md5 = hexify . unword' . md5'
    where hexify = foldr showHex ""
          showHex = (++) . (printf "%02x")
          unword' = foldr unword []

md5' :: String -> [Word32]
md5' str = [a, b, c, d]
    where (a, b, c, d, _) = calcHash (str, 0, [], [], h0)

calcHash :: Munch -> Hash
calcHash s = h
    where (_, _, _, _, h) = muncher s

muncher :: Munch -> Munch
muncher s = muncher' (trail l, l, a, block, h)
    where (_, l, a, block, h) = muncher' s

muncher' :: Munch -> Munch
muncher' s@([], _, _, _, h) = s
muncher' s = muncher' $! execState munch s

munch :: MunchS
munch = do
    (cur : rest, l, a, block, h) <- get
    let l' = l `seq` l + 1
    let p = l' `seq` l' `mod` charsInWord
    let a' = a `seq` cur `seq` (cur : a)
    let (a'', block') = if p == 0
                            then a' `seq` block `seq`
                                ([], ((word . reverse) a') : block)
                            else a' `seq` block `seq`
                                (a', block)
    let (block'', h') = if l' `mod` charsInBlock == 0
                            then block' `seq` h `seq`
                                ([], execState ((hash . reverse) block') h)
                            else block' `seq` h `seq`
                                (block', h)
    let (aa, bb, cc, dd, ii) = h'
    aa `seq` bb `seq` cc `seq` dd `seq` ii `seq`
        l' `seq` a'' `seq` block'' `seq` h' `seq`
        put (rest, l', a'', block'', h')

hash :: [Word32] -> HashS
hash block = do
    (a, b, c, d, _) <- get
    let phases = a `seq` b `seq` c `seq` d `seq` [ein, zwei, drei, vier]
    let steps = concat $ map (replicate $ kSize `div` (length phases)) phases
    mapM_ (hashBlock block) steps
    (a', b', c', d', i) <- get
    a' `seq` b' `seq` c' `seq` d' `seq` i `seq`
        put (a + a', b + b', c + c', d + d', i)

hashBlock :: [Word32] -> Hasher -> HashS
hashBlock block hasher = do
    h@(a, b, c, d, i) <- get
    let (f, g) = a `seq` b `seq` c `seq` d `seq` (hasher h)
    let b' = f `seq` g `seq` i `seq` (calcB block a b f g i)
    let i' = (i + 1) `mod` kSize
    b' `seq` i' `seq` put (d, b', b, c, i')

ein :: Hasher
ein (_, b, c, d, i) = f' `seq` g' `seq` (f', g')
    where f' = (b .&. c) .|. ((complement b) .&. d)
          g' = fromIntegral i

zwei :: Hasher
zwei (_, b, c, d, i) = f' `seq` g' `seq` (f', g')
    where f' = (d .&. b) .|. ((complement d) .&. c)
          g' = fromIntegral $ (5 * i + 1) `mod` blockWords

drei :: Hasher
drei (_, b, c, d, i) = f' `seq` g' `seq` (f', g')
    where f' = b `xor` c `xor` d
          g' = fromIntegral $ (3 * i + 5) `mod` blockWords

vier :: Hasher
vier (_, b, c, d, i) = f' `seq` g' `seq` (f', g')
    where f' = c `xor` (b .|. (complement d))
          g' = fromIntegral $ (7 * i) `mod` blockWords

calcB :: [Word32] -> Word32 -> Word32 -> Word32 -> Word32 -> Int -> Word32
calcB block a b f g i = a `seq` b `seq` f `seq` ki `seq` ri `seq` bg `seq`
    (b + (rotateL (a + f + ki + bg) ri))
    where bg = block !! (fromIntegral g)
          ki = k !! (fromIntegral i)
          ri = fromIntegral (r !! (fromIntegral i))

trail :: Word64 -> String
trail l = "\x80" ++ (replicate zeroBlock' '\0') ++ len64 l'
    where l' = l * charSize
          zeroBlock = charsInBlock - (l + charsInLen + 1) `mod` charsInBlock
          zeroBlock' = fromIntegral $ zeroBlock

len64 :: Word64 -> String
len64 l = map (byte l) [0 .. charSize - 1]

unword :: Word32 -> [Word8] -> [Word8]
unword w = (++) $ map wordByte [0 .. charsInWord - 1]
    where wordByte = fromIntegral . ord . (byte w)

byte :: (Integral a) => a -> a -> Char
byte x b = chr . fromIntegral $
    x `div` beyondChar ^ b `mod` beyondChar

word :: [Char] -> Word32
word = foldr acc' 0
    where acc' x a = a `seq` x `seq` (a * beyondChar + (fromIntegral . ord) x)

k :: [Word32]
k = map fk [1 .. to]
  where to = fromIntegral kSize
        fac = fromIntegral beyondWord
        fk = floor . (* fac) . abs . sin

kSize :: (Num a, Integral a) => a
kSize = fromIntegral 64

r :: [Word32]
r = (concat . map fr) [0 .. 3]
    where fr = (take blockWords) . cycle . r'

r' :: Int -> [Word32]
r' 0 = [7, 12, 17, 22]
r' 1 = [5, 9, 14, 20]
r' 2 = [4, 11, 16, 23]
r' 3 = [6, 10, 15, 21]

h0 :: Hash
h0 = (0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0)

blockSize :: (Num a, Integral a) => a
blockSize = fromIntegral blockWords * wordSize

blockWords :: (Num a, Integral a) => a
blockWords = fromIntegral 16

lenSize :: (Num a, Integral a) => a
lenSize = fromIntegral $ bitSize (minBound :: Word64)

wordSize :: (Num a, Integral a) => a
wordSize = fromIntegral $ bitSize (minBound :: Word32)

beyondWord :: (Num a, Integral a) => a
beyondWord = fromIntegral 2 ^ wordSize

charSize :: (Num a, Integral a) => a
charSize = fromIntegral $ bitSize (minBound :: Word8)

beyondChar :: (Num a, Integral a) => a
beyondChar = fromIntegral 2 ^ charSize

charsInBlock :: (Num a, Integral a) => a
charsInBlock = fromIntegral blockSize `div` charSize

charsInLen :: (Num a, Integral a) => a
charsInLen = fromIntegral lenSize `div` charSize

charsInWord :: (Num a, Integral a) => a
charsInWord = fromIntegral wordSize `div` charSize
