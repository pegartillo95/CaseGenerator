{-# LANGUAGE CPP #-}

module Arbitrary where

import System.Random
import System.Random.TF
import System.Random.TF.Gen(splitn)
import Data.Word
import Data.Bits
import Data.Char
import Data.Ratio
import qualified Data.Set as Set

import Control.Monad
  ( liftM
  , liftM2
  , liftM3
  , liftM4
  , liftM5
  )

-- | This is the exported, visible class
class Arbitrary a where
    arbitrary :: Gen a

--instances for the class Arbitrary of base data types
instance Arbitrary () where
  arbitrary = return ()

instance Arbitrary Bool where
      arbitrary = choose (False,True)

instance Arbitrary Ordering where
      arbitrary = elements [LT, EQ, GT]

instance Arbitrary Integer where
  arbitrary = arbitrarySizedIntegral

instance Arbitrary Int where
  arbitrary = arbitrarySizedIntegral

instance Arbitrary Float where
  arbitrary = arbitrarySizedFractional

instance Arbitrary Double where
  arbitrary = arbitrarySizedFractional

instance Arbitrary Char where
  arbitrary = arbitraryChar

--instance for the class Arbitrary for the complex data types
instance Arbitrary a => Arbitrary (Maybe a) where
      arbitrary = arbitrary1

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
      arbitrary = arbitrary2

instance Arbitrary a => Arbitrary [a] where
      arbitrary = arbitrary1

--instance for the different types of tuples
instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
      arbitrary = arbitrary2

instance (Arbitrary a, Arbitrary b, Arbitrary c)
         => Arbitrary (a,b,c)
     where
      arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
         => Arbitrary (a,b,c,d)
     where
      arbitrary = liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e)
         => Arbitrary (a,b,c,d,e)
     where
      arbitrary = liftM5 (,,,,) arbitrary arbitrary arbitrary arbitrary arbitrary

---- Arbitrary instances for container types
instance (Ord a, Arbitrary a) => Arbitrary (Set.Set a) where
  arbitrary = fmap Set.fromList arbitrary

  ------------------------------------------------------------------------------------------------
------------------------------------------Auxiliar functions -----------------------------------
------------------------------------------------------------------------------------------------
#define TheGen TFGen

newTheGen :: IO TFGen
newTheGen = newTFGen

bits, mask, doneBit :: Integral a => a
bits = 14
mask = 0x3fff
doneBit = 0x4000

chip :: Bool -> Word32 -> TFGen -> TFGen
chip done n g = splitn g (bits+1) (if done then m .|. doneBit else m)
  where
    m = n .&. mask

chop :: Integer -> Integer
chop n = n `shiftR` bits

stop :: Integral a => a -> Bool
stop n = n <= mask

mkTheGen :: Int -> TFGen
mkTheGen = mkTFGen

newtype Gen a = MkGen{
  unGen :: QCGen -> Int -> a -- ^ Run the generator on a particular seed.
 }

-- | The "standard" QuickCheck random number generator.
-- A wrapper around either 'TFGen' on GHC, or 'StdGen'
-- on other Haskell systems.
newtype QCGen = QCGen TheGen

frequency :: [(Int, Arbitrary a)] -> Arbitrary a
frequency [] = error "QuickCheck.frequency used with empty list"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error "QuickCheck.pick used with empty list"

sized :: (Int -> Gen a) -> Gen a
sized f = MkGen (\r n -> let MkGen m = f n in m r n)

-- | Generates a random element in the given inclusive range.
choose :: Random a => (a,a) -> Gen a
choose rng = MkGen (\r _ -> let (x,_) = randomR rng r in x)

-- | Generates one of the given values. The input list must be non-empty.
elements :: [a] -> Gen a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

-- Useful for getting at minBound and maxBound without having to
-- fiddle around with asTypeOf.
withBounds :: Bounded a => (a -> a -> Gen a) -> Gen a
withBounds k = k minBound maxBound

inBounds :: Integral a => (Integer -> a) -> Gen Integer -> Gen a
inBounds fi g = fmap fi (g `suchThat` (\x -> toInteger (fi x) == x))

-- | Generates a value that satisfies a predicate.
suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p =
  do mx <- gen `suchThatMaybe` p
     case mx of
       Just x  -> return x
       Nothing -> sized (\n -> resize (n+1) (gen `suchThat` p))

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (try 0 . max 1)
 where
  try _ 0 = return Nothing
  try k n = do x <- resize (2*k+n) gen
               if p x then return (Just x) else try (k+1) (n-1)

-- | Overrides the size parameter. Returns a generator which uses
-- the given size instead of the runtime-size parameter.
resize :: Int -> Gen a -> Gen a
resize n _ | n < 0 = error "Test.QuickCheck.resize: negative size"
resize n (MkGen g) = MkGen (\r _ -> g r n)

-----------------------------------------------------------------------------
-------------------------Arbitrary generators--------------------------------
-----------------------------------------------------------------------------

-- | Generates an integral number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedIntegral :: Integral a => Gen a
arbitrarySizedIntegral =
  sized $ \n ->
  inBounds fromInteger (choose (-toInteger n, toInteger n))


-- | Generates a fractional number. The number can be positive or negative
-- and its maximum absolute value depends on the size parameter.
arbitrarySizedFractional :: Fractional a => Gen a
arbitrarySizedFractional =
  sized $ \n ->
    let n' = toInteger n in
      do a <- choose ((-n') * precision, n' * precision)
         b <- choose (1, precision)
         return (fromRational (a % b))
 where
  precision = 9999999999999 :: Integer

-- | Generates a character.
-- ASCII characters are generated more often than non-ASCII.
arbitraryChar :: Gen Char
arbitraryChar =
  frequency
    [(3, arbitraryASCIIChar),
     (1, arbitraryUnicodeChar)]

-- | Generates any Unicode character (but not a surrogate)
arbitraryUnicodeChar :: Gen Char
arbitraryUnicodeChar =
  arbitraryBoundedEnum `suchThat` (not . isSurrogate)
  where
    isSurrogate c = generalCategory c == Surrogate

-- | Generates a random ASCII character (0-127).
arbitraryASCIIChar :: Gen Char
arbitraryASCIIChar = choose ('\0', '\127')

-- | Generates an element of a bounded enumeration.
arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedEnum =
  withBounds $ \mn mx ->
  do n <- choose (fromEnum mn, fromEnum mx)
     return (toEnum n)


------------------------------------------------------------------------------------
--------------------Auxiliary classes-----------------------------------------------
------------------------------------------------------------------------------------

-- | Lifting of the 'Arbitrary' class to unary type constructors.
class Arbitrary1 f where
  liftArbitrary :: Gen a -> Gen (f a)

arbitrary1 :: (Arbitrary1 f, Arbitrary a) => Gen (f a)
arbitrary1 = liftArbitrary arbitrary

-- | Lifting of the 'Arbitrary' class to binary type constructors.
class Arbitrary2 f where
  liftArbitrary2 :: Gen a -> Gen b -> Gen (f a b)

arbitrary2 :: (Arbitrary2 f, Arbitrary a, Arbitrary b) => Gen (f a b)
arbitrary2 = liftArbitrary2 arbitrary arbitrary