{-# LANGUAGE BangPatterns, MagicHash, TypeOperators #-}

module Data.BloomFilter.Util
    (
      FastShift(..)
    , nextPowerOfTwo
    , (:*)(..)
    ) where

import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import GHC.Word

-- | A strict pair type.
data a :* b = !a :* !b
            deriving (Eq, Ord, Show)

-- | Compute the nearest power of two greater to or equal than the
-- given number.
nextPowerOfTwo :: Int -> Int
{-# INLINE nextPowerOfTwo #-}
nextPowerOfTwo n =
    let a = n - 1
        b = a .|. (a `shiftR` 1)
        c = b .|. (b `shiftR` 2)
        d = c .|. (c `shiftR` 4)
        e = d .|. (d `shiftR` 8)
        f = e .|. (e `shiftR` 16)
        g = f .|. (f `shiftR` 32)  -- in case we're on a 64-bit host
        !h = g + 1
    in h

-- | This is a workaround for poor optimisation in GHC 6.8.2.  It
-- fails to notice constant-width shifts, and adds a test and branch
-- to every shift.  This imposes about a 10% performance hit.
-- AVG: This is no longer the case
class FastShift a where
    shiftL :: a -> Int -> a
    shiftR :: a -> Int -> a

instance FastShift Word32 where
    {-# INLINE shiftL #-}
    shiftL = Bits.unsafeShiftL

    {-# INLINE shiftR #-}
    shiftR = Bits.unsafeShiftR

instance FastShift Word64 where
    {-# INLINE shiftL #-}
    shiftL = Bits.unsafeShiftL

    {-# INLINE shiftR #-}
    shiftR = Bits.unsafeShiftR

instance FastShift Int where
    {-# INLINE shiftL #-}
    shiftL = Bits.unsafeShiftL

    {-# INLINE shiftR #-}
    shiftR = Bits.unsafeShiftR

instance FastShift Integer where
    {-# INLINE shiftL #-}
    shiftL = Bits.unsafeShiftL

    {-# INLINE shiftR #-}
    shiftR = Bits.unsafeShiftR
