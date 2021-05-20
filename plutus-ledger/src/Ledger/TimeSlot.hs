{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
-- {-# LANGUAGE TemplateHaskell    #-}

-- This GHC option prevents the error:
-- "GHC Core to PLC plugin: E042:Error: Unsupported feature: Kind: *"
-- Because Plutus can't handle unboxed tuples which come from worker/wrapper
{-# OPTIONS_GHC -fno-worker-wrapper #-}

{-# LANGUAGE NamedFieldPuns     #-}
module Ledger.TimeSlot(
  SlotConfig(..)
, slotRangeToPOSIXTimeRange
, slotToPOSIXTimeRange
, slotToBeginPOSIXTime
, slotToEndPOSIXTime
, posixTimeRangeToSlotRange
, posixTimeToSlot
, currentSlot
) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Default              (Default (def))
import           GHC.Generics              (Generic)
import           Plutus.V1.Ledger.Interval (Interval (Interval, ivFrom, ivTo), interval)
import           Plutus.V1.Ledger.Slot     (Slot (Slot), SlotRange)
import           Plutus.V1.Ledger.Time     (POSIXTime (POSIXTime, getPOSIXTime), POSIXTimeRange)
import           PlutusTx.Prelude
import           Prelude                   (IO)
import qualified Prelude                   as Haskell

data SlotConfig =
    SlotConfig
        { scSlotLength   :: Integer -- ^ Length (number of seconds) of 1 slot
        , scZeroSlotTime :: POSIXTime -- ^ Beginning of the first slot
        }
    deriving (Haskell.Show, Haskell.Eq, Generic, ToJSON, FromJSON)

instance Default SlotConfig where
  {-# INLINABLE def #-}
  def = SlotConfig{ scSlotLength = 5, scZeroSlotTime = POSIXTime beginOfTime }

{-# INLINABLE beginOfTime #-}
-- | 'beginOfTime' corresponds to the Shelley launch date
-- (2020-07-29T21:44:51Z) which is 1596059091 in POSIX time.
beginOfTime :: Integer
beginOfTime = 1596059091

{-# INLINABLE slotRangeToPOSIXTimeRange #-}
-- | Convert a 'SlotRange' to 'POSIXTimeRange'
slotRangeToPOSIXTimeRange :: SlotConfig -> SlotRange -> POSIXTimeRange
slotRangeToPOSIXTimeRange sc sr =
  let lbound = slotToBeginPOSIXTime sc <$> ivFrom sr
      ubound = slotToEndPOSIXTime sc <$> ivTo sr
   in Interval lbound ubound

{-# INLINABLE slotToPOSIXTimeRange #-}
slotToPOSIXTimeRange :: SlotConfig -> Slot -> POSIXTimeRange
slotToPOSIXTimeRange sc slot =
  interval (slotToBeginPOSIXTime sc slot) (slotToEndPOSIXTime sc slot)

{-# INLINABLE slotToBeginPOSIXTime #-}
slotToBeginPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToBeginPOSIXTime SlotConfig{scSlotLength, scZeroSlotTime} (Slot n) =
  let secondsAfterBegin = n * scSlotLength
   in POSIXTime $ getPOSIXTime scZeroSlotTime + secondsAfterBegin

{-# INLINABLE slotToEndPOSIXTime #-}
slotToEndPOSIXTime :: SlotConfig -> Slot -> POSIXTime
slotToEndPOSIXTime sc@SlotConfig{scSlotLength} slot =
  slotToBeginPOSIXTime sc slot + POSIXTime (scSlotLength - 1)

{-# INLINABLE posixTimeRangeToSlotRange #-}
-- | Convert a 'POSIXTimeRange' to 'SlotRange'
posixTimeRangeToSlotRange :: SlotConfig -> POSIXTimeRange -> SlotRange
posixTimeRangeToSlotRange sc ptr = posixTimeToSlot sc <$> ptr

{-# INLINABLE posixTimeToSlot #-}
-- | Convert a 'POSIXTime' to 'Slot'
posixTimeToSlot :: SlotConfig -> POSIXTime -> Slot
posixTimeToSlot SlotConfig{scSlotLength, scZeroSlotTime} (POSIXTime t) =
  let timePassed = t - getPOSIXTime scZeroSlotTime
      slotsPassed = divide timePassed scSlotLength
  in Slot slotsPassed

-- | Get the current slot number
currentSlot :: SlotConfig -> IO Slot
currentSlot _ = return $ Slot 0
