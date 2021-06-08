{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
-- For the 'IsString' instance
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.ByteString.Instances (stringToBuiltinByteString) where

import           Data.String                  (IsString (..))
import           PlutusTx.ByteString.Internal as Plutus
import qualified PlutusTx.String              as String

import qualified GHC.Magic                    as Magic
{- Note [noinline hack]
For some functions we have two conflicting desires:
- We want to have the unfolding available for the plugin.
- We don't want the function to *actually* get inlined before the plugin runs, since we rely
on being able to see the original function for some reason.

'INLINABLE' achieves the first, but may cause the function to be inlined too soon.

We can solve this at specific call sites by using the 'noinline' magic function from
GHC. This stops GHC from inlining it. As a bonus, it also won't be inlined if
that function is compiled later into the body of another function.

We do therefore need to handle 'noinline' in the plugin, as it itself does not have
an unfolding.
-}

-- We can't put this in `Builtins.hs`, since that force `O0` deliberately, which prevents
-- the unfoldings from going in. So we just stick it here. Fiddly.
instance IsString ByteString where
    -- Try and make sure the dictionary selector goes away, it's simpler to match on
    -- the application of 'stringToBuiltinByteString'
    {-# INLINE fromString #-}
    -- See Note [noinline hack]
    fromString = Magic.noinline stringToBuiltinByteString

{-# INLINABLE stringToBuiltinByteString #-}
stringToBuiltinByteString :: String -> ByteString
stringToBuiltinByteString = String.encodeUtf8 . String.stringToBuiltinString
