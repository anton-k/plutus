{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
The Cardano Node CLI expects serialised binary values to be wrapped with a
'TextEnvelope'. This module contains orphan instances of 'HasTextEnvelope'.
-}
module Ledger.TextEnvelope where

import           Cardano.Api              (AsType, HasTextEnvelope (textEnvelopeType), HasTypeProxy (proxyToAsType),
                                           SerialiseAsCBOR, TextEnvelopeType (TextEnvelopeType))
import           Cardano.Binary           (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import           Codec.Serialise          (decode, encode)
import qualified Data.Text                as Text
import           Plutus.V1.Ledger.Api     (plutusScriptEnvelopeType)
import           Plutus.V1.Ledger.Scripts (Script)
import           PlutusTx.Prelude

instance HasTextEnvelope Script where
    textEnvelopeType _ = TextEnvelopeType $ Text.unpack plutusScriptEnvelopeType

instance SerialiseAsCBOR Script

instance FromCBOR Script where
    fromCBOR = decode

instance ToCBOR Script where
    toCBOR = encode

instance HasTypeProxy Script where
    data AsType Script = AsScript
    proxyToAsType _ = AsScript
