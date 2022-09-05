{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module PlutusComplexValidator  where

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.String        (IsString (..))
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Ada           as Ada
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx.Builtins.Internal as B
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Builtins.Class    as PTXBC
import qualified PlutusTx.Prelude as PTXP
import qualified Plutus.V1.Ledger.Scripts as Plutus
import           Prelude hiding (($))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

------------------------------------------------------

data MyComplexParameter = MyComplexParameter
    {
        parameterInteger :: Integer,
        parameterString :: B.BuiltinByteString
    }
     deriving Show

PlutusTx.makeLift ''MyComplexParameter

------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: MyComplexParameter -> MyComplexDatum -> MyComplexRedeemer -> ScriptContext -> Bool
mkValidator parameter datum redeemer scriptcontext =

        PTXP.traceIfFalse get_compare_errorstring_for_parameterIntegerTodatumInteger compare_parameterIntegerTodatumInteger &&
        PTXP.traceIfFalse get_compare_errorstring_for_datumIntegerToredeemerInteger compare_datumIntegerToredeemerInteger &&
        PTXP.traceIfFalse get_compare_errorstring_for_parameterStringTodatumString compare_parameterStringTodatumString &&
        PTXP.traceIfFalse get_compare_errorstring_for_datumStringToredeemerString compare_datumStringToredeemerString

        where
        
        get_compare_errorstring_for_parameterIntegerTodatumInteger :: B.BuiltinString
        get_compare_errorstring_for_parameterIntegerTodatumInteger = "parameterInteger is not equal to datumInteger"
        
        get_compare_errorstring_for_datumIntegerToredeemerInteger :: B.BuiltinString
        get_compare_errorstring_for_datumIntegerToredeemerInteger = "datumInteger is not equal to redeemerInteger"
        
        get_compare_errorstring_for_parameterStringTodatumString :: B.BuiltinString
        get_compare_errorstring_for_parameterStringTodatumString = "parameterString is not equal to datumString" 
        
        get_compare_errorstring_for_datumStringToredeemerString :: B.BuiltinString
        get_compare_errorstring_for_datumStringToredeemerString = "datumString is not equal to redeemerString"

        
        compare_parameterIntegerTodatumInteger :: Bool
        compare_parameterIntegerTodatumInteger = get_parameterInteger PTXP.== get_datumInteger
        
        compare_datumIntegerToredeemerInteger :: Bool
        compare_datumIntegerToredeemerInteger = get_datumInteger PTXP.== get_redeemerInteger
        
        compare_parameterStringTodatumString :: Bool
        compare_parameterStringTodatumString = get_parameterString PTXP.== get_datumString
        
        compare_datumStringToredeemerString :: Bool
        compare_datumStringToredeemerString = get_datumString PTXP.== get_redeemerString
 
        
        get_parameterInteger :: Integer
        get_parameterInteger = parameterInteger parameter
        
        get_parameterString :: B.BuiltinByteString
        get_parameterString = parameterString parameter
        
        get_datumInteger :: Integer
        get_datumInteger = datumInteger datum       
        
        get_datumString :: B.BuiltinByteString
        get_datumString = datumString datum
        
        get_redeemerInteger :: Integer
        get_redeemerInteger = redeemerInteger redeemer 
        
        get_redeemerString :: B.BuiltinByteString
        get_redeemerString = redeemerString redeemer

------------------------------------------------------

data PlutusComplexValidator
instance Scripts.ValidatorTypes PlutusComplexValidator where
    type instance DatumType PlutusComplexValidator = MyComplexDatum
    type instance RedeemerType PlutusComplexValidator = MyComplexRedeemer

typedValidator :: MyComplexParameter -> Scripts.TypedValidator PlutusComplexValidator
typedValidator p = Scripts.mkTypedValidator @PlutusComplexValidator
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MyComplexDatum @MyComplexRedeemer 

validator :: MyComplexParameter -> Scripts.Validator
validator = Scripts.validatorScript . typedValidator

------------------------------------------------------

data MyComplexDatum = MyComplexDatum
    { 
        datumInteger :: Integer,
        datumString :: B.BuiltinByteString
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


data MyComplexRedeemer = MyComplexRedeemer
    {
        redeemerInteger :: Integer,
        redeemerString :: B.BuiltinByteString
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MyComplexDatum
PlutusTx.unstableMakeIsData ''MyComplexRedeemer