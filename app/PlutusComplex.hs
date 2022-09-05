{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Cardano.Api.Shelley ( fromPlutusData )
import           Codec.Serialise       (serialise)
import           Data.String
import qualified Data.String           as FS
import qualified Data.ByteString.Short as SBS
import           Data.Aeson            (encode)
import           Data.Aeson as Json ( encode )
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.ByteString.Lazy qualified as LB
import qualified Ledger
import           Ledger
import           PlutusTx              (Data (..))
import qualified PlutusTx
import           PlutusTx.Builtins.Class as PTXBC
import qualified PlutusTx.Builtins.Internal as B
import qualified Plutus.V1.Ledger.Api as Plutus
import           Plutus.V1.Ledger.Value
import           Prelude
import           System.Environment (getArgs)

import PlutusComplexValidator

------------------------------------------------------

main :: IO ()
main = do
  arguments <- getArgs
  processCommand arguments

  putStrLn ""
  putStrLn "Done!"
  putStrLn ""

------------------------------------------------------

processCommand :: [String] -> IO ()
processCommand arguments = do 
    let command = getCommand arguments
    case command of "write-datum" -> processDatum arguments
                    "write-redeemer" -> processRedeemer arguments
                    "write-validator" -> processValidator arguments

------------------------------------------------------


processDatum :: [String] -> IO ()
processDatum arguments = do
  let command = getCommand arguments
  let outputFile =  getOuputFile arguments
  let datumInteger = getIntFromArray arguments 2
  let datumString = getBIBStringFromArray arguments 3
  writeDatum outputFile datumInteger datumString
  let resultString = ( show ( datumInteger )  ++ " " ++ show( datumString ) )
  printResult command outputFile resultString

processRedeemer :: [String] -> IO ()
processRedeemer arguments = do
  let command = getCommand arguments
  let outputFile =  getOuputFile arguments
  let redeemerInteger = getIntFromArray arguments 2
  let redeemerString = getBIBStringFromArray arguments 3
  writeRedeemer outputFile redeemerInteger redeemerString
  let resultString = ( show ( redeemerInteger )  ++ " " ++ show( redeemerString ) )
  printResult command outputFile resultString

processValidator :: [String] -> IO ()
processValidator arguments = do
  let command = getCommand arguments
  let outputFile =  getOuputFile arguments
  let parameterInteger = getIntFromArray arguments 2
  let parameterString = getBIBStringFromArray arguments 3
  writeMyValidator outputFile parameterInteger parameterString
  let resultString = ( show ( parameterInteger )  ++ " " ++ show( parameterString ) )
  printResult command outputFile resultString

writeDatum :: String -> Integer -> B.BuiltinByteString -> IO ()
writeDatum outputFile inputdatumInteger inputdatumString = do
    let myDatum = MyComplexDatum   {
        datumInteger = inputdatumInteger ,
        datumString = inputdatumString    }
    writeData outputFile myDatum

writeRedeemer :: String -> Integer -> B.BuiltinByteString  -> IO ()
writeRedeemer outputFile inputredeemerInteger inputredeemerString  = do
    let myRedeemer = MyComplexRedeemer   {
        redeemerInteger = inputredeemerInteger ,
        redeemerString = inputredeemerString    }
    writeData outputFile myRedeemer

writeMyValidator :: FilePath -> Integer -> B.BuiltinByteString -> IO (Either (FileError ()) ())
writeMyValidator scriptFile inputparameterInteger inputparameterString = writeValidator scriptFile $ validator $ MyComplexParameter
    { 
        parameterInteger = inputparameterInteger ,
        parameterString = inputparameterString 
    }

------------------------------------------------------

getCommand :: [String] -> String
getCommand arguments = getStringFromArray arguments 0

getOuputFile :: [String] -> String
getOuputFile arguments = getStringFromArray arguments 1

printResult :: String ->  String -> String -> IO ()
printResult command outputFile resultString = do 
  putStrLn ( "succesfully processed " ++ command ++ " " ++ resultString)
  putStrLn ( "output written to: ")
  putStrLn outputFile


------------------------------------------------------

stringToBIBString :: String -> B.BuiltinByteString
stringToBIBString inputString = FS.fromString inputString

getIntFromArray :: [String] -> Int -> Integer
getIntFromArray inputArray index = fromInteger $ read (inputArray !! index)

getStringFromArray :: [String] -> Int -> String
getStringFromArray inputArray index = FS.fromString (inputArray !! index)

getBIBStringFromArray :: [String] -> Int -> B.BuiltinByteString
getBIBStringFromArray inputArray index = FS.fromString (getStringFromArray inputArray index)

------------------------------------------------------

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  print file
  LB.writeFile file (toJsonString isData)

writeUnit :: IO ()
writeUnit = writeJSON "unit.json" ()

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript