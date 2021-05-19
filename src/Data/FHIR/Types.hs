{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Data.FHIR.Types where
import Data.Aeson
import Data.Aeson.DeriveNoPrefix
--import Data.Aeson.TH                                                                                                                                            
import qualified Data.UUID as UUID
import GHC.Generics
import GHC.TypeLits
import Servant
import RIO
import qualified RIO.Map as Map
import qualified RIO.Text as T
 

newtype RID = RID UUID.UUID
  deriving (Eq, Show)

instance FromHttpApiData RID where      
  parseUrlPiece piece = do
    s <- parseUrlPiece piece
    case UUID.fromString s of
      Nothing -> Left . T.pack $ "no valid UUID-piece " ++ show piece
      Just uid -> return $ RID uid

instance ToHttpApiData RID where
    toUrlPiece = T.pack . show

data FHIR_Type = 
    FHIR_Bundle
  | FHIR_CapabilityStatement
  | FHIR_Encounter 
  | FHIR_Patient
  deriving (Eq, Show)

instance FromHttpApiData FHIR_Type where
  parseUrlPiece piece = do
    s <- parseUrlPiece piece
    case s :: Text of
      "Bundle"               -> return $ FHIR_Bundle
      "CapabilityStatment"   -> return $ FHIR_CapabilityStatement
      "Encounter"            -> return $ FHIR_Encounter
      "Patient"              -> return $ FHIR_Patient
      _ -> Left . T.pack $ "no valid FHIR type " ++ show piece

data Compartment = CT_Patient | CT_Encounter 
  deriving (Eq, Show)

instance FromHttpApiData Compartment where
  parseUrlPiece piece = do
    s <- parseUrlPiece piece
    case s :: Text of
      "Patient" -> return $ CT_Patient
      "Encounter" -> return $ CT_Encounter
      _ -> Left . T.pack $ "no valid compartment type " ++ show piece

instance ToHttpApiData Compartment where
    toUrlPiece = T.pack . show

