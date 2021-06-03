{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Data.FHIR.Types (
      RID(..)
    , FHIR_Type(..)
    , Compartment(..)
    , module Data.FHIR.XML
    ) where

import Data.Aeson

import Data.Aeson.DeriveNoPrefix
--import Data.Aeson.TH                                                                                                                                            
import qualified Data.UUID as UUID
import GHC.Generics
import GHC.TypeLits
import Servant
import RIO
import qualified RIO.Text as T
 
import Data.FHIR.XML

newtype RID = RID UUID.UUID
  deriving (Generic, Eq, Show)

instance FromHttpApiData RID where      
  parseUrlPiece piece = do
    s <- parseUrlPiece piece
    case UUID.fromString s of
      Nothing -> Left . T.pack $ "no valid UUID-piece: " ++ show piece
      Just uid -> return $ RID uid

instance ToHttpApiData RID where
    toUrlPiece = T.pack . show

data FHIR_Type = 
    FHIR_Bundle
  | FHIR_Any
  | FHIR_CapabilityStatement
  | FHIR_DomainResource
  | FHIR_Encounter 
  | FHIR_Patient
  | FHIR_Resource
  deriving (Generic, Eq, Ord, Show)

instance FromHttpApiData FHIR_Type where
  parseUrlPiece piece = do
    s <- parseUrlPiece piece
    case s :: Text of
      "*"                    -> return $ FHIR_Any
      "Bundle"               -> return $ FHIR_Bundle
      "CapabilityStatement"  -> return $ FHIR_CapabilityStatement
      "Encounter"            -> return $ FHIR_Encounter
      "Patient"              -> return $ FHIR_Patient
      _ -> Left . T.pack $ "no valid FHIR type " ++ show piece

data ResourceType
  = FhirAccount
  | FhirActivityDefinition
  | FhirAdverseEvent
  | FhirAllergyIntolerance
  | FhirAppointment
  | FhirAppointmentResponse
  | FhirAuditEvent
  | FhirBasic
  | FhirBinary
  | FhirBiologicallyDerivedProduct
  | FhirBodyStructure
  | FhirBundle
  | FhirCapabilityStatement
  | FhirCarePlan
  | FhirCareTeam
  | FhirCatalogEntry
  | FhirChargeItem
  | FhirChargeItemDefinition
  | FhirClaim
  | FhirClaimResponse
  | FhirClinicalImpression
  | FhirCodeSystem
  | FhirCommunication
  | FhirCommunicationRequest
  | FhirCompartmentDefinition
  | FhirComposition
  | FhirConceptMap
  | FhirCondition
  | FhirConsent
  | FhirContract
  | FhirCoverage
  | FhirCoverageEligibilityRequest
  | FhirCoverageEligibilityResponse
  | FhirDetectedIssue
  | FhirDevice
  | FhirDeviceDefinition
  | FhirDeviceMetric
  | FhirDeviceRequest
  | FhirDeviceUseStatement
  | FhirDiagnosticReport
  | FhirDocumentManifest
  | FhirDocumentReference
  | FhirEffectEvidenceSynthesis
  | FhirEncounter
  | FhirEndpoint
  | FhirEnrollmentRequest
  | FhirEnrollmentResponse
  | FhirEpisodeOfCare
  | FhirEventDefinition
  | FhirEvidence
  | FhirEvidenceVariable
  | FhirExampleScenario
  | FhirExplanationOfBenefit
  | FhirFamilyMemberHistory
  | FhirFlag
  | FhirGoal
  | FhirGraphDefinition
  | FhirGroup
  | FhirGuidanceResponse
  | FhirHealthcareService
  | FhirImagingStudy
  | FhirImmunization
  | FhirImmunizationEvaluation
  | FhirImmunizationRecommendation
  | FhirImplementationGuide
  | FhirInsurancePlan
  | FhirInvoice
  | FhirLibrary
  | FhirLinkage
  | FhirList
  | FhirLocation
  | FhirMeasure
  | FhirMeasureReport
  | FhirMedia
  | FhirMedication
  | FhirMedicationAdministration
  | FhirMedicationDispense
  | FhirMedicationKnowledge
  | FhirMedicationRequest
  | FhirMedicationStatement
  | FhirMedicinalProduct
  | FhirMedicinalProductAuthorization
  | FhirMedicinalProductContraindication
  | FhirMedicinalProductIndication
  | FhirMedicinalProductIngredient
  | FhirMedicinalProductInteraction
  | FhirMedicinalProductManufactured
  | FhirMedicinalProductPackaged
  | FhirMedicinalProductPharmaceutical
  | FhirMedicinalProductUndesirableEffect
  | FhirMessageDefinition
  | FhirMessageHeader
  | FhirMolecularSequence
  | FhirNamingSystem
  | FhirNutritionOrder
  | FhirObservation
  | FhirObservationDefinition
  | FhirOperationDefinition
  | FhirOperationOutcome
  | FhirOrganization
  | FhirOrganizationAffiliation
  | FhirParameters
  | FhirPatient
  | FhirPaymentNotice
  | FhirPaymentReconciliation
  | FhirPerson
  | FhirPlanDefinition
  | FhirPractitioner
  | FhirPractitionerRole
  | FhirProcedure
  | FhirProvenance
  | FhirQuestionnaire
  | FhirQuestionnaireResponse
  | FhirRelatedPerson
  | FhirRequestGroup
  | FhirResearchDefinition
  | FhirResearchElementDefinition
  | FhirResearchStudy
  | FhirResearchSubject
  | FhirRiskAssessment
  | FhirRiskEvidenceSynthesis
  | FhirSchedule
  | FhirSearchParameter
  | FhirServiceRequest
  | FhirSlot
  | FhirSpecimen
  | FhirSpecimenDefinition
  | FhirStructureDefinition
  | FhirStructureMap
  | FhirSubscription
  | FhirSubstance
  | FhirSubstancePolymer
  | FhirSubstanceProtein
  | FhirSubstanceReferenceInformation
  | FhirSubstanceSpecification
  | FhirSubstanceSourceMaterial
  | FhirSupplyDelivery
  | FhirSupplyRequest
  | FhirTask
  | FhirTerminologyCapabilities
  | FhirTestReport
  | FhirTestScript
  | FhirValueSet
  | FhirVerificationResult
  | FhirVisionPrescription
  deriving (Generic, Eq, Ord, Show)


data Compartment
  = CompartmentPatient 
  | CompartmentEncounter 
  | CompartmentRelatedPerson
  | CompartmentPractitioner
  | CompartmentDevice
  deriving (Generic, Eq, Show)

instance FromHttpApiData Compartment where
  parseUrlPiece piece = do
    s <- parseUrlPiece piece
    case s :: Text of
      "Patient" -> return $ CompartmentPatient
      "Encounter" -> return $ CompartmentEncounter
      "RelatedPerson" -> return $ CompartmentRelatedPerson
      "Practitioner" -> return $ CompartmentPractitioner
      "Device" -> return $ CompartmentDevice
      _ -> Left . T.pack $ "no valid compartment type: " ++ show piece

instance ToHttpApiData Compartment where
    toUrlPiece = T.pack . show

