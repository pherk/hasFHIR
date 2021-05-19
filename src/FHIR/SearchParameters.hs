{-# LANGUAGE NoImplicitPrelude #-}
module FHIR.SearchParameters where

data Params = Params [SearchParameter]
  deriving (Generic, Show)

   
-- | A search parameter that defines a named search item that 
--   can be used to search/filter on a resource.
data SearchParameter = SearchParameter
        { searchParameter_id :: Maybe Id
          -- ^ The logical id of the resource, as used in the URL for the 
          --   resource. Once assigned, this value never changes.
        , searchParameter_meta :: Maybe Meta
          -- ^ The metadata about the resource. This is content that is 
          --   maintained by the infrastructure. Changes to the content 
          --   might not always be associated with version changes to the 
          --   resource.
        , searchParameter_implicitRules :: Maybe Uri
          --   resource was constructed, and which must be understood when 
          --   processing the content. Often, this is a reference to an 
          --   implementation guide that defines the special rules along 
          --   with other profiles etc.
        , searchParameter_language :: Maybe Code
          -- ^ The base language in which the resource is written.
        , searchParameter_text :: Maybe Narrative
          -- ^ A human-readable narrative that contains a summary of the 
          --   resource and can be used to represent the content of the 
          --   resource to a human. The narrative need not encode all the 
          --   structured data, but is required to contain sufficient 
          --   detail to make it &quot;clinically safe&quot; for a human 
          --   to just read the narrative. Resource definitions may define 
          --   what content should be represented in the narrative to 
          --   ensure clinical safety.
        , searchParameter_contained :: [ResourceContainer]
          -- ^ These resources do not have an independent existence apart 
          --   from the resource that contains them - they cannot be 
          --   identified independently, and nor can they have their own 
          --   independent transaction scope.
        , searchParameter_extension :: [Extension]
          -- ^ May be used to represent additional information that is not 
          --   part of the basic definition of the resource. To make the 
          --   use of extensions safe and manageable, there is a strict 
          --   set of governance applied to the definition and use of 
          --   extensions. Though any implementer can define an extension, 
          --   there is a set of requirements that SHALL be met as part of 
          --   the definition of the extension.
        , searchParameter_modifierExtension :: [Extension]
          -- ^ May be used to represent additional information that is not 
          --   part of the basic definition of the resource and that 
          --   modifies the understanding of the element that contains it 
          --   and/or the understanding of the containing element's 
          --   descendants. Usually modifier elements provide negation or 
          --   qualification. To make the use of extensions safe and 
          --   manageable, there is a strict set of governance applied to 
          --   the definition and use of extensions. Though any 
          --   implementer is allowed to define an extension, there is a 
          --   set of requirements that SHALL be met as part of the 
          --   definition of the extension. Applications processing a 
          --   resource are required to check for modifier extensions. 
          --   Modifier extensions SHALL NOT change the meaning of any 
          --   elements on Resource or DomainResource (including cannot 
          --   change the meaning of modifierExtension itself).
        , searchParameter_url :: Uri
          -- ^ An absolute URI that is used to identify this search 
          --   parameter when it is referenced in a specification, model, 
          --   design or an instance; also called its canonical 
          --   identifier. This SHOULD be globally unique and SHOULD be a 
          --   literal address at which at which an authoritative instance 
          --   of this search parameter is (or will be) published. This 
          --   URL can be the target of a canonical reference. It SHALL 
          --   remain the same when the search parameter is stored on 
          --   different servers.
        , searchParameter_version :: Maybe Xsd.XsdString
          -- ^ The identifier that is used to identify this version of the 
          --   search parameter when it is referenced in a specification, 
          --   model, design or instance. This is an arbitrary value 
          --   managed by the search parameter author and is not expected 
          --   to be globally unique. For example, it might be a timestamp 
          --   (e.g. yyyymmdd) if a managed version is not available. 
          --   There is also no expectation that versions can be placed in 
          --   a lexicographical sequence.
        , searchParameter_name :: Xsd.XsdString
          -- ^ A natural language name identifying the search parameter. 
          --   This name should be usable as an identifier for the module 
          --   by machine processing applications such as code generation.
        , searchParameter_derivedFrom :: Maybe Canonical
          -- ^ Where this search parameter is originally defined. If a 
          --   derivedFrom is provided, then the details in the search 
          --   parameter must be consistent with the definition from which 
          --   it is defined. i.e. the parameter should have the same 
          --   meaning, and (usually) the functionality should be a proper 
          --   subset of the underlying search parameter.
        , searchParameter_status :: PublicationStatus
          -- ^ The status of this search parameter. Enables tracking the 
          --   life-cycle of the content.
        , searchParameter_experimental :: Maybe Boolean
          -- ^ A Boolean value to indicate that this search parameter is 
          --   authored for testing purposes (or 
          --   education/evaluation/marketing) and is not intended to be 
          --   used for genuine usage.
        , searchParameter_date :: Maybe DateTime
          -- ^ The date (and optionally time) when the search parameter 
          --   was published. The date must change when the business 
          --   version changes and it must change if the status code 
          --   changes. In addition, it should change when the substantive 
          --   content of the search parameter changes.
        , searchParameter_publisher :: Maybe Xsd.XsdString
          -- ^ The name of the organization or individual that published 
          --   the search parameter.
        , searchParameter_contact :: [ContactDetail]
          -- ^ Contact details to assist a user in finding and 
          --   communicating with the publisher.
        , searchParameter_description :: Markdown
          -- ^ And how it used.
        , searchParameter_useContext :: [UsageContext]
          -- ^ The content was developed with a focus and intent of 
          --   supporting the contexts that are listed. These contexts may 
          --   be general categories (gender, age, ...) or may be 
          --   references to specific programs (insurance plans, studies, 
          --   ...) and may be used to assist with indexing and searching 
          --   for appropriate search parameter instances.
        , searchParameter_jurisdiction :: [CodeableConcept]
          -- ^ A legal or geographic region in which the search parameter 
          --   is intended to be used.
        , searchParameter_purpose :: Maybe Markdown
          -- ^ Explanation of why this search parameter is needed and why 
          --   it has been designed as it has.
        , searchParameter_code :: Code
          -- ^ The code used in the URL or the parameter name in a 
          --   parameters resource for this search parameter.
        , searchParameter_base :: [Code]
          -- ^ The base resource type(s) that this search parameter can be 
          --   used against.
        , searchParameter_type :: SearchParamType
          -- ^ The type of value that a search parameter may contain, and 
          --   how the content is interpreted.
        , searchParameter_expression :: Maybe Xsd.XsdString
          -- ^ A FHIRPath expression that returns a set of elements for 
          --   the search parameter.
        , searchParameter_xpath :: Maybe Xsd.XsdString
          -- ^ An XPath expression that returns a set of elements for the 
          --   search parameter.
        , searchParameter_xpathUsage :: Maybe XPathUsageType
          -- ^ How the search parameter relates to the set of elements 
          --   returned by evaluating the xpath query.
        , searchParameter_target :: [Code]
          -- ^ Types of resource (if a resource is referenced).
        , searchParameter_multipleOr :: Maybe Boolean
          -- ^ Whether multiple values are allowed for each time the 
          --   parameter exists. Values are separated by commas, and the 
          --   parameter matches if any of the values match.
        , searchParameter_multipleAnd :: Maybe Boolean
          -- ^ Whether multiple parameters are allowed - e.g. more than 
          --   one parameter with the same name. The search matches if all 
          --   the parameters match.
        , searchParameter_comparator :: [SearchComparator]
          -- ^ Comparators supported for the search parameter.
        , searchParameter_modifier :: [SearchModifierCode]
          -- ^ A modifier supported for the search parameter.
        , searchParameter_chain :: [Xsd.XsdString]
          -- ^ Contains the names of any search parameters which may be 
          --   chained to the containing search parameter. Chained 
          --   parameters may be added to search parameters of type 
          --   reference and specify that resources will only be returned 
          --   if they contain a reference to a resource which matches the 
          --   chained parameter value. Values for this field should be 
          --   drawn from SearchParameter.code for a parameter on the 
          --   target resource type.
        , searchParameter_component :: [SearchParameterComponent]
          -- ^ Used to define the parts of a composite search parameter.
        }
        deriving (Eq,Show)
              
 
-- | A search parameter that defines a named search item that 
--   can be used to search/filter on a resource.
data SearchParameterComponent = SearchParameterComponent
        { searchParameterComponent_id :: Maybe String_primitive
        , searchParameterComponent_extension :: [Extension]
          -- ^ May be used to represent additional information that is not 
          --   part of the basic definition of the element. To make the 
          --   use of extensions safe and manageable, there is a strict 
          --   set of governance applied to the definition and use of 
          --   extensions. Though any implementer can define an extension, 
          --   there is a set of requirements that SHALL be met as part of 
          --   the definition of the extension.
        , searchParameterComponent_modifierExtension :: [Extension]
          -- ^ May be used to represent additional information that is not 
          --   part of the basic definition of the element and that 
          --   modifies the understanding of the element in which it is 
          --   contained and/or the understanding of the containing 
          --   element's descendants. Usually modifier elements provide 
          --   negation or qualification. To make the use of extensions 
          --   safe and manageable, there is a strict set of governance 
          --   applied to the definition and use of extensions. Though any 
          --   implementer can define an extension, there is a set of 
          --   requirements that SHALL be met as part of the definition of 
          --   the extension. Applications processing a resource are 
          --   required to check for modifier extensions. Modifier 
          --   extensions SHALL NOT change the meaning of any elements on 
          --   Resource or DomainResource (including cannot change the 
          --   meaning of modifierExtension itself).
        , searchParameterComponent_definition :: Canonical
          -- ^ The definition of the search parameter that describes this 
          --   part.
        , searchParameterComponent_expression :: Xsd.XsdString
          -- ^ A sub-expression that defines how to extract values for 
          --   this component from the output of the main 
          --   SearchParameter.expression.
        }
        deriving (Eq,Show)

data XPathUsageType_list
    = XPathUsageType_list_Normal
      -- ^ Normal
    | XPathUsageType_list_Phonetic
      -- ^ Phonetic
    | XPathUsageType_list_Nearby
      -- ^ Nearby
    | XPathUsageType_list_Distance
      -- ^ Distance
    | XPathUsageType_list_Other
      -- ^ Other
    deriving (Eq,Show,Enum)

data SearchModifierCode_list
    = SearchModifierCode_list_Missing
      -- ^ Missing
    | SearchModifierCode_list_Exact
      -- ^ Exact
    | SearchModifierCode_list_Contains
      -- ^ Contains
    | SearchModifierCode_list_Not
      -- ^ Not
    | SearchModifierCode_list_Text
      -- ^ Text
    | SearchModifierCode_list_In
      -- ^ In
    | SearchModifierCode_list_Not_in
      -- ^ Not In
    | SearchModifierCode_list_Below
      -- ^ Below
    | SearchModifierCode_list_Above
      -- ^ Above
    | SearchModifierCode_list_Type
      -- ^ Type
    | SearchModifierCode_list_Identifier
      -- ^ Identifier
    | SearchModifierCode_list_OfType
      -- ^ Of Type
    deriving (Eq,Show,Enum)
 
data SearchModifierCode = SearchModifierCode
        { searchModifierCode_id :: Maybe String_primitive
        , searchModifierCode_value :: Maybe SearchModifierCode_list
        , searchModifierCode_extension :: [Extension]
          -- ^ May be used to represent additional information that is not 
          --   part of the basic definition of the element. To make the 
          --   use of extensions safe and manageable, there is a strict 
          --   set of governance applied to the definition and use of 
          --   extensions. Though any implementer can define an extension, 
          --   there is a set of requirements that SHALL be met as part of 
          --   the definition of the extension.
        }
        deriving (Eq,Show)

data SearchComparator_list
    = SearchComparator_list_Eq
      -- ^ Equals
    | SearchComparator_list_Ne
      -- ^ Not Equals
    | SearchComparator_list_Gt
      -- ^ Greater Than
    | SearchComparator_list_Lt
      -- ^ Less Than
    | SearchComparator_list_Ge
      -- ^ Greater or Equals
    | SearchComparator_list_Le
      -- ^ Less of Equal
    | SearchComparator_list_Sa
      -- ^ Starts After
    | SearchComparator_list_Eb
      -- ^ Ends Before
    | SearchComparator_list_Ap
      -- ^ Approximately
    deriving (Eq,Show,Enum)
 
data SearchComparator = SearchComparator
        { searchComparator_id :: Maybe String_primitive
        , searchComparator_value :: Maybe SearchComparator_list
        , searchComparator_extension :: [Extension]
          -- ^ May be used to represent additional information that is not 
          --   part of the basic definition of the element. To make the 
          --   use of extensions safe and manageable, there is a strict 
          --   set of governance applied to the definition and use of 
          --   extensions. Though any implementer can define an extension, 
          --   there is a set of requirements that SHALL be met as part of 
          --   the definition of the extension.
        }
        deriving (Eq,Show)
