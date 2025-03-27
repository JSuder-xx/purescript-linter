module Data.JsonSchema
  ( JsonSchema
  , addDescription
  , arrayOf
  , boolean
  , integer
  , json
  , maybe
  , null
  , patternPropertiesObject
  , string
  , object
  , object'
  ) where

import Prelude

-- | This is a _VERY_ simple Api for DRY'ing up creation of JsonSchemas which took about 60 minutes to author. As it stands this Api is sufficient for this project.
-- |
-- | Ideas that were considered but skipped
-- | - Phantom types to declare the kind of value of the schema to control what values might be allowed for defaulting or to ensure that only array schemas could set a minItems / maxItems.
-- | - Using type classes to automatically derive schemas for types. The problem is that providing descriptions and other rich metadata requires much more information that is encoded in the PureScript type system.
-- |   Things like min and max, defaults, etc. are difficult to encode. It could be possible to use type class machinery as follows
-- |   ```purescript
-- |   -- helper defined in library
-- |   -- I've built a decent number of row-TYPE-to-record-VALUE style type classes at work. I've never had to try to join multiple sets but I think it might be do-able.
-- |   jsonSchemaOf :: forall @row @descriptionsRow @defaultsRow.
-- |     JsonSchema row @descriptionsRow @defaultsRow schemaRow
-- |     => JsonSchema
-- |
-- |   -- then users would define the types
-- |   type ConfigRow =
-- |     ( shouldBlahBlah :: Boolean
-- |     , maxBleeBlee :: Int
-- |     )
-- |
-- |   type ConfigDescriptionsRow =
-- |     ( shouldBlahBlah :: "Indicates whether..."
-- |     , maxBleeBlee :: "The maximum number of blee blees..."
-- |     )
-- |
-- |   type ConfigDefaultsRow =
-- |     ( shouldBlahBlah :: false
-- |     , maxBleeBlee :: 7
-- |     )
-- |
-- |   configSchema :: JsonSchema
-- |   configSchema = jsonSchemaOf @ConfigRow @ConfigDescriptionsRow @ConfigDefaultsRow
-- |   ```

import Data.Argonaut (class EncodeJson, Json, encodeJson, extend)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Type.Row.Homogeneous (class Homogeneous)

newtype JsonSchema = JsonSchema Json

instance EncodeJson JsonSchema where
  encodeJson (JsonSchema j) = j

json :: JsonSchema -> Json
json (JsonSchema j) = j

addDescription :: String -> JsonSchema -> JsonSchema
addDescription d (JsonSchema j) = JsonSchema $ extend (Tuple "description" $ encodeJson d) j

maybe :: JsonSchema -> JsonSchema
maybe justSchema = JsonSchema $ encodeJson { anyOf: [ justSchema, null ] }

null :: JsonSchema
null = simpleType "null"

integer :: JsonSchema
integer = simpleType "integer"

string :: JsonSchema
string = simpleType "string"

boolean :: JsonSchema
boolean = simpleType "boolean"

-- | Declare an object of properties found in the record with ALL properties required.
object :: forall r. Homogeneous r JsonSchema => Record r -> JsonSchema
object = object' { allRequired: true } <<< Object.fromHomogeneous

-- | Low level object helper. When `allRequired` then ALL properties in the given object are required else NONE are required.
object' :: { allRequired :: Boolean } -> Object JsonSchema -> JsonSchema
object' { allRequired } properties = JsonSchema
  if not allRequired then baseObject
  else extend (Tuple "required" $ encodeJson $ Object.keys properties) baseObject
  where
  baseObject = encodeJson
    { "type": "object"
    , additionalProperties: false
    , properties: map @Object json properties
    }

patternPropertiesObject :: forall r. Homogeneous r JsonSchema => Record r -> JsonSchema
patternPropertiesObject r = JsonSchema $ encodeJson
  { "type": "object"
  , patternProperties: Object.fromHomogeneous r
  }

arrayOf :: JsonSchema -> JsonSchema
arrayOf (JsonSchema items) = JsonSchema $ encodeJson { "type": "array", items }

simpleType :: String -> JsonSchema
simpleType type_ = JsonSchema $ encodeJson { "type": type_ }
