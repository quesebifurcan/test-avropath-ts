{-# LANGUAGE OverloadedStrings #-}

module AvroPath where

import Prelude

import Data.Avro.Schema
import Data.List.NonEmpty (NonEmpty( (:|) ))
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Text.Pretty.Simple (pPrint)
import qualified Data.Aeson as Aeson
import qualified Data.Avro.JSON as AJ
import qualified Data.Avro.Schema as Schema
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T

-- :cmd return $ Prelude.unlines [":reload", ":main"]
-- DONE: type annotations with typescript types
-- TODO: simple filters
--   filter(filterArgs: string) {
--     return new Array_B(`${this.jsonpath}[${filterArgs}]`)
--   }
-- TODO: min, max and avg for Array_TsInt
-- get min() {
--   return new TsInt(`${this.jsonpath}.min()`)
-- }
-- get max() {
--   return new TsInt(`${this.jsonpath}.max()`)
-- }
-- get average() {
--   return new TsInt(`${this.jsonpath}.average()`)
-- }
-- DONE: slice
-- TODO: freeform
-- TODO: unions
-- DONE: tagged types to enforce equality
-- DONE: skip extractDerivables
-- TODO: generic array? Only array directly above a record needs special treatment
-- function check<T>(a: T, b: T): null { return null }
-- let t2 = check(t0, t0)

arrayPrefix = "Array_"
indentationOffset = 2

decodeSchema :: FilePath -> IO (Either String Schema.Schema)
decodeSchema p = Aeson.eitherDecode <$> LBS.readFile p

data TsType
  = TsRecord TsRecordType
  | TsArray TsArrayType
  | TsInt
  | TsFloat
  | TsString
  | TsEnum TsEnumType
  | TsUnion (NonEmpty TsType)
  | TsNull
  | TsNamedType T.Text
  | TsPlaceholder T.Text -- TODO: unions
  deriving (Ord, Eq, Show)

data TsRecordField = TsRecordField {
  tsFieldName :: T.Text
  , tsFieldType :: TsType
  }
  deriving (Ord, Eq, Show)

data TsRecordType = TsRecordType {
  tsRecordName :: T.Text
  , tsRecordFields :: [TsRecordField]
  }
  deriving (Ord, Eq, Show)

data TsArrayType = TsArrayType {
  tsArrayName :: T.Text
  , tsArrayItems :: TsType
  }
  deriving (Ord, Eq, Show)

data TsEnumType = TsEnumType {
  tsEnumName :: T.Text,
  tsEnumSymbols :: [T.Text]
  }
  deriving (Ord, Eq, Show)

data TsTypeState = TsTypeState (S.Set TsType)
  deriving (Ord, Eq, Show)

instance Monoid TsTypeState where
  mempty = TsTypeState S.empty
  mappend (TsTypeState r) (TsTypeState r') =
    TsTypeState (S.union r r')

class TSFormat a where
  formatTs :: a -> (Doc String)

instance TSFormat TsType where
  formatTs (TsArray (TsArrayType _ t@(TsNamedType name))) =
    emptyDoc
  formatTs (TsArray a@(TsArrayType name items)) =
    tsClass name body
    where
        body = vcat [indexMethod a, sliceMethod a, mapMethod a]
  formatTs (TsRecord r@(TsRecordType name fields)) =
    tsClass name body
    where
      body = recordAt r
  formatTs (TsEnum (TsEnumType name symbols)) =
    tsClass name emptyDoc
    -- "export enum" <+> pretty name <+> withBraces symbolsDoc
    -- where
    --   symbolsDoc = vcat $ punctuate comma (fmap pretty symbols)
  formatTs TsInt = tsClass "TsInt" emptyDoc
  formatTs TsString = tsClass "TsString" emptyDoc
  formatTs TsFloat = tsClass "TsFloat" emptyDoc

toMapName typeName = T.append (unTN typeName) "__map__"

arrayifyKeys :: Schema.Type -> Maybe Schema.Type
arrayifyKeys r@(Schema.Record typeName _ _ _ _ fields) =
  Just r {
  name = TN $ toMapName typeName
  , fields = (fmap pluralizeField fields)
  }
  where
    pluralizeField f@(Schema.Field _ _ _ _ fieldType _) =
      f { fldType = Schema.Array fieldType }
arrayifyKeys a@(Schema.Array _) = Just a
arrayifyKeys _ = Nothing

getTsTypeName :: TsType -> T.Text
getTsTypeName (TsRecord (TsRecordType name _)) = name
getTsTypeName (TsArray (TsArrayType name items)) =
  T.append arrayPrefix (getTsTypeName items)
getTsTypeName (TsUnion options) =
  T.intercalate " | " (NE.toList $ NE.map getTsTypeName options)
getTsTypeName (TsNamedType x) = x
getTsTypeName (TsEnum (TsEnumType name _)) = name
getTsTypeName x = T.pack $ show x

getTsTypeSignature :: TsType -> Doc ann
getTsTypeSignature (TsRecord (TsRecordType name _)) = pretty name
getTsTypeSignature (TsArray (TsArrayType _ items)) =
  "Array" <> "<" <> getTsTypeSignature items <> ">"
getTsTypeSignature (TsUnion options) =
  cat $ punctuate pipe options'
  where
    options' = NE.toList $ NE.map getTsTypeSignature options
getTsTypeSignature (TsNamedType x) = pretty x
getTsTypeSignature (TsEnum (TsEnumType name _)) = pretty name
getTsTypeSignature TsString = "string"
getTsTypeSignature TsInt = "number"

avroToTypescript' :: [Type] -> ([TsType], [TsTypeState])
avroToTypescript' xs = unzip $ fmap avroToTypescript xs

avroToTypescript :: Type -> (TsType, TsTypeState)
avroToTypescript (Schema.Record typeName _ _ _ _ fields) =
  (result, update)
  where
    avroFieldTypes = fmap fldType fields
    (tsFieldTypes, newTypes) = avroToTypescript' avroFieldTypes
    recordFields =
      zipWith TsRecordField (fmap fldName fields) tsFieldTypes
    result = TsRecord $ TsRecordType (unTN typeName) recordFields
    update = mconcat newTypes `mappend` TsTypeState (S.singleton result)
avroToTypescript (Schema.Array items) =
  (array, update)
  where
    update =
      newTypes `mappend`
      a `mappend`
      TsTypeState (S.singleton array)
    array = TsArray $ TsArrayType tsArrayName tsItemType
    (tsItemType, a) = avroToTypescript items
    tsArrayName = T.append arrayPrefix (getTsTypeName tsItemType)
    newTypes =
      case fmap avroToTypescript $ arrayifyKeys items of
        Just (_, newTypes') -> newTypes' -- TODO: correct?
        Nothing             -> mempty
avroToTypescript (Schema.Enum name _ _ _ symbols _) =
  (t, TsTypeState (S.singleton t))
  where
    t = TsEnum $ TsEnumType (unTN name) symbols
avroToTypescript (Schema.NamedType x) =
  (TsNamedType (unTN x), TsTypeState S.empty)
avroToTypescript (Schema.Union _ _) = (TsPlaceholder "union", mempty)
avroToTypescript Schema.Int = (TsInt, TsTypeState (S.singleton TsInt))
avroToTypescript Schema.Double = (TsFloat, TsTypeState (S.singleton TsFloat))
avroToTypescript Schema.Long = (TsInt, TsTypeState (S.singleton TsInt))
avroToTypescript Schema.String = (TsString, TsTypeState (S.singleton TsString))
avroToTypescript x = error $ show x

newInstance (TsRecordField name fieldType) =
  pretty name <>
  colon <+>
  "new" <+>
  pretty fieldValue <>
  lparen <>
  "`" <>
  "${this.jsonpath}" <>
  dot <>
  pretty name <>
  "`" <>
  rparen
  where
    fieldValue = getTsTypeName fieldType

getter name = "get" <+> name <> lparen <> rparen

recordAt r@(TsRecordType name fields) =
  getter "at" <+> withBraces body
  where
    body = "return" <+> withBraces (vcat fieldsExpr)
    fieldsExpr = punctuate comma (fmap newInstance fields)

tsClassDeclaration :: Doc ann
tsClassDeclaration = "export class"

jsonpathTypeDecl :: Doc ann
jsonpathTypeDecl = "jsonpath: string"

jsonpathAssignment :: Doc ann
jsonpathAssignment = "this.jsonpath = jsonpath"

typeDeclaration :: T.Text -> T.Text -> Doc ann
typeDeclaration name type' = pretty name <> colon <> pretty type'

constructorAssignment :: T.Text -> Doc ann
constructorAssignment t =
  "this" <> dot <> t' <+> equals <+> t' <> semi
  where t' = pretty t

withBraces :: Doc ann -> Doc ann
withBraces body =
  vcat [lbrace, indent indentationOffset body, rbrace]

constructor :: Doc ann
constructor =
  "constructor" <>
  parens jsonpathTypeDecl <+>
  withBraces (jsonpathAssignment <> semi)

typeTag name =
  "private tag" <> colon <+> dquote <> pretty name <> dquote <> semi

tsClass :: T.Text -> Doc ann -> Doc ann
tsClass name content =
  tsClassDeclaration <+> pretty name <+> withBraces body
  where
    body = vcat [
      typeTag name
      , jsonpathTypeDecl <> semi
      , constructor
      , content
      ]

withBackticks :: Doc ann -> Doc ann
withBackticks s = "`" <> s <> "`"

jsFormatString s = "$" <> lbrace <> s <> rbrace

indexMethod (TsArrayType name items) =
  "index(i: number)" <+>
  withBraces body
  where
    body =
      "return new" <+>
      pretty (getTsTypeName items) <>
      lparen <>
      withBackticks (jsFormatString "this.jsonpath" <>
                     lbracket <>
                     jsFormatString "i" <>
                     rbracket) <>
      rparen <>
      semi

sliceMethod (TsArrayType name items) =
  "slice(start: number, end: number)" <+>
  withBraces body
  where
    body =
      "return new" <+>
      pretty name <>
      lparen <>
      withBackticks (jsFormatString "this.jsonpath" <>
                     lbracket <>
                     jsFormatString "start" <>
                     colon <>
                     jsFormatString "end" <>
                     rbracket) <>
      rparen <>
      semi

mapMethod (TsArrayType _ (TsRecord (TsRecordType name fields))) =
  getter "map" <+> withBraces body
  where
    body = "return" <+> withBraces properties <> semi
      where
        properties =
          vcat $
          punctuate comma $
          fmap (mkMappingEntry . tsFieldName) fields
        templateString =
          jsFormatString "this.jsonpath" <>
          lbracket <>
          "*"<>
          rbracket
        mkMappingEntry fieldName =
          pretty fieldName <>
          colon <+>
          "new" <+>
          pretty name <>
          "__map__" <>
          lparen <>
          withBackticks templateString <>
          rparen <>
          dot <>
          "at" <>
          dot <>
          pretty fieldName
mapMethod _ = emptyDoc
