import Prelude

import Data.Set as S
import Data.Text.Prettyprint.Doc
  
import AvroPath (decodeSchema, avroToTypescript, TsTypeState(..), formatTs)

main :: IO ()
main = do
  decodedSchema <- decodeSchema "idl-output/A.avsc"
  case decodedSchema of
    Left err -> putStrLn err
    Right schema ->
      let
        (TsTypeState types) = snd $ avroToTypescript schema
        allDocs = vcat $ fmap formatTs (S.toList types)
      in
        -- writeFile "tsoutput/index.ts" $ show allDocs
        putStrLn $ show allDocs

