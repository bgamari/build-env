module File
  ( readCabalDotConfig, parseSeedFile )
  where

-- base
import Data.Either
  ( partitionEithers )

-- containers
import Data.Map
  ( Map )
import qualified Data.Map as Map

-- text
import Data.Text
  ( Text )
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

-- build-env
import CabalPlan

--------------------------------------------------------------------------------

-- | Read a @cabal.config@ file, filtering out lines we don't want
-- (such as @--with-compiler@).
--
-- If the file contains valid @cabal.project@ syntax (which this function
-- does /not/ check), the output will also be valid @cabal.project@ syntax.
readCabalDotConfig :: FilePath -> IO Text
readCabalDotConfig fp
    =  Text.unlines
    .  filter ( not . uselessLine . Text.strip )
    .  Text.lines
   <$> Text.readFile fp
  where
    uselessLine :: Text -> Bool
    uselessLine l
        =  Text.null l
        || Text.isPrefixOf "--" l
        || Text.isPrefixOf "with-compiler" l

-- | Parse a seed file. Each line must either be:
--
--  - A package, in the format @pkg +flag1 -flag2 >= 0.1 && < 0.3@.
--
--    Flags and constraints are optional.
--    When both are present, flags must precede constraints.
--    Constraints must use valid @cabal@ constraint syntax.
--
--  - An allow-newer specification, e.g. @allow-newer: pkg1:pkg2,*:base,...@.
--    This is not allowed to span multiple lines.
parseSeedFile :: FilePath -> IO (PkgSpecs, AllowNewer)
parseSeedFile fp = do
  ls <-  filter ( not . uselessLine )
      .  map Text.strip
      .  Text.lines
     <$> Text.readFile fp
  let
    (pkgs, allowNewers) = partitionEithers $ map parseSeedFileLine ls
  return ( Map.fromList pkgs, mconcat allowNewers )
  where
    uselessLine :: Text -> Bool
    uselessLine l
        =  Text.null l
        || Text.isPrefixOf "--" l

parseSeedFileLine :: Text -> Either (PkgName, PkgSpec) AllowNewer
parseSeedFileLine l
  | Just an <- Text.stripPrefix "allow-newer:" l
  = Right $ parseAllowNewer an
  | otherwise
  = Left $ parsePkgSpec l

parseAllowNewer :: Text -> AllowNewer
parseAllowNewer l =
  AllowNewer $ map parseOneAllowNewer (Text.splitOn "," l)
  where
    parseOneAllowNewer t
      | (Text.strip -> a, Text.strip . Text.drop 1 -> b) <- Text.breakOn ":" t
      , a == "*" || validPackageName a
      , b == "*" || validPackageName b
      = (a,b)
      | otherwise
      = error $ "Invalid allow-newer syntax in seed file: " <> Text.unpack t

parsePkgSpec :: Text -> (PkgName, PkgSpec)
parsePkgSpec l =
  case Text.words l of
    pkg:rest
      | validPackageName pkg
      -> ( PkgName pkg, parseSpec Map.empty rest )
    _ -> error $ "Invalid package in seed file:" <> Text.unpack l
  where
    parseSpec :: Map Text Bool -> [Text] -> PkgSpec
    parseSpec flags []
      = PkgSpec { psConstraints = Nothing
                , psFlags       = FlagSpec flags }
    parseSpec flags (w:ws)
      | Just (s,f) <- Text.uncons w
      , s == '+' || s == '-'
      , let b = if s == '+' then True else False
      = parseSpec (Map.insert f b flags) ws
      | otherwise
      = PkgSpec { psConstraints = Just $ Constraints (Text.unwords (w:ws))
                , psFlags       = FlagSpec flags }
