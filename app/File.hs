module File
  ( parseCabalDotConfigPkgs, parseSeedFile )
  where

-- base
import Data.Char
  ( isSpace )
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

-- | Parse constrained packages from the @constraints@ stanza
-- of the @cabal.config@ file at the given filepath:
--
-- > constraints: pkg1 ==ver1,
-- >              pkg2 ==ver2,
-- > ...
--
-- This function disregards all other contents of the @cabal.config@ package.
parseCabalDotConfigPkgs :: FilePath -> IO PkgSpecs
parseCabalDotConfigPkgs fp = do
  ls <-  filter ( not . isCommentLine . Text.strip )
      .  Text.lines
     <$> Text.readFile fp
  return $ outsideStanza Map.empty ls
  where
    outsideStanza :: PkgSpecs -> [Text] -> PkgSpecs
    outsideStanza pkgs []
      = pkgs
    outsideStanza pkgs (l:ls)
      | Just rest <- Text.stripPrefix "constraints:" l
      = inConstraintsStanza (pkgs `addPkgFromLine` rest) ls
      | otherwise
      = outsideStanza pkgs ls

    inConstraintsStanza :: PkgSpecs -> [Text] -> PkgSpecs
    inConstraintsStanza pkgs []
      = pkgs
    inConstraintsStanza pkgs (l:ls)
      | let (ws, rest) = Text.span isSpace l
      , not $ Text.null ws
      = inConstraintsStanza (pkgs `addPkgFromLine` rest) ls
      | otherwise
      = outsideStanza pkgs (l:ls)

    addPkgFromLine :: PkgSpecs -> Text -> PkgSpecs
    addPkgFromLine pkgs l =
      let (pkgName, pkgSpec) = parseCabalDotConfigLine l
      in  Map.insert pkgName pkgSpec pkgs

parseCabalDotConfigLine :: Text -> (PkgName, PkgSpec)
parseCabalDotConfigLine
  = parsePkgSpec "'cabal.config' file"
  . Text.dropAround ((==) ',')
      -- drop commas

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
  ls <-  filter ( not . isCommentLine )
      .  map Text.strip
      .  Text.lines
     <$> Text.readFile fp
  let
    (pkgs, allowNewers) = partitionEithers $ map parseSeedFileLine ls
  return ( Map.fromList pkgs, mconcat allowNewers )
  where

isCommentLine :: Text -> Bool
isCommentLine l
    =  Text.null l
    || Text.isPrefixOf "--" l

parseSeedFileLine :: Text -> Either (PkgName, PkgSpec) AllowNewer
parseSeedFileLine l
  | Just an <- Text.stripPrefix "allow-newer:" l
  = Right $ parseAllowNewer an
  | otherwise
  = Left $ parsePkgSpec "seed file" l

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

parsePkgSpec :: String -> Text -> (PkgName, PkgSpec)
parsePkgSpec what l =
  case Text.words l of
    pkg:rest
      | validPackageName pkg
      -> ( PkgName pkg, parseSpec Map.empty rest )
    _ -> error $ "Invalid package in " <> what <> ": " <> Text.unpack l
  where
    parseSpec :: Map Text Bool -> [Text] -> PkgSpec
    parseSpec flags []
      = PkgSpec { psConstraints = Nothing
                , psFlags       = FlagSpec flags }
    parseSpec flags (w:ws)
      | Just (s,f) <- Text.uncons w
      , s == '+' || s == '-'
      = parseSpec (Map.insert f (s == '+') flags) ws
      | otherwise
      = PkgSpec { psConstraints = Just $ Constraints (Text.unwords (w:ws))
                , psFlags       = FlagSpec flags }
