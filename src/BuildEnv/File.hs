{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  BuildEnv.File
-- Description :  Parse packages and units from files
--
-- This module implements the parsing of the two file formats supported
-- by @build-env@:
--
--  - SEED files, containing a list of seed units from which to compute
--    a build plan. See 'parseSeedFile'.
--
--  - @cabal.config@ files containing version constraints on packages.
--    See 'parseCabalDotConfigPkgs'.
module BuildEnv.File
  ( parseCabalDotConfigPkgs, parseSeedFile )
  where

-- base
import Data.Char
  ( isSpace )

-- containers
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-- text
import Data.Text
  ( Text )
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

-- build-env
import BuildEnv.CabalPlan

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
      | Just rest <- Text.strip <$> Text.stripPrefix "constraints:" l
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

-- | Parse a 'PkgName' and 'PkgSpec' from a line in a @cabal.config@ file.
--
-- Assumes whitespace has already been stripped.
parseCabalDotConfigLine :: Text -> (PkgName, PkgSpec)
parseCabalDotConfigLine txt
  | let (pkg, rest)
          = Text.break isSpace
          $ Text.dropAround (',' ==) -- drop commas
          $ txt
  , validPackageName pkg
  = ( PkgName pkg, parsePkgSpec rest )
  | otherwise
  = error $ "Invalid package in cabal.config file: " <> Text.unpack txt

-- NB: update the readme after changing the documentation below.

-- | Parse a seed file. Each line must either be:
--
--  - A Cabal unit, in the format @unit +flag1 -flag2 >= 0.1 && < 0.3@.
--
--    A unit can be of the form @pkgName@, @lib:pkgName@, @exe:pkgName@,
--    @pkgName:lib:compName@, ... as per Cabal component syntax.
--
--    The unit name must be followed by a space.
--
--    Flags and constraints are optional.
--    When both are present, flags must precede constraints.
--    Constraints must use valid Cabal constraint syntax.
--
--  - An allow-newer specification, e.g. @allow-newer: pkg1:pkg2,*:base,...@.
--    This is not allowed to span multiple lines.
--
-- Returns @(units, allowNewer)@.
parseSeedFile :: FilePath -> IO (UnitSpecs, AllowNewer)
parseSeedFile fp = do
  ls <-  filter ( not . isCommentLine )
      .  map Text.strip
      .  Text.lines
     <$> Text.readFile fp
  return $ go Map.empty mempty ls

  where
    go :: UnitSpecs -> AllowNewer -> [Text] -> (UnitSpecs, AllowNewer)
    go units ans [] = (units, ans)
    go units ans (l:ls)
      | Just an <- Text.stripPrefix "allow-newer:" l
      = go units (ans <> parseAllowNewer an) ls
      | let (pkgTyComp, rest) = Text.break isSpace l
      , Just (pkgName, comp) <- parsePkgComponent pkgTyComp
      , let spec = parsePkgSpec rest
            thisUnit = Map.singleton pkgName
              (Remote, spec, Set.singleton comp)
                -- we assume units in a seed file
                -- don't refer to local packages
      = go (units `unionUnitSpecsCombining` thisUnit) ans ls
      | otherwise
      = error $ "Invalid package in seed file : " <> Text.unpack l

isCommentLine :: Text -> Bool
isCommentLine l
    =  Text.null l
    || Text.isPrefixOf "--" l

parseAllowNewer :: Text -> AllowNewer
parseAllowNewer l =
  AllowNewer $ Set.fromList $ map parseOneAllowNewer (Text.splitOn "," l)
  where
    parseOneAllowNewer t
      | (Text.strip -> a, Text.strip . Text.drop 1 -> b) <- Text.breakOn ":" t
      , a == "*" || validPackageName a
      , b == "*" || validPackageName b
      = (a,b)
      | otherwise
      = error $ "Invalid allow-newer syntax in seed file: " <> Text.unpack t

