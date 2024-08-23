{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}

module BuildEnv.Path
  ( SymbolicPath, RelativePath, AbsolutePath
  , FileOrDir(..)
  , CWD, Pkg, PkgDb, Project
  , Tmp, Logs, Fetch, Prefix, Install

  , sameDirectory
  , mkSymbolicPath
  , mkRelativePath
  , mkAbsolutePath

  , interpretSymbolicPath
  , getSymbolicPath
  , getAbsolutePath
  , relativeSymbolicPath
  , absoluteSymbolicPath
  , makeAbsolute

  , (<.>), (</>)
  )
  where

-- base
import Data.Kind
  ( Type )

-- directory
import qualified System.Directory as Directory

-- filepath
import qualified System.FilePath as FilePath

--------------------------------------------------------------------------------

-- | A type-level symbolic name, to an abstract file or directory
-- (e.g. the Cabal package directory).
data FileOrDir
  = -- | A file (with no further information).
    File
  | -- | The abstract name of a directory or category of directories,
    -- e.g. the package directory or a source directory.
    Dir Type

-- | Is this symbolic path allowed to be absolute, or must it be relative?
data AllowAbsolute
  = -- | The path may be absolute, or it may be relative.
    AllowAbsolute
  | -- | The path must be relative.
    OnlyRelative

-- | A symbolic path, possibly relative to an abstract location specified
-- by the @from@ type parameter.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
type SymbolicPathX :: AllowAbsolute -> Type -> FileOrDir -> Type
newtype SymbolicPathX allowAbsolute from to = SymbolicPath FilePath
  deriving newtype Show
type role SymbolicPathX nominal nominal nominal

-- | A symbolic relative path, relative to an abstract location specified
-- by the @from@ type parameter.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
type RelativePath = SymbolicPathX 'OnlyRelative

-- | A path which is either absolute or relative to the given abstract*
-- location specified by the @from@ type parameter.
--
-- They are *symbolic*, which means we cannot perform any 'IO'
-- until we interpret them (using e.g. 'interpretSymbolicPath').
type SymbolicPath = SymbolicPathX 'AllowAbsolute

-- | An absolute path, or a reference to a path from the PATH environment variable.
type AbsolutePath :: FileOrDir -> Type
newtype AbsolutePath to = AbsolutePath ( forall from. SymbolicPath from to )
instance Show ( AbsolutePath to ) where
  show ( AbsolutePath fp ) = show fp

--------------------------------------------------------------------------------

-- | Abstract directory: current working directory.
data CWD

-- | Abstract directory: project root for 'build-env' commands.
data Project

-- | Abstract directory: package directory (e.g. a directory containing the @.cabal@ file).
data Pkg

-- | Abstract directory: package database directory (e.g. a directory containing a @package.conf@ file).
data PkgDb

-- | Abstract temporary directory.
data Tmp

-- | Abstract directory for logs.
data Logs

-- | Abstract directory: fetched sources directory.
data Fetch

-- | Abstract directory: prefix.
data Prefix

-- | Abstract directory: installation directory.
data Install

--------------------------------------------------------------------------------

mkSymbolicPath :: FilePath -> SymbolicPath from to
mkSymbolicPath = SymbolicPath

mkRelativePath :: FilePath -> RelativePath from to
mkRelativePath = SymbolicPath

mkAbsolutePath :: FilePath -> AbsolutePath to
mkAbsolutePath fp = AbsolutePath ( mkSymbolicPath fp )

sameDirectory :: SymbolicPathX allowAbsolute from to
sameDirectory = SymbolicPath "."

getSymbolicPath :: SymbolicPathX allowAbsolute from to -> FilePath
getSymbolicPath ( SymbolicPath p ) = p

getAbsolutePath :: AbsolutePath to -> FilePath
getAbsolutePath ( AbsolutePath p ) = getSymbolicPath p

relativeSymbolicPath :: RelativePath from to -> SymbolicPath from to
relativeSymbolicPath ( SymbolicPath p ) = SymbolicPath p

absoluteSymbolicPath :: AbsolutePath to -> SymbolicPath from to
absoluteSymbolicPath ( AbsolutePath p ) = p

-- | Interpret a symbolic path with respect to the given directory.
--
-- Use this function before directly interacting with the file system in order
-- to take into account a working directory argument.
interpretSymbolicPath :: SymbolicPath CWD ( Dir dir ) -> SymbolicPathX allowAbsolute dir to -> FilePath
interpretSymbolicPath ( SymbolicPath workDir ) ( SymbolicPath p ) =
  if workDir == "."
  then p -- NB: this just avoids creating paths of the form "./././blah".
  else workDir </> p
          -- Note that this properly handles an absolute symbolic path,
          -- because if @q@ is absolute, then @p </> q = q@.

-- | Make the given 'SymbolicPath' absolute.
makeAbsolute :: SymbolicPath CWD ( Dir dir ) -> SymbolicPath dir to -> IO ( AbsolutePath to )
makeAbsolute workDir path =
  mkAbsolutePath <$> Directory.makeAbsolute ( interpretSymbolicPath workDir path )

-------------------------------------------------------------------------------

-- * Composition

-------------------------------------------------------------------------------

infixr 7 <.>

-- | Types that support 'System.FilePath.<.>'.
class FileLike p where
  -- | Like 'System.FilePath.<.>', but also supporting symbolic paths.
  (<.>) :: p -> String -> p

instance FileLike FilePath where
  (<.>) = (FilePath.<.>)

instance p ~ File => FileLike ( SymbolicPathX allowAbsolute dir p ) where
  SymbolicPath p <.> ext = SymbolicPath ( p <.> ext )

instance p ~ File => FileLike ( AbsolutePath p ) where
  p <.> ext = mkAbsolutePath ( getAbsolutePath p <.> ext )

infixr 5 </>

-- | Types that support 'System.FilePath.</>'.
class PathLike p q r | q r -> p, p q -> r where
  -- | Like 'System.FilePath.</>', but also supporting symbolic paths.
  (</>) :: p -> q -> r

instance ( q ~ FilePath ) => PathLike FilePath q FilePath where
  (</>) = (FilePath.</>)

-- | This instance ensures we don't accidentally discard a symbolic path
-- in a 'System.FilePath.</>' operation due to the second path being absolute.
--
-- (Recall that @a </> b = b@ whenever @b@ is absolute.)
instance
  (b1 ~ 'Dir b2, a3 ~ a1, c2 ~ c3)
  => PathLike
      ( SymbolicPathX allowAbsolute a1 b1 )
      ( SymbolicPathX midAbsolute   b2 c2 )
      ( SymbolicPathX allowAbsolute a3 c3 )
  where
  SymbolicPath p1 </> SymbolicPath p2 =
    if p1 == "."
    then SymbolicPath p2 -- NB: this just avoids creating paths of the form "./././blah".
    else SymbolicPath (p1 </> p2)

instance
  ( b1 ~ 'Dir b2, c2 ~ c3 )
  => PathLike
      ( AbsolutePath                 b1 )
      ( SymbolicPathX midAbsolute b2 c2 )
      ( AbsolutePath                 c3 )
  where
  AbsolutePath (SymbolicPath p1) </> SymbolicPath p2 =
    mkAbsolutePath (p1 </> p2)
