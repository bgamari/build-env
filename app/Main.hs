module Main where

import Options.Applicative

import Build
import BuildOne
import CabalPlan

data Opts = Opts { compiler :: Compiler
                 , outputDir :: FilePath
                 , packages :: [PkgSpec]
                 }

options :: Parser Opts
options = Opts
    <$> optCompiler
    <*> option str (short 'o' <> long "output" <> help "output directory")
    <*> some (argument pkgSpec (help "output directory"))
  where
    optCompiler :: Parser Compiler
    optCompiler =
        Compiler
          <$> option str (long "ghc" <> value "ghc" <> help "ghc path")
          <*> option str (long "ghc-pkg" <> value "ghc-pkg" <> help "ghc path") 

    pkgSpec :: ReadM PkgSpec
    pkgSpec = PkgSpec <$> (PkgName <$> str) <*> pure Nothing <*> pure mempty

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> options) mempty
    plan <- computePlan (packages opts)
    buildPlan (outputDir opts) (compiler opts) plan
