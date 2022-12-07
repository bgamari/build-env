{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Action where

-- base
import Control.Arrow
  ( (***) )
import Control.Monad
  ( void )
import Data.Char
  ( isAlpha, isAlphaNum )
import Data.Foldable
  ( toList )
import Data.Kind
  ( Type )
import Data.String
  ( IsString(..) )
import GHC.TypeNats
  ( Nat, type (+) )
import GHC.Stack
  ( HasCallStack )
import Unsafe.Coerce
  ( unsafeCoerce )

-- directory
import System.Directory

-- operational
import Control.Monad.Operational

-- text
import Data.Text
  ( Text )
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS

-- build-env
import Utils
  ( CallProcess(..) )
import qualified Utils as CP
  ( callProcess )

--------------------------------------------------------------------------------
-- The AST.

data Interp = Run | Script

data Code (i :: Interp) ( a :: Type ) where
  Str    :: Text -> Code i Text
  Arr    :: [Text] -> Code i [Text]
  (:<>:) :: Code i Text -> Code i Text -> Code i Text
  Var    :: Var i a -> Code i a

data FileOrDir
  = File | Dir

-- | A declarative description of an action, to be run in @IO@
-- or interpreted into a shell script.
data Action (i :: Interp) (r :: Type) where
  -- | Call a program with some arguments in a given directory.
  --
  -- Error out on non-zero exit code.
  CallProcess
    :: [Code i Text]
        -- ^ append this to PATH
    -> [(Code i Text, Code i Text)]
        -- ^ set these environment variables
    -> Code i Text
        -- ^ use this working directory
    -> Code i Text
        -- ^ process
    -> [Code i Text]
        -- ^ arguments
    -> Action i ()

  -- | Check whether the given file or directory exists.
  DoesItExist :: FileOrDir
              -> Code i Text
              -> Action i (Code i Bool)

  -- | Create a file at the given filepath with the given contents.
  CreateFile :: Code i Text -- ^ file path
             -> Text        -- ^ file contents
             -> Action i ()

  -- | Canonicalise a path.
  --Canonicalise :: FilePath -> Action FilePath

  -- | Run an action within a temporary directory.
  --WithTempDir :: TempDirPermanence
  --            -> Code Text -- ^ directory name template
  --            -> Fun FilePath
  --               -- ^ action to run
  --            -> Action ()

  -- | Define a variable.
  Let :: BashName -> Code i a -> Action i ( Var i a )

  -- | Write into a function result variable.
  WriteResVar :: Code i a ->
                 ResVar s i a ->
                 Action i (ResVar s i a)

  -- | Define a function
  Fun :: BashName ->
           -- ^ function name (for readability)
         Vec n BashName ->
           -- ^ value argument names (for readability)
         Vec m BashName ->
           -- ^ array argument names (for readability)
         FunTy i n m r ->
         Action i ( Fun i n m r )

  -- | Call a function.
  CallFun :: Fun i n m r ->
             Vec n (Var i Text) ->
             Vec m (Var i [Text]) ->
             Action i (Var i r)

  -- | Fold over an array
  Fold :: BashName
          -- ^ accumulator name (for readability)
       -> Var i [Text]
          -- ^ values to fold
       -> Fold   -- ^ how to combine the values
       -> Action i (Var i Text)

  If :: ResVar s i a ->
        Code i Bool ->
         -- ^ condition
       ( ResVar s i a -> Program (Action i) (ResVar s i a) ) ->
         -- ^ true branch
       ( ResVar s i a -> Program (Action i) (ResVar s i a) ) ->
         -- ^ false branch
         Action i (ResVar s i a)

data family Var (i :: Interp) (a :: Type)
newtype instance Var Run    a = VarValue a
newtype instance Var Script a = VarName BashName

type FunTy i n m r
  = forall s.
    Vec n (Var i Text) ->
    Vec m (Var i [Text]) ->
    ResVar s i r ->
    Program (Action i) (ResVar s i r)

data family Fun (i :: Interp) (n :: Nat) (m :: Nat) (r :: Type)
newtype instance Fun Run    n m r = FunValue ( FunTy Run n m r )
data instance Fun Script n m r =
  FunName
    BashName -- ^ function name
    BashName -- ^ result variable name

data family ResVar s (i :: Interp) (a :: Type)
data instance ResVar s Run a = InitResVar | RunResVar (Var Run a)
newtype instance ResVar s Script a = ResVar (Var Script a)

data Fold =
  BuildArgs
    Text -- ^ starting string
    Text -- ^ string to add before each argument

--------------------------------------------------------------------------------
-- Running in IO

runProgram :: Program (Action Run) a -> IO a
runProgram = interpretWithMonad runAction

eval :: Code Run a -> a
eval (Str a) = a
eval (Arr a) = a
eval (a :<>: b) = eval  a <> eval  b
eval (Var (VarValue v)) = v

runAction :: HasCallStack => Action Run a -> IO a
runAction = \case

  CallProcess addToPATH envVars cwd p args ->
    let cp = CP { cwd          = str cwd
                , extraPATH    = map str addToPATH
                , extraEnvVars = map (str *** str) envVars
                , prog         = str p
                , args         = map str args }
    in CP.callProcess cp

  DoesItExist fileOrDir f ->
    Var . VarValue <$>
      checker (Text.unpack $ eval f)
    where
      checker = case fileOrDir of
        File -> doesFileExist
        Dir  -> doesDirectoryExist

  CreateFile path contents ->
    Text.writeFile (str path) contents

  Let _ val -> return $ VarValue $ eval  val

  Fun _ _ _ fn -> return $ FunValue fn

  CallFun (FunValue f) strArgs arrArgs -> do
    RunResVar a <- runProgram $ f strArgs arrArgs InitResVar
    return $ a

  Fold _ (VarValue arr) f -> case f of
    BuildArgs s0 sep ->
      return $ VarValue $ s0 <> sep <> Text.intercalate sep arr

  If resVar cond tru fls ->
    if eval cond
    then runProgram $ tru resVar
    else runProgram $ fls resVar

  WriteResVar val _ ->
    return $ RunResVar $ VarValue $ eval val

  where
    str = Text.unpack . eval

--------------------------------------------------------------------------------
-- Interpreting as a shell script

data St
  = St { indentation :: Int
       , unique :: Unique }

newtype Unique = Unique Int

newtype ShM a = ShM { runShM :: WriterT [Text] (State St) a }
  deriving newtype ( Functor, Applicative, Monad )

evalShM :: ShM () -> Text
evalShM (ShM m)
  = Text.unlines
  $ snd
  $ ( `evalState` (St 0 (Unique 1)) )
  $ runWriterT m

writeLines :: [Text] -> ShM ()
writeLines ls = ShM do
  i <- indentation <$> lift get
  tell ( map (Text.replicate i " " <>) ls )

fresh :: ShM Unique
fresh = ShM $ lift do
  St i u@(Unique a) <- get
  put (St i (Unique (a+1)))
  return u

modifyIndent :: ( Int -> Int ) -> ShM a -> ShM a
modifyIndent f (ShM m) = ShM do
  St i u <- lift get
  lift $ put (St (f i) u)
  res <- m
  St _ v <- lift get
  lift $ put (St i v)
  return res

indent :: ShM a -> ShM a
indent = modifyIndent (+2)

noIndent :: ShM a -> ShM a
noIndent = modifyIndent (const 0)

splice :: Code Script a -> Text
splice (Str a) = around a
splice (Arr a) = Text.unwords (map around a)
splice (a :<>: b) = splice a <> splice b
splice (Var (VarName (BashName v))) = "\"${" <> v <> "}\""

around :: Text -> Text
around str = "\"" <> str <> "\""

shellScript :: Program (Action Script) a -> ShM a
shellScript = scriptLines . view
  where
    scriptLines :: ProgramView (Action Script) a -> ShM a
    scriptLines = \case
      Return a  ->
        return a
      mb :>>= f ->
        shellAction mb >>= shellScript . f

shellAction :: Action Script a -> ShM a
shellAction = \ case

  CallProcess addToPath envVars cwd p args -> do
    Unique u <- fresh
    let
      var = "exitCode_" <> Text.pack (show u)
      code =
        [ "( cd " <> splice cwd <> " ; \\" ]
        ++ mbUpdatePath
        ++ map mkEnvVar envVars
        ++
        [ "  " <> splice p <> " " <> Text.unwords (map splice args) <> " )"
        , var <> "=$?"
        , "if [ \"${" <> var <> "}\" -eq 0 ]"
        , "then true"
        , "else"
        , "  echo \"callProcess failed with non-zero exit code. Command:\""
        , "  echo \"> " <> ppr p <> " " <> Text.unwords (map ppr args) <> "\""
        , "  exit \"${" <> var <> "}\""
        , "fi" ]
    writeLines code
    where

      ppr :: Code Script a -> Text
      ppr (Str a) = a
      ppr (Arr a) = Text.unwords a
      ppr (a :<>: b) = ppr a <> ppr b
      ppr (Var (VarName (BashName v))) = "${" <> v <> "}"

      mbUpdatePath :: [Text]
      mbUpdatePath
        | null addToPath
        = []
        | otherwise
        = [ "  export PATH=$PATH:" <> Text.intercalate ":" (map splice addToPath) <> " ; \\" ]

      mkEnvVar :: (Code Script Text, Code Script Text) -> Text
      mkEnvVar (var,val) = "  export " <> splice var <> "=" <> splice val <> " ; \\"

  DoesItExist fileOrDir d -> do
    Unique u <- fresh
    let
      var = "exists_" <> Text.pack (show u)
      code =
        [ var <> "=false"
        , "if [ " <> f_or_d <> " " <> splice d <> " ]"
        , "then"
        , "  if [ -L "<> splice d <> " ]"
        , "  then true"
        , "  else"
        , "    " <> var <> "=true"
        , "  fi"
        , "fi" ]
    writeLines code
    return $ Var (VarName (BashName var))
      where
        f_or_d = case fileOrDir of
          File -> "-f"
          Dir  -> "-d"

  CreateFile fp contents -> do
    writeLines ["cat > " <> splice fp <> " << EOF"]
    noIndent $
      writeLines ( Text.lines contents ++ [ "EOF" ] )

  Let nm@(BashName var) val -> do
    writeLines [ var <> "=" <> varRHS val ]
    return $ VarName nm
    where
      varRHS :: Code Script a -> Text
      varRHS (Str a) = "\"" <> a <> "\""
      varRHS (Arr a) = "( " <> Text.unwords (map around a) <> " )"
      varRHS (a :<>: b) = varRHS a <> varRHS b
      varRHS (Var (VarName (BashName v))) = "\"${" <> v <> "}\""

  Fun funNm@(BashName funName) valNames arrNames fn -> do
    Unique i <- fresh
    let resVar = "res_" <> Text.pack (show i)
        res = BashName resVar
    writeLines [ funName <> "() {" ]
    indent do
      writeLines $
        zipWith mkArg [1..] (toList valNames ++ toList arrNames)
      void $
        shellScript (fn (fmap VarName valNames) (fmap VarName arrNames) (ResVar $ VarName res))
    writeLines [ "}" ]
    return $ FunName funNm res
      where
        mkArg :: Int -> BashName -> Text
        mkArg i (BashName arg) = "local -n " <> arg <> "=${" <> Text.pack (show i) <> "}"

  CallFun (FunName (BashName funName) res@(BashName resName)) strArgs arrArgs -> do
    writeLines [ resName <> "=\"\""
               , funName <> " " <> allVars ]
    return $ VarName res
    where
      varName :: Var Script a -> Text
      varName (VarName (BashName nm)) = nm
      allVars :: Text
      allVars = Text.unwords $ toList (fmap varName strArgs) ++ toList (fmap varName arrArgs)

  Fold accName@(BashName acc) (VarName (BashName arrName)) f -> do
    writeLines code
    return $ VarName accName
    where
      code = case f of
        BuildArgs s0 sep ->
          [ "" <> acc <> "=\"" <> s0 <> "\""
          , "for i in \"${!" <> arrName <> "[@]}\"; do"
          , "  " <> acc <> "=\"${" <> acc <> "}\"\"" <> sep <> "\"\"${" <> arrName <> "[$i]}\""
          , "done" ]

  If resVar cond tru fls -> do
    writeLines [ "if [ " <> splice cond <> " = \"true\" ]"
               , "then" ]
    void $ indent $ shellScript $ tru resVar
    writeLines [ "else" ]
    void $ indent $ shellScript $ fls resVar
    writeLines [ "fi" ]
    return resVar

  WriteResVar val resVar@(ResVar (VarName (BashName varNm))) -> do
    writeLines [ varNm <> "=" <> splice val ]
    return resVar

--------------------------------------------------------------------------------
-- Utilities.

-- | A valid @bash@ variable name: contains only alphanumeric identifiers
-- and underscores, and does not start with a number.
newtype BashName = BashName Text

instance IsString BashName where
  fromString str
    | c:cs <- str
    , isAlpha c || c == '_'
    , all (\ x -> isAlphaNum x || x == '_') cs
    = BashName ( Text.pack str )
    | otherwise
    = error $ "Invalid Bash variable name \"" <> str <> "\""

infixr 4 :.
data Vec (n :: Nat) (a :: Type) where
  Nil :: Vec 0 a
  (:.) :: a -> Vec n a -> Vec (1 + n) a

deriving stock instance Functor     (Vec n)
deriving stock instance Foldable    (Vec n)
deriving stock instance Traversable (Vec n)

--------------------------------------------------------------------------------
-- DSL and example

act :: Action i a -> Program (Action i) a
act = unsafeCoerce singleton

ifThenElse :: ResVar s i a ->
              Code i Bool ->
              ( ResVar s i a -> Program (Action i) (ResVar s i a ) ) ->
              ( ResVar s i a -> Program (Action i) (ResVar s i a ) ) ->
              Program (Action i) (ResVar s i a)
ifThenElse resVar c a b = act (If resVar c a b)

fun :: forall r i n m
    .  BashName ->
       Vec n BashName ->
       Vec m BashName ->
       FunTy i n m r ->
       Program (Action i) (Fun i n m r)
fun nm vals arrs f = act (Fun nm vals arrs f)

callFun :: Fun i n m r ->
           Vec n (Var i Text) ->
           Vec m (Var i [Text]) ->
           Program (Action i) (Var i r)
callFun fn vals arrs = act (CallFun fn vals arrs)

fold :: BashName -> Var i [Text] -> Fold -> Program (Action i) (Var i Text)
fold accNm arr foldFn = act (Fold accNm arr foldFn)

let' :: BashName -> Code i a -> Program (Action i) (Var i a)
let' nm val = act (Let nm val)

callProcess :: [Code i Text]
            -> [(Code i Text, Code i Text)]
            -> Code i Text
            -> Code i Text
            -> [Code i Text]
            -> Program (Action i) ()
callProcess pATH envVars cwd prog args =
  act (CallProcess pATH envVars cwd prog args)

doesItExist :: FileOrDir -> Code i Text -> Program (Action i) (Code i Bool)
doesItExist fOrD path = act (DoesItExist fOrD path)

createFile :: Code i Text -> Text -> Program (Action i) ()
createFile fp contents = act (CreateFile fp contents)

writeResVar :: Code i a -> ResVar s i a -> Program (Action i) (ResVar s i a)
writeResVar  val resVar = act (WriteResVar val resVar)

example :: forall (i :: Interp). Program (Action i) (Code i Text)
example = do
  myFun <- fun @Text "myFun" ("test" :. "prog" :. Nil) ("args" :. Nil) \ ( test :. prog :. _ ) ( arr :. _ ) resVar -> do
    res <- fold "accArgs" arr (BuildArgs "" " --package-id ")
    testExists <- doesItExist Dir (Var test)
    ifThenElse resVar testExists
        ( \ rv -> do
          callProcess [] [( Var prog :<>: Str "_datadir", Str "myDataDir" )] (Str ".")
               (Var prog) [ Str "ghc", Str "Setup.hs", Var res ]
          writeResVar (Str "branch1") rv
        )
        ( \ rv -> do
            callProcess [] [] (Str ".") (Str "echo") [ Str "dir didn't exist" ]
            writeResVar (Str "branch2") rv
        )
  progVar <- let' "myProg" (Str "echo")
  arrVar  <- let' "myArr" (Arr ["pkg1-123","pkg2-234","pkg3-456"])
  testVar <- let' "testVar" (Str "install2")
  funRes <- callFun myFun (testVar :. progVar :. Nil) (arrVar :. Nil)
  return (Var funRes)

putScript :: Program (Action Script) r -> IO ()
putScript prog = Text.putStrLn $ evalShM (void $ shellScript prog)

runScript :: Program (Action Run) (Code Run r) -> IO r
runScript prog = eval <$> runProgram prog


findSetupHs :: Program (Action i) (Fun i 1 0 Text)
findSetupHs = fun @Text "findSetupHs" ("root" :. Nil) Nil \ ( root :. _ ) _ rv0 -> do
  let setupHs  = Var root :<>: Str "/Setup.hs"
      setupLhs = Var root :<>: Str "/Setup.lhs"
  f1 <- doesItExist File setupHs
  ifThenElse rv0 f1 ( writeResVar setupHs ) \ rv1 -> do
    f2 <- doesItExist File setupLhs
    ifThenElse rv1 f2 ( writeResVar setupLhs ) \ rv2 -> do
      createFile setupHs $
        Text.unlines
          [ "import Distribution.Simple"
          , "main = defaultMain"
          ]
      writeResVar setupHs rv2
