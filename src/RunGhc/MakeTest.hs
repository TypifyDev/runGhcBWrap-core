{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module RunGhc.MakeTest where

import RunGhc.Locate
import RunGhc.LocatedModule
import RunGhc.SystemModule
import RunGhc.Executable
import RunGhc.MakeExe
import RunGhc.UserInput
import RunGhc.MakeTest.TypeSig
import RunGhc.MakeTest.HKTs
import RunGhc.MakeTest.FFI

import GHC.Generics
import Text.IStr
import Control.Monad (replicateM)
import Data.Typeable
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import Data.Default
import Data.Bifunctor
import Data.Text (Text, pack)
import System.Random (randomRIO)
import System.Exit
import Data.Char (toUpper)

-- new idea : standardized Main.hs that runs 2 sets of symbols

-- Could also do these script builders by Arity


--x = compareFunc "f" mkF

--mkExample fname =

-- For modeling user-input
data SourceCode = SourceCode
  { _sourceCode_code :: T.Text
  , _sourceCode_target :: FunctionName
  } deriving Generic
instance FromJSON SourceCode
instance ToJSON SourceCode

-- The scope of our computation the user can see
data CodeChallengeResult = CodeChallengeResult
  { _ccrRunGhc_exitCode :: ExitCode
  , _ccrRunGhc_stderr :: [String]
  , _ccrRunGhc_tests :: [Bool]
  } deriving Generic
instance FromJSON CodeChallengeResult
instance ToJSON CodeChallengeResult

class Reduce a where
  reduce :: a -> [Bool]

instance Reduce CodeChallengeResult where
  reduce = _ccrRunGhc_tests

instance Reduce TestSolutionResult where
  reduce tsol = fmap _success $ _testSolRunGhc_tests tsol

-- The scope of our computation the user can see
data TestSolutionResult = TestSolutionResult
  { _testSolRunGhc_exitCode :: ExitCode
  , _testSolRunGhc_stderr :: [String]
  , _testSolRunGhc_tests :: [TryCodeResult T.Text T.Text]
  } deriving Generic
instance FromJSON TestSolutionResult
instance ToJSON TestSolutionResult

-- A set of functions
data TypeInfo a b = TypeInfo
  { _typesIn :: a
  , _typeOut :: b
  }
type T a = Proxy a
type_ :: T a
type_ = Proxy

-- A neat hack to pass back the result like a typed-ffi
-- This is because we can import this in both the executable 
-- passed to runghcand also import in the program which
-- calls runghc. 
data TryCodeResult a b = TryCodeResult
  { _input :: a -- TODO: better typing
  , _output :: b -- TODO: better typing
  , _expectedOutput :: b
  , _success :: Bool
  } deriving Generic

instance Functor (TryCodeResult a) where
  fmap f tryCodeResult = TryCodeResult
    (_input tryCodeResult)
    (f $ _output tryCodeResult)
    (f $ _expectedOutput tryCodeResult)
    (_success tryCodeResult)

instance Bifunctor TryCodeResult where
  bimap f g (TryCodeResult inp out expected success) = 
    TryCodeResult (f inp) (g out) (g expected) success

instance (ToJSON a, ToJSON b) => ToJSON (TryCodeResult a b)
instance (FromJSON a, FromJSON b) => FromJSON (TryCodeResult a b)

toCompareExe
  :: LocatedUserModule -- User
  -> LocatedTestModule -- System: Compare Against
  -> LocatedTestModule -- Run Compare
  -> Executable
toCompareExe (LocatedUserModule locUser) (LocatedTestModule locTestLib) (LocatedTestModule locTestMain) =
  Executable
  { _main = locTestMain
  , _library = [locUser, locTestLib]
  }

-- There's no core reason we need to have the names be the same
--
-- Also the only name we ever need to actually know (prior to runtime generation of the exe)
-- is the compilation target of the user's module
--
-- There is truly no issue with randomly generating names. This will also help us to
-- enforce that the user just cannot ever get the references  #{moduleName}.#{testFunction} or #{moduleName}.#{testData}
-- and in fact, we don't even know! only the compareFunc will for a sec!

-- A script definition that defers picking names until later, this is why it contains a function
newtype NoNameScript = NoNameScript
  { getNoNameScript
    :: ModuleName {-random name-}
    -> FunctionName {-random name-}
    -> VarName {-random name-}    
    -> Script
    -- ^ Should we instead make this Locatable a => a ?
  }

type VarName = T.Text

-- A script definition that defers picking names until later, this is why it contains a function
newtype TypedNoNameScript inputSlot outputSlot = TypedNoNameScript
  { getTypedNoNameScript
    :: ModuleName {-random name-}
    -> FunctionName {-random name-}
    -> VarName{-random name-}
    -> TypeInfo (T inputSlot) (T outputSlot)
    -> Script
    -- ^ Should we instead make this Locatable a => a ?
  }

data Purity = Pure | Impure

--- monadic MAKE
--- change: use compareFuncHKT instead of compareFuncTyped -> set to False
compareGenArityT1 :: Purity -> TypedNoNameScript a b -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT1 purity = compareFuncHKT (lift purity . lambdaT1)

compareGenArityT2 :: Purity -> TypedNoNameScript (a, b) c -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT2 purity = compareFuncHKT (lift purity . lambdaT2)

compareGenArityT3 :: Purity -> TypedNoNameScript (a, b, c) d -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT3 purity = compareFuncHKT (lift purity . lambdaT3)

compareGenArityT4 :: Purity -> TypedNoNameScript (a, b, c, d) e -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT4 purity = compareFuncHKT (lift purity . lambdaT4)

compareGenArityT5 :: Purity -> TypedNoNameScript (a, b, c, d, e) f -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT5 purity = compareFuncHKT (lift purity . lambdaT5)

compareGenArityT6 :: Purity -> TypedNoNameScript (a, b, c, d, e, f) g -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT6 purity = compareFuncHKT (lift purity . lambdaT6)

compareGenArityT7 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g) h -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT7 purity = compareFuncHKT (lift purity . lambdaT7)

compareGenArityT8 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h) i -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT8 purity = compareFuncHKT (lift purity . lambdaT8)

compareGenArityT9 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i) j -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT9 purity = compareFuncHKT (lift purity . lambdaT9)

compareGenArityT10 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j) k -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT10 purity = compareFuncHKT (lift purity . lambdaT10)

compareGenArityT11 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k) l -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT11 purity = compareFuncHKT (lift purity . lambdaT11)

compareGenArityT12 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l) m -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT12 purity = compareFuncHKT (lift purity . lambdaT12)

compareGenArityT13 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m) n -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT13 purity = compareFuncHKT (lift purity . lambdaT13)

compareGenArityT14 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n) o -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT14 purity = compareFuncHKT (lift purity . lambdaT14)

compareGenArityT15 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) p -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT15 purity = compareFuncHKT (lift purity . lambdaT15)

compareGenArityT16 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) q -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT16 purity = compareFuncHKT (lift purity . lambdaT16)

compareGenArityT17 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) r -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT17 purity = compareFuncHKT (lift purity . lambdaT17)

compareGenArityT18 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) s -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT18 purity = compareFuncHKT (lift purity . lambdaT18)

compareGenArityT19 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) t -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT19 purity = compareFuncHKT (lift purity . lambdaT19)

compareGenArityT20 :: Purity -> TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) u -> SourceCode -> IO (Either T.Text Executable)
compareGenArityT20 purity = compareFuncHKT (lift purity . lambdaT20)



-- Is there a better way to say "This is from IO a but we know that a will be printed and decoded as FromJSON a
-- So all we need to do here is communicate what b is so that FromJSON works
-- and as i understand if we dont handle this properly then it will
-- likely error with "No instance for FromJSON (IO b)"
handleMonadicOutputType :: TypedNoNameScript a (m b) -> TypedNoNameScript a b
handleMonadicOutputType (TypedNoNameScript f) = TypedNoNameScript $ 
  \moduleName funcName varName (TypeInfo inputProxy outputProxy) ->
    -- We need to construct a TypeInfo with (m b) as output
    -- outputProxy :: Proxy b
    -- We need :: Proxy (m b)
    -- But we can't construct that from just Proxy b without knowing m
    f moduleName funcName varName (TypeInfo inputProxy (liftProxy outputProxy))
  where
    -- This doesn't work because we don't know what m is!
    liftProxy :: Proxy b -> Proxy (m b)
    liftProxy _ = Proxy

---- Shortforms
compareMonadicArityT1 :: TypedNoNameScript a            (m b) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT1  nnScript srcCode = compareGenArityT1  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT2 :: TypedNoNameScript (a, b)        (m c) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT2  nnScript srcCode = compareGenArityT2  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT3 :: TypedNoNameScript (a, b, c)      (m d) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT3  nnScript srcCode = compareGenArityT3  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT4 :: TypedNoNameScript (a, b, c, d)    (m e) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT4  nnScript srcCode = compareGenArityT4  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT5 :: TypedNoNameScript (a, b, c, d, e)  (m f) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT5  nnScript srcCode = compareGenArityT5  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT6 :: TypedNoNameScript (a, b, c, d, e, f) (m g) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT6  nnScript srcCode = compareGenArityT6  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT7 :: TypedNoNameScript (a, b, c, d, e, f, g) (m h) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT7  nnScript srcCode = compareGenArityT7  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT8 :: TypedNoNameScript (a, b, c, d, e, f, g, h) (m i) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT8  nnScript srcCode = compareGenArityT8  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT9 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i) (m j) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT9  nnScript srcCode = compareGenArityT9  Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT10 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j) (m k) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT10 nnScript srcCode = compareGenArityT10 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT11 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k) (m l) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT11 nnScript srcCode = compareGenArityT11 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT12 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l) (m m1) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT12 nnScript srcCode = compareGenArityT12 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT13 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m1) (m n) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT13 nnScript srcCode = compareGenArityT13 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT14 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m1, n) (m o) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT14 nnScript srcCode = compareGenArityT14 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT15 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m1, n, o) (m p) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT15 nnScript srcCode = compareGenArityT15 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT16 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m1, n, o, p) (m q) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT16 nnScript srcCode = compareGenArityT16 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT17 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m1, n, o, p, q) (m r) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT17 nnScript srcCode = compareGenArityT17 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT18 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m1, n, o, p, q, r) (m s) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT18 nnScript srcCode = compareGenArityT18 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT19 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m1, n, o, p, q, r, s) (m t) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT19 nnScript srcCode = compareGenArityT19 Impure (handleMonadicOutputType nnScript) srcCode

compareMonadicArityT20 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m1, n, o, p, q, r, s, t) (m u) -> SourceCode -> IO (Either T.Text Executable)
compareMonadicArityT20 nnScript srcCode = compareGenArityT20 Impure (handleMonadicOutputType nnScript) srcCode


comparePureArityT1 :: TypedNoNameScript a b -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT1 = compareGenArityT1 Pure

comparePureArityT2 :: TypedNoNameScript (a, b) c -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT2 = compareGenArityT2 Pure

comparePureArityT3 :: TypedNoNameScript (a, b, c) d -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT3 = compareGenArityT3 Pure

comparePureArityT4 :: TypedNoNameScript (a, b, c, d) e -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT4 = compareGenArityT4 Pure

comparePureArityT5 :: TypedNoNameScript (a, b, c, d, e) f -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT5 = compareGenArityT5 Pure

comparePureArityT6 :: TypedNoNameScript (a, b, c, d, e, f) g -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT6 = compareGenArityT6 Pure

comparePureArityT7 :: TypedNoNameScript (a, b, c, d, e, f, g) h -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT7 = compareGenArityT7 Pure

comparePureArityT8 :: TypedNoNameScript (a, b, c, d, e, f, g, h) i -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT8 = compareGenArityT8 Pure

comparePureArityT9 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i) j -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT9 = compareGenArityT9 Pure

comparePureArityT10 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j) k -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT10 = compareGenArityT10 Pure

comparePureArityT11 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k) l -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT11 = compareGenArityT11 Pure

comparePureArityT12 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l) m -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT12 = compareGenArityT12 Pure

comparePureArityT13 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m) n -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT13 = compareGenArityT13 Pure

comparePureArityT14 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n) o -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT14 = compareGenArityT14 Pure

comparePureArityT15 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) p -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT15 = compareGenArityT15 Pure

comparePureArityT16 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) q -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT16 = compareGenArityT16 Pure

comparePureArityT17 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) r -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT17 = compareGenArityT17 Pure

comparePureArityT18 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) s -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT18 = compareGenArityT18 Pure

comparePureArityT19 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) t -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT19 = compareGenArityT19 Pure

comparePureArityT20 :: TypedNoNameScript (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) u -> SourceCode -> IO (Either T.Text Executable)
comparePureArityT20 = compareGenArityT20 Pure






lambdaT1 :: FunctionName -> FnT a b
lambdaT1 fname = FnT . pack $ [istr| \a   -> #{fname} a|] 

lambdaT2 :: FunctionName -> FnT (a,b) c
lambdaT2 fname = FnT . pack $ [istr| \(a, b) -> #{fname} a b|]

lambdaT3 :: FunctionName -> FnT (a,b,c) d
lambdaT3 fname = FnT . pack $ [istr| \(a, b, c) -> #{fname} a b c|]

lambdaT4 :: FunctionName -> FnT (a,b,c,d) e
lambdaT4 fname = FnT . pack $ [istr| \(a, b, c, d) -> #{fname} a b c d|]

lambdaT5 :: FunctionName -> FnT (a,b,c,d,e) f
lambdaT5 fname = FnT . pack $ [istr| \(a, b, c, d, e) -> #{fname} a b c d e|]

lambdaT6 :: FunctionName -> FnT (a,b,c,d,e,f) g
lambdaT6 fname = FnT . pack $ [istr| \(a, b, c, d, e, f) -> #{fname} a b c d e f|]

lambdaT7 :: FunctionName -> FnT (a,b,c,d,e,f,g) h
lambdaT7 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g) -> #{fname} a b c d e f g|]

lambdaT8 :: FunctionName -> FnT (a,b,c,d,e,f,g,h) i
lambdaT8 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h) -> #{fname} a b c d e f g h|]

lambdaT9 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i) j
lambdaT9 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i) -> #{fname} a b c d e f g h i|]

lambdaT10 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j) k
lambdaT10 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j) -> #{fname} a b c d e f g h i j|]

lambdaT11 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k) l
lambdaT11 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k) -> #{fname} a b c d e f g h i j k|]

lambdaT12 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l) m
lambdaT12 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l) -> #{fname} a b c d e f g h i j k l|]

lambdaT13 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m) n
lambdaT13 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m) -> #{fname} a b c d e f g h i j k l m|]

lambdaT14 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n) o
lambdaT14 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> #{fname} a b c d e f g h i j k l m n|]

lambdaT15 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) p
lambdaT15 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> #{fname} a b c d e f g h i j k l m n o|]

lambdaT16 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) q
lambdaT16 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> #{fname} a b c d e f g h i j k l m n o p|]

lambdaT17 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) r
lambdaT17 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> #{fname} a b c d e f g h i j k l m n o p q|]

lambdaT18 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) s
lambdaT18 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> #{fname} a b c d e f g h i j k l m n o p q r|]

lambdaT19 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) t
lambdaT19 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> #{fname} a b c d e f g h i j k l m n o p q r s|]

lambdaT20 :: FunctionName -> FnT (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) u
lambdaT20 fname = FnT . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> #{fname} a b c d e f g h i j k l m n o p q r s t|]

 

 
-- hask> lift Pure f
-- take a pure function and make it monadic or just leave it as is
-- this comes into effect when writing the expression
lift :: Purity -> FnT a b -> FnHKT a b
lift arity = FnHKT arity . getFnT


-- Typed monadic actions of some arity

-- compareMonadicArityT1
--   :: TypedNoNameScript a b-> SourceCode -> IO (Either T.Text Executable)
-- compareMonadicArityT1 = compareFuncTyped thing

-- thing :: FunctionName -> FnHKT a b
-- thing = undefined


makeNames :: IO (T.Text,T.Text,T.Text)
makeNames = do
  testVarName <- ((<>) "t") <$> randomAlphaNumCamelCaseName 5
  -- Must start with a Capital
  moduleName <- ((<>) "M") <$> randomAlphaNumCamelCaseName 5
  testFuncName <- ((<>) "f") <$> randomAlphaNumCamelCaseName 5
  pure (testVarName, moduleName, testFuncName)

compareFuncHKT 
  :: forall a b.
     (FunctionName -> FnHKT a b)
  -- ^ Essentially, this means pick the arity of the problem
  -> TypedNoNameScript a b
  -- ^ The provided solution and input data as a haskell module
  -> SourceCode
  -- ^ User func name
  -> IO (Either T.Text Executable)
compareFuncHKT mkFn mkScript sourceCode = do -- (fnameUser, userScript) = do
  (testVarName, moduleName, testFuncName) <- makeNames
  
  let
    testLibScript :: Script
    testLibScript = (getTypedNoNameScript mkScript) (ModuleName moduleName) testFuncName testVarName (TypeInfo Proxy Proxy)
    locatedTestLib :: LocatedTestModule 
    locatedTestLib = LocatedTestModule $ locate [PathSegment moduleName] $ testLibScript

    settings_ = def
    userModule = mkUserModule' settings_ $ _sourceCode_code sourceCode
    -- TODO: pass ModuleName here
    mainModule = genMakeMainComparativeTypedTestScriptHKT
      mkFn
      testVarName
      (localImport $ getLocatedUserModule userModule, _sourceCode_target sourceCode)
      (localImport $ getLocatedTestModule locatedTestLib, testFuncName)
    locatedMainModule = LocatedTestModule $ locate [PathSegment "Main"] mainModule
    
  pure $ Right $ toCompareExe userModule locatedTestLib locatedMainModule


-- Link and call genMakeMainComparativeTestScript with args to customize it
compareFuncTyped
  :: forall a b.
     (FunctionName -> FnT a b)
  -- ^ Essentially, this means pick the arity of the problem
  -> TypedNoNameScript a b
  -- ^ The provided solution and input data as a haskell module
  -> SourceCode
  -- ^ User func name
  -> IO (Either T.Text Executable)
compareFuncTyped mkFn mkScript sourceCode = do -- (fnameUser, userScript) = do
  (testVarName, moduleName, testFuncName) <- makeNames  
  let
    testLibScript :: Script
    testLibScript = (getTypedNoNameScript mkScript) (ModuleName moduleName) testFuncName testVarName (TypeInfo Proxy Proxy)
    locatedTestLib :: LocatedTestModule 
    locatedTestLib = LocatedTestModule $ locate [PathSegment moduleName] $ testLibScript

    settings_ = def
    userModule = mkUserModule' settings_ $ _sourceCode_code sourceCode
    -- TODO: pass ModuleName here
    mainModule = genMakeMainComparativeTypedTestScript
      mkFn
      testVarName
      (localImport $ getLocatedUserModule userModule, _sourceCode_target sourceCode)
      (localImport $ getLocatedTestModule locatedTestLib, testFuncName)
    locatedMainModule = LocatedTestModule $ locate [PathSegment "Main"] mainModule
    
  pure $ Right $ toCompareExe userModule locatedTestLib locatedMainModule

-- Link and call genMakeMainComparativeTestScript with args to customize it
compareFunc
  :: (FunctionName -> Fn)
  -- ^ Essentially, this means pick the arity of the problem
  -> NoNameScript
  -- ^ The provided solution and input data as a haskell module
  -> SourceCode
  -- ^ User func name
  -> IO (Either T.Text Executable)
compareFunc mkFn mkScript sourceCode = do -- (fnameUser, userScript) = do
  (testVarName, moduleName, testFuncName) <- makeNames  
  let
    testLibScript :: Script
    testLibScript = (getNoNameScript mkScript) (ModuleName moduleName) testFuncName testVarName
    locatedTestLib :: LocatedTestModule 
    locatedTestLib = LocatedTestModule $ locate [PathSegment moduleName] $ testLibScript

    settings_ = def
    userModule = mkUserModule' settings_ $ _sourceCode_code sourceCode
    -- TODO: pass ModuleName here
    mainModule = genMakeMainComparativeTestScript
      mkFn
      testVarName
      (localImport $ getLocatedUserModule userModule, _sourceCode_target sourceCode)
      (localImport $ getLocatedTestModule locatedTestLib, testFuncName)
    locatedMainModule = LocatedTestModule $ locate [PathSegment "Main"] mainModule
    
  pure $ Right $ toCompareExe userModule locatedTestLib locatedMainModule

-- randomAlphaNumCamelCaseName :: Int -> IO T.Text
-- randomAlphaNumCamelCaseName numWords = do
--     words <- replicateM numWords randomWord
--     pure $ T.concat $ zipWith capitalize [0..] words
--   where
--     alphaNum = ['a'..'z'] ++ ['0'..'9']
--     randomWord :: IO T.Text
--     randomWord = do
--         len <- randomRIO (3, 8)
--         chars <- replicateM len $ do
--             idx <- randomRIO (0, length alphaNum - 1)
--             pure $ alphaNum !! idx
--         pure $ T.pack chars
--     capitalize :: Int -> T.Text -> T.Text
--     capitalize 0 w = w  -- first word stays lowercase
--     capitalize _ w = case T.uncons w of
--         Nothing -> w
--         Just (c, rest) -> T.cons (toUpper c) rest
randomAlphaNumCamelCaseName :: Int -> IO T.Text
randomAlphaNumCamelCaseName numWords = do
    words <- replicateM numWords randomWord
    pure $ T.concat $ zipWith capitalize [0..] words
  where
    alphaNum = ['a'..'z'] ++ ['0'..'9']
    randomWord :: IO T.Text
    randomWord = do
        len <- randomRIO (3, 8)
        chars <- replicateM len $ do
            idx <- randomRIO (0, length alphaNum - 1)
            pure $ alphaNum !! idx
        pure $ T.pack chars
    capitalize :: Int -> T.Text -> T.Text
    capitalize n w
      | n == 0    = w  -- first word stays lowercase
      | otherwise = case T.uncons w of
          Nothing -> w
          Just (c, rest) -> T.cons (toUpper c) rest
    
type TestScript = Script


-- Perhaps we can derive this func we pass as an arg?
genMakeMainComparativeTestScript
  :: (FunctionName -> Fn)
  -> VarName
  -> (Import, FunctionName) -- UserModuleName -- In theory, we could directly get this from the Import type
  -> (Import, FunctionName) -- TestModule     -- In theory, we could directly get this from the Import type
  -> TestScript
genMakeMainComparativeTestScript mkF testValueName (userModule, fnameUser) (testModule, fnameCompare) = Script $ pack [istr|
module Main where

import Control.Monad (mapM)
#{showImportLine userModule}
#{showImportLine testModule}



--userF, solutionF 
userF = #{getFn . mkF $ importName (getImportName userModule) <> "." <> fnameUser}
solutionF = #{getFn . mkF $ importName (getImportName testModule) <> "." <> fnameCompare}

main :: IO ()
main = do
  inputs <- #{importName $ getImportName testModule}.#{testValueName} -- inputs :: (ToJSON a, FromJSON a) => a 
  valsUser <- mapM userF inputs
  valsOurSolution <- mapM solutionF inputs
  print $ zipWith (==) valsUser valsOurSolution
|]

  
-- Perhaps we can derive this func we pass as an arg?
genMakeMainComparativeTypedTestScript
  :: (FunctionName -> FnT a b)
  -> VarName
  -> (Import, FunctionName) -- UserModuleName -- In theory, we could directly get this from the Import type
  -> (Import, FunctionName) -- TestModule     -- In theory, we could directly get this from the Import type
  -> TestScript
genMakeMainComparativeTypedTestScript mkF testValueName (userModule, fnameUser) (testModule, fnameCompare) = Script $ pack [istr|
module Main where

import Control.Monad (mapM)
#{showImportLine userModule}
#{showImportLine testModule}
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Aeson as Aeson
import RunGhc.MakeTest


--userF, solutionF -- should i use the proxies to assert the types here?
userF = #{getFnT . mkF $ importName (getImportName userModule) <> "." <> fnameUser}
solutionF = #{getFnT . mkF $ importName (getImportName testModule) <> "." <> fnameCompare}

main :: IO ()
main = do
  inputs <- #{importName $ getImportName testModule}.#{testValueName} -- inputs :: (ToJSON a, FromJSON a) => a 
  valsUser <- mapM userF inputs
  valsOurSolution <- mapM solutionF inputs
  -- TODO: should we add in a stripMetadata (for functions)?
  let x = zipWith3 (\outUser expec_ inp -> TryCodeResult inp outUser expec_ (outUser == expec_))  valsUser valsOurSolution inputs
  LBS.putStrLn $ Aeson.encode x 
|]

showFnHKTExpression :: FnHKT a b -> T.Text
showFnHKTExpression f = ensureIO (purity f) "pure $" <> (getFnHKTLambda f)
  where
    ensureIO :: Purity -> T.Text -> T.Text
    ensureIO b t = case b of
      Impure -> ""
      Pure -> t
  
  
genMakeMainComparativeTypedTestScriptHKT
  :: (FunctionName -> FnHKT a b)
  -> VarName
  -> (Import, FunctionName) -- UserModuleName -- In theory, we could directly get this from the Import type
  -> (Import, FunctionName) -- TestModule     -- In theory, we could directly get this from the Import type
  -> TestScript
genMakeMainComparativeTypedTestScriptHKT mkF testValueName (userModule, fnameUser) (testModule, fnameCompare) = Script $ pack [istr|
module Main where

import Control.Monad (mapM)
#{showImportLine userModule}
#{showImportLine testModule}
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Aeson as Aeson
import RunGhc.MakeTest

userF = #{userF_expr}
solutionF = #{solutionF_expr}

-- Compare
main :: IO ()
main = do
  inputs <- #{importName $ getImportName testModule}.#{testValueName}
  valsUser <- mapM userF inputs
  valsOurSolution <- mapM solutionF inputs
  let x = zipWith3 (\outUser expec_ inp -> TryCodeResult inp outUser expec_ (outUser == expec_))  valsUser valsOurSolution inputs
  LBS.putStrLn $ Aeson.encode x 
|]
  where 
    userF_expr = showFnHKTExpression . mkF $ importName (getImportName userModule) <> "." <> fnameUser
    solutionF_expr = showFnHKTExpression . mkF $ importName (getImportName testModule) <> "." <> fnameCompare
  
newtype ImportName = ImportName { importName :: T.Text }
getImportName :: Import -> ImportName
getImportName imp = ImportName $ fromMaybe (combine $ _import_pathSeg imp) (_import_qualifiedName imp)
  where
    combine = T.intercalate "." . fmap getPathSegment
-- get either qualified name or just the module name as path
-- keeping in mind that everything is technically a qualified import
-- where if we havent overrided the name it is just the Module's name itself
-- eg. import Data.Text ; f = Data.Text.pack

newtype TypedExpression a
  = TypedExpression T.Text
  
newtype Expression = Expression { getExpression :: T.Text }
-- To Enable a TypedExpression
  --where
    -- In theory, we could do something like this to typecheck a singular expression, before we include it in the script gen
    --mkExpression :: (a -> IO b) -> IO b
    --mkExpression f = $([e| 1 + 1 + g|] )

newtype TypedName a = TypedName { getTypedReference :: T.Text }

-- In theory we could do:
-- apply :: Function (a ': xs) -> TypedName a -> Function xs
--
-- and TypedName is gen'd by:
-- writeHaskellTopLevelVar :: VarName -> Expression -> TypedName 

data FnT a b = FnT { getFnT :: T.Text }

data FnHKT a b = FnHKT
  { purity :: Purity
  , getFnHKTLambda :: T.Text
  }



-- eg f1 = "userF = " <> liftWhenPure f1 <> lambdaString


newtype Fn = Fn { getFn :: T.Text }
chainFn :: Fn -> Fn -> Fn
chainFn f1 f2 = Fn $ getFn (wrap f1) <> " . " <> getFn (wrap f2)

chainFnT :: FnT a b -> FnT b c -> FnT a c
chainFnT f1 f2 = FnT $ getFnT (wrapT f1) <> " . " <> getFnT (wrapT f2)

wrapT :: FnT a b -> FnT a b
wrapT f = FnT ("(" <> getFnT f <> ")") 

wrap :: Fn -> Fn
wrap f = Fn $ "(" <> getFn f <> ")" 

-- | TODO: this should really be (m b) or even (m a) but I have no idea how to do this just yet
------- :: Fn  a b -> FnT a (m a)
liftFnT :: FnT a b -> FnT a b
liftFnT f = pure_ `chainFnT` f
  where pure_ = FnT "pure"

liftFn :: Fn -> Fn
liftFn f = pure_ `chainFn` f
  where pure_ = Fn "pure"

-- This is the `id` function                    
mkF1 :: FunctionName -> Fn
mkF1 fname = Fn . pack $ [istr| \a -> #{fname} a|] 

mkF2 :: FunctionName -> Fn
mkF2 fname = Fn . pack $ [istr| \(a, b) -> #{fname} a b|]

mkF3 :: FunctionName -> Fn
mkF3 fname = Fn . pack $ [istr| \(a, b, c) -> #{fname} a b c|]

mkF4 :: FunctionName -> Fn
mkF4 fname = Fn . pack $ [istr| \(a, b, c, d) -> #{fname} a b c d|]

mkF5 :: FunctionName -> Fn
mkF5 fname = Fn . pack $ [istr| \(a, b, c, d, e) -> #{fname} a b c d e|]

mkF6 :: FunctionName -> Fn
mkF6 fname = Fn . pack $ [istr| \(a, b, c, d, e, f) -> #{fname} a b c d e f|]

mkF7 :: FunctionName -> Fn
mkF7 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g) -> #{fname} a b c d e f g|]

mkF8 :: FunctionName -> Fn
mkF8 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h) -> #{fname} a b c d e f g h|]

mkF9 :: FunctionName -> Fn
mkF9 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i) -> #{fname} a b c d e f g h i|]

mkF10 :: FunctionName -> Fn
mkF10 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j) -> #{fname} a b c d e f g h i j|]

mkF11 :: FunctionName -> Fn
mkF11 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k) -> #{fname} a b c d e f g h i j k|]

mkF12 :: FunctionName -> Fn
mkF12 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l) -> #{fname} a b c d e f g h i j k l|]

mkF13 :: FunctionName -> Fn
mkF13 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m) -> #{fname} a b c d e f g h i j k l m|]

mkF14 :: FunctionName -> Fn
mkF14 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> #{fname} a b c d e f g h i j k l m n|]

mkF15 :: FunctionName -> Fn
mkF15 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> #{fname} a b c d e f g h i j k l m n o|]

mkF16 :: FunctionName -> Fn
mkF16 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> #{fname} a b c d e f g h i j k l m n o p|]

mkF17 :: FunctionName -> Fn
mkF17 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> #{fname} a b c d e f g h i j k l m n o p q|]

mkF18 :: FunctionName -> Fn
mkF18 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> #{fname} a b c d e f g h i j k l m n o p q r|]

mkF19 :: FunctionName -> Fn
mkF19 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> #{fname} a b c d e f g h i j k l m n o p q r s|]

mkF20 :: FunctionName -> Fn
mkF20 fname = Fn . pack $ [istr| \(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> #{fname} a b c d e f g h i j k l m n o p q r s t|]



arity1 :: T a -> T a
arity1 = id

arity2 :: T (a, b) -> (T a, T b)
arity2 _ = (type_, type_)

arity3 :: T (a, b, c) -> (T a, T b, T c)
arity3 _ = (type_, type_, type_)

arity4 :: T (a, b, c, d) -> (T a, T b, T c, T d)
arity4 _ = (type_, type_, type_, type_)

arity5 :: T (a, b, c, d, e) -> (T a, T b, T c, T d, T e)
arity5 _ = (type_, type_, type_, type_, type_)

arity6 :: T (a, b, c, d, e, f) -> (T a, T b, T c, T d, T e, T f)
arity6 _ = (type_, type_, type_, type_, type_, type_)

arity7 :: T (a, b, c, d, e, f, g) -> (T a, T b, T c, T d, T e, T f, T g)
arity7 _ = (type_, type_, type_, type_, type_, type_, type_)

arity8 :: T (a, b, c, d, e, f, g, h) -> (T a, T b, T c, T d, T e, T f, T g, T h)
arity8 _ = (type_, type_, type_, type_, type_, type_, type_, type_)

arity9 :: T (a, b, c, d, e, f, g, h, i) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i)
arity9 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity10 :: T (a, b, c, d, e, f, g, h, i, j) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j)
arity10 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity11 :: T (a, b, c, d, e, f, g, h, i, j, k) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k)
arity11 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity12 :: T (a, b, c, d, e, f, g, h, i, j, k, l) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l)
arity12 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity13 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m)
arity13 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity14 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n)
arity14 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity15 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o)
arity15 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity16 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p)
arity16 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity17 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p, T q)
arity17 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity18 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p, T q, T r)
arity18 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity19 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p, T q, T r, T s)
arity19 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity20 :: T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> (T a, T b, T c, T d, T e, T f, T g, T h, T i, T j, T k, T l, T m, T n, T o, T p, T q, T r, T s, T t)
arity20 _ = (type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_, type_)

arity1Str :: Typeable a => T a -> String
arity1Str = show . typeRep

arity2Str :: (Typeable a, Typeable b) => T (a, b) -> (String, String)
arity2Str x =
    let (a, b) = arity2 x
    in (show (typeRep a), show (typeRep b))

arity3Str :: (Typeable a, Typeable b, Typeable c) => T (a, b, c) -> (String, String, String)
arity3Str x =
    let (a, b, c) = arity3 x
    in (show (typeRep a), show (typeRep b), show (typeRep c))

arity4Str :: (Typeable a, Typeable b, Typeable c, Typeable d) => T (a, b, c, d) -> (String, String, String, String)
arity4Str x =
    let (a, b, c, d) = arity4 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d))

arity5Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => T (a, b, c, d, e) -> (String, String, String, String, String)
arity5Str x =
    let (a, b, c, d, e) = arity5 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e))

arity6Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f) => T (a, b, c, d, e, f) -> (String, String, String, String, String, String)
arity6Str x =
    let (a, b, c, d, e, f) = arity6 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f))

arity7Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g) => T (a, b, c, d, e, f, g) -> (String, String, String, String, String, String, String)
arity7Str x =
    let (a, b, c, d, e, f, g) = arity7 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g))

arity8Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h) => T (a, b, c, d, e, f, g, h) -> (String, String, String, String, String, String, String, String)
arity8Str x =
    let (a, b, c, d, e, f, g, h) = arity8 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h))

arity9Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i) => T (a, b, c, d, e, f, g, h, i) -> (String, String, String, String, String, String, String, String, String)
arity9Str x =
    let (a, b, c, d, e, f, g, h, i') = arity9 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'))

arity10Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j) => T (a, b, c, d, e, f, g, h, i, j) -> (String, String, String, String, String, String, String, String, String, String)
arity10Str x =
    let (a, b, c, d, e, f, g, h, i', j) = arity10 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j))

arity11Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k) => T (a, b, c, d, e, f, g, h, i, j, k) -> (String, String, String, String, String, String, String, String, String, String, String)
arity11Str x =
    let (a, b, c, d, e, f, g, h, i', j, k) = arity11 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k))

arity12Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l) => T (a, b, c, d, e, f, g, h, i, j, k, l) -> (String, String, String, String, String, String, String, String, String, String, String, String)
arity12Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l) = arity12 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l))

arity13Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m) => T (a, b, c, d, e, f, g, h, i, j, k, l, m) -> (String, String, String, String, String, String, String, String, String, String, String, String, String)
arity13Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m) = arity13 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m))

arity14Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity14Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n) = arity14 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n))

arity15Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity15Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o) = arity15 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o))

arity16Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity16Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p) = arity16 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p))

arity17Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p, Typeable q) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity17Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p, q) = arity17 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p), show (typeRep q))

arity18Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p, Typeable q, Typeable r) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity18Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p, q, r) = arity18 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p), show (typeRep q), show (typeRep r))

arity19Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p, Typeable q, Typeable r, Typeable s) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity19Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p, q, r, s) = arity19 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p), show (typeRep q), show (typeRep r), show (typeRep s))

arity20Str :: (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable h, Typeable i, Typeable j, Typeable k, Typeable l, Typeable m, Typeable n, Typeable o, Typeable p, Typeable q, Typeable r, Typeable s, Typeable t) => T (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) -> (String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String)
arity20Str x =
    let (a, b, c, d, e, f, g, h, i', j, k, l, m, n, o, p, q, r, s, t) = arity20 x
    in (show (typeRep a), show (typeRep b), show (typeRep c), show (typeRep d), show (typeRep e), show (typeRep f), show (typeRep g), show (typeRep h), show (typeRep i'), show (typeRep j), show (typeRep k), show (typeRep l), show (typeRep m), show (typeRep n), show (typeRep o), show (typeRep p), show (typeRep q), show (typeRep r), show (typeRep s), show (typeRep t))

-- runPure1 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure1 = genMakeMainComparativeTestScript liftPure1

-- runPure2 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure2 = genMakeMainComparativeTestScript liftPure2

-- runPure3 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure3 = genMakeMainComparativeTestScript liftPure3

-- runPure4 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure4 = genMakeMainComparativeTestScript liftPure4

-- runPure5 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure5 = genMakeMainComparativeTestScript liftPure5

-- runPure6 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure6 = genMakeMainComparativeTestScript liftPure6

-- runPure7 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure7 = genMakeMainComparativeTestScript liftPure7

-- runPure8 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure8 = genMakeMainComparativeTestScript liftPure8

-- runPure9 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure9 = genMakeMainComparativeTestScript liftPure9

-- runPure10 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure10 = genMakeMainComparativeTestScript liftPure10

-- runPure11 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure11 = genMakeMainComparativeTestScript liftPure11

-- runPure12 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure12 = genMakeMainComparativeTestScript liftPure12

-- runPure13 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure13 = genMakeMainComparativeTestScript liftPure13

-- runPure14 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure14 = genMakeMainComparativeTestScript liftPure14

-- runPure15 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure15 = genMakeMainComparativeTestScript liftPure15

-- runPure16 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure16 = genMakeMainComparativeTestScript liftPure16

-- runPure17 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure17 = genMakeMainComparativeTestScript liftPure17

-- runPure18 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure18 = genMakeMainComparativeTestScript liftPure18

-- runPure19 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure19 = genMakeMainComparativeTestScript liftPure19

-- runPure20 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runPure20 = genMakeMainComparativeTestScript liftPure20

-- runMonadic1 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic1 = genMakeMainComparativeTestScript mkF1

-- runMonadic2 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic2 = genMakeMainComparativeTestScript mkF2

-- runMonadic3 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic3 = genMakeMainComparativeTestScript mkF3

-- runMonadic4 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic4 = genMakeMainComparativeTestScript mkF4

-- runMonadic5 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic5 = genMakeMainComparativeTestScript mkF5

-- runMonadic6 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic6 = genMakeMainComparativeTestScript mkF6

-- runMonadic7 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic7 = genMakeMainComparativeTestScript mkF7

-- runMonadic8 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic8 = genMakeMainComparativeTestScript mkF8

-- runMonadic9 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic9 = genMakeMainComparativeTestScript mkF9

-- runMonadic10 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic10 = genMakeMainComparativeTestScript mkF10

-- runMonadic11 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic11 = genMakeMainComparativeTestScript mkF11

-- runMonadic12 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic12 = genMakeMainComparativeTestScript mkF12

-- runMonadic13 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic13 = genMakeMainComparativeTestScript mkF13

-- runMonadic14 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic14 = genMakeMainComparativeTestScript mkF14

-- runMonadic15 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic15 = genMakeMainComparativeTestScript mkF15

-- runMonadic16 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic16 = genMakeMainComparativeTestScript mkF16

-- runMonadic17 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic17 = genMakeMainComparativeTestScript mkF17

-- runMonadic18 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic18 = genMakeMainComparativeTestScript mkF18

-- runMonadic19 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic19 = genMakeMainComparativeTestScript mkF19

-- runMonadic20 :: FunctionName -> VarName -> Import -> Import -> TestScript
-- runMonadic20 = genMakeMainComparativeTestScript mkF20
  --where
    -- In theory, we could do something like this to typecheck a singular expression, before we include it in the script gen
    --mkExpression :: (a -> IO b) -> IO b
    --mkExpression f = $([e| 1 + 1 + g|] )



type EitherDesign = Either
script
  :: forall a
  .  Typeable a
  => LocatedUserModule -- The user's solution, to make 
  -> LocatedUserModule -- Our Correct Solution, to make an import
  -> EitherDesign [[a]] [a]
  -- We could choose the design of this being a list of type names OR just the tests themselves
  -- which would be all we need to get the type info
script = undefined   
-- script fname = [istr|
-- main = do
--   inputs <- makeTest #{test11Inputs} 

--   valsUser <- mapM User.#{fname} inputs
--   valsOurSolution <- mapM Solution.#{fname} inputs

--   print $ zipWith (==) valsUser valsOurSolution

-- |]


data Generator
  = StaticConstant T.Text
  | Unfoldr  

newtype Tests' = Tests' T.Text
newtype Filter = Filter T.Text
newtype Count = Count Int

makeTests :: Count -> Filter -> Generator -> Tests'
makeTests (Count ct) (Filter filt) generator = Tests' $ 
  "Prelude.take " <> (T.pack $ show ct)
  <> " $ "
  <> " filter "
  <> filt
  <> " "
  <> renderGenerator generator

renderGenerator :: Generator -> T.Text
renderGenerator = \case
  StaticConstant t -> t
  Unfoldr -> "[]"

  
instance ToJSON ExitCode
instance FromJSON ExitCode
