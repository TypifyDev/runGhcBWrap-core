{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module RunGhc.Locate where

import Data.Aeson
import GHC.Generics
import System.FilePath
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.List.Split as List
import qualified Data.Text as T

toSimpleImport :: NonEmpty PathSegment -> Import
toSimpleImport = Import Nothing

toQualifiedImport :: T.Text -> NonEmpty PathSegment -> Import
toQualifiedImport qname segs = Import (Just qname) segs

newtype PathSegment = PathSegment { getPathSegment :: T.Text }
  deriving (Show, Generic)
instance ToJSON PathSegment
instance FromJSON PathSegment
data Import = Import
  { _import_qualifiedName :: Maybe T.Text
  , _import_pathSeg :: NonEmpty PathSegment
  } deriving (Show, Generic)
instance ToJSON Import
instance FromJSON Import

newtype Imports = Imports { getImports :: [Import] } deriving (Show, Generic)
instance FromJSON Imports
instance ToJSON Imports
instance Semigroup Imports where
  Imports a <> Imports b = Imports $ a <> b
instance Monoid Imports where
  mempty = Imports mempty

type FileExt = T.Text
pathSegsToPath :: FileExt -> NonEmpty PathSegment -> FilePath
pathSegsToPath fileExt (PathSegment p :| [])       = T.unpack $ p <> fileExt
pathSegsToPath fE      (PathSegment p :| (q : qs)) = T.unpack p </> pathSegsToPath fE (q :| qs)

pathSegsFromFilePath :: FilePath -> NonEmpty PathSegment
pathSegsFromFilePath = NE.fromList . fmap (PathSegment . T.pack) . List.splitOn "/"

pathSegsToModuleName :: NonEmpty PathSegment -> T.Text
pathSegsToModuleName (PathSegment p :| [])       = p
pathSegsToModuleName (PathSegment p :| (q : qs)) = p <> "." <> pathSegsToModuleName (q :| qs)

-- HeadlessScript ==> LocatedModule
makeSourcePath :: FilePath -> NonEmpty PathSegment -> FilePath
makeSourcePath baseDir segs =
  let segs_ = T.intercalate "/" $ NE.toList $ NE.map getPathSegment segs
  in
    baseDir </> (T.unpack segs_)

showImportLine :: Import -> T.Text
showImportLine imp = case _import_qualifiedName imp of
  Nothing ->
    "import " <> pathSegsToModuleName (_import_pathSeg imp) <> ""
  Just qName ->
    "import qualified "
    <> pathSegsToModuleName (_import_pathSeg imp)
    <> " as "
    <> qName
    <> ""
