-- |
-- Module: Hakyll.Web.PureScript
-- Copyright: (C) 2015 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Hakyll.Web.PureScript
( Options(..)
, pureScriptCompiler
, pureScriptCompilerWith
, renderPureScript
, renderPureScriptWith
) where

import Control.Monad.Writer.Strict
import Data.Functor
import qualified Data.Map as Map
import Hakyll.Core.Compiler
import Hakyll.Core.Identifier
import Hakyll.Core.Item
import Language.PureScript.AST.Declarations
import Language.PureScript.Errors
import Language.PureScript.Make
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Parser.Declarations
import System.Directory
import System.FilePath
import System.IO

pureScriptCompiler :: Compiler (Item String)
pureScriptCompiler = getResourceBody >>= renderPureScript

pureScriptCompilerWith :: Options -> Compiler (Item String)
pureScriptCompilerWith options =
  getResourceBody >>= renderPureScriptWith options

renderPureScript :: Item String -> Compiler (Item String)
renderPureScript = renderPureScriptWith defaultOptions

renderPureScriptWith :: Options -> Item String -> Compiler (Item String)
renderPureScriptWith options item = do
  result <- unsafeCompiler $ ioCompiler options $ itemFilenameAndContents item
  makeItem result

itemFilenameAndContents :: Item String -> (FilePath, String)
itemFilenameAndContents item = (toFilePath $ itemIdentifier item, itemBody item)

ioCompiler :: Options -> (FilePath, String) -> IO String
ioCompiler options filenameAndContents = do
  tmpDir <- getTemporaryDirectory
  let tmpWorkDir = tmpDir ++ "/purescript-hakyll"
  case parseModulesFromFiles id [filenameAndContents] of
    Left errors -> fail $ prettyPrintMultipleErrors True errors
    Right filesAndModules -> do
      let modules = map snd filesAndModules
          modNamesToFiles =
            Map.fromList (map (\(file, mod) -> (getModuleName mod, Right file))
                              filesAndModules)
          makeActions = buildMakeActions tmpWorkDir modNamesToFiles Map.empty
                                         False
      (successOrErrors, _) <- runMake options $ make makeActions modules
      case successOrErrors of
        Left errors -> fail $ prettyPrintMultipleErrors True errors
        Right _ -> do
          moduleContents <- mapM (loadModuleContents tmpWorkDir) modules
          let allModuleContents = concat moduleContents
          return allModuleContents

loadModuleContents :: FilePath -> Module -> IO String
loadModuleContents workingDir mod =
  readFile (workingDir ++ "/" ++ runModuleName (getModuleName mod) ++
            "/index.js")
