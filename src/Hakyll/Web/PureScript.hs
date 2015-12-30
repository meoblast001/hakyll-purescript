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

-- |Compiles a PureScript file into a JavaScript file containing all of the
-- modules.
pureScriptCompiler :: Compiler (Item String)
pureScriptCompiler = getResourceBody >>= renderPureScript

-- |Compiles a PureScript file into a JavaScript file containing all of the
-- modules. Compiler options can be given.
pureScriptCompilerWith :: Options -> Compiler (Item String)
pureScriptCompilerWith options =
  getResourceBody >>= renderPureScriptWith options

-- |Compiles a PureScript file item into a JavaScript file containing all of the
-- modules.
renderPureScript :: Item String -> Compiler (Item String)
renderPureScript = renderPureScriptWith defaultOptions

-- |Compiles a PureScript file item into a JavaScript file containing all of the
-- modules. Compiler options can be given.
renderPureScriptWith :: Options -> Item String -> Compiler (Item String)
renderPureScriptWith options item = do
  result <- unsafeCompiler $ ioCompiler options $ itemFilenameAndContents item
  makeItem result

-- Get the file path and contents of an item (PureScript file).
itemFilenameAndContents :: Item String -> (FilePath, String)
itemFilenameAndContents item = (toFilePath $ itemIdentifier item, itemBody item)

-- |Compiles PureScript file contents with specified options and returns the
-- JavaScript. If multiple modules exist in the PureScript file, all of their
-- compiled results appear in the returned string.
ioCompiler :: Options -> (FilePath, String) -> IO String
ioCompiler options filenameAndContents = do
  -- Specify the working directory under the temporary directory.
  tmpDir <- getTemporaryDirectory
  let tmpWorkDir = tmpDir ++ "/purescript-hakyll"
  -- Try to parse the modules from the PureScript file.
  case parseModulesFromFiles id [filenameAndContents] of
    Left errors -> fail $ prettyPrintMultipleErrors True errors
    Right filesAndModules -> do
      let modules = map snd filesAndModules
          modNamesToFiles =
            Map.fromList (map (\(file, mod) -> (getModuleName mod, Right file))
                              filesAndModules)
          -- Build compilation actions compiling the modules without foreign
          -- imports.
          makeActions = buildMakeActions tmpWorkDir modNamesToFiles Map.empty
                                         False
      -- Compile the PureScript to JavaScript.
      (successOrErrors, _) <- runMake options $ make makeActions modules
      case successOrErrors of
        Left errors -> fail $ prettyPrintMultipleErrors True errors
        Right _ -> do
          -- Load the module contents for all output modules.
          moduleContents <- mapM (loadModuleContents tmpWorkDir) modules
          -- Concatenate all results.
          let allModuleContents = concat moduleContents
          return allModuleContents

-- |Use the path of the temporary working directory and the module and return
-- the JavaScript contents.
loadModuleContents :: FilePath -> Module -> IO String
loadModuleContents workingDir mod =
  readFile (workingDir ++ "/" ++ runModuleName (getModuleName mod) ++
            "/index.js")
