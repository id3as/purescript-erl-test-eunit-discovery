module Erl.Tests.EUnit.Discovery where

import Prelude

import Control.Monad.Free (Free, hoistFree)
import Data.Filterable (filter, filterMap)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Erl.Data.List (List, concat)
import Erl.ModuleName (ModuleName(..), NativeModuleName, nativeModuleName)
import Erl.Test.EUnit (Group(..), TestF(..), TestSet, collectTests)

foreign import findModuleNames :: String -> Effect (List ModuleName)
foreign import getExportedTests :: NativeModuleName -> Effect (Maybe (Free TestF Unit))
foreign import filterTests :: (String -> Boolean) -> List TestSet -> List TestSet

type FindOptions
  = { moduleFilter :: ModuleName -> Boolean
    , testFilter :: String -> Boolean
    }

defaultOptions :: FindOptions
defaultOptions =
  { moduleFilter: (\_ -> true)
  , testFilter: (\_ -> true)
  }

findTests :: String -> FindOptions -> Effect (List TestSet)
findTests dir { moduleFilter, testFilter } = do
  findModuleNames dir
    <#> filter moduleFilter
    >>= traverse (\(m :: ModuleName) -> map (annotateModule m) <$> getExportedTests (nativeModuleName m))
    <#> filterMap identity
    <#> map collectTests
    <#> concat
    <#> filterTests testFilter

annotateModule :: ModuleName -> Free TestF Unit -> Free TestF Unit
annotateModule (ModuleName mn) = hoistFree f
  where 
  f :: forall a. TestF a -> TestF a
  f (TestGroup (Group n s) a) = (TestGroup (Group (n <> " (" <> mn <> ")") s) a)
  f other = other