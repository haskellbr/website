module Haskellbr.WebsiteSpec (spec) where

import Haskellbr.Website

import Test.Hspec

spec :: Spec
spec =
    describe "main" $ do
        it "returns the unit" $
            main `shouldReturn` ()
