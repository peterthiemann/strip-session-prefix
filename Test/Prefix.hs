module Prefix (spec) where

import BSession.Parse
import BSession.Prefix
import Test.Hspec

spec :: Spec
spec = do
  it "recognizes basic linear prefixes" do
    (readSession "!A.?B.end" `stripPrefix` readSession "!A.ret")
      `shouldReturn` readSession "?B.end"
