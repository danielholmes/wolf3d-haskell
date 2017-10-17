import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Wolf3D" $ do
        it "should work placeholder test" $
            "hello" `shouldBe` "hello"
