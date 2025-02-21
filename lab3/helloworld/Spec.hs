import Test.Hspec

-- Functions to test
mhead1 :: [a] -> a
mhead1 a = a!!0

mhead2 :: [a] -> a
mhead2 (x:_) = x

mhead3 :: [a] -> a
mhead3 a = last $ reverse a

listToValue :: [a] -> a
listToValue [x] = x

mhead4 :: [a] -> a
mhead4 x = listToValue (take 1 x)

mhead5 :: [a] -> a
mhead5 = head

main :: IO ()
main = hspec $ do
  describe "mhead1" $ do
    it "returns the first element of a list" $
      mhead1 [1,2,3] `shouldBe` 1

  describe "mhead2" $ do
    it "returns the first element of a list" $
      mhead2 [1,2,3] `shouldBe` 1

  describe "mhead3" $ do
    it "returns the first element of a list" $
      mhead3 [1,2,3] `shouldBe` 1

  describe "mhead4" $ do
    it "returns the first element of a list" $
      mhead4 [1,2,3] `shouldBe` 1

  describe "mhead5" $ do
    it "returns the first element of a list" $
      mhead5 [1,2,3] `shouldBe` 1
