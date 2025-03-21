import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "app/Main.hs"]
