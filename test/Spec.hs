import Test.Hspec
import Wolf3D.HeroSpec
import Wolf3D.RunnerSpec
import Wolf3D.DisplaySpec

main :: IO ()
main = hspec $ do
  heroSpec
  runnerSpec
  displaySpec
