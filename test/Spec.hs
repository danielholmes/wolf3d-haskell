import Test.Hspec
import Wolf3D.HeroSpec
import Wolf3D.RunnerSpec
import Wolf3D.DisplaySpec
import Wolf3D.GeomSpec

main :: IO ()
main = hspec $ do
  heroSpec
  runnerSpec
  displaySpec
  geomSpec
